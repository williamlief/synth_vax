library(MASS) # must be loaded before tidyverse - namespace collision on select
library(tidyverse)
library(here)
library(stargazer)
library(foreach)
library(augsynth)
library(tidysynth)


dat <- readRDS(here("data/weekly_data_2021-05-17.rds"))  

excluded_states <- read_csv(here("data-raw/lottery_announce_dates.csv")) %>% 
  mutate(state = str_trim(state)) %>% 
  filter(!is.na(lottery_announce_date) & state != "OH") %>% 
  pull(state)

dat <- dat %>% filter(!state %in% excluded_states)

# augsynth wrapper funs  -------------------------------------------------------

placebo_test <- function(s) { # note: this is called permutation test in other files
  # Takes in a state abbreviation  and runs an augsynth estimate with that state

  placebo.panel <- dat %>% 
    mutate(placebo_post = if_else(state == s & centered_week >= 0, 1L, 0L))
  
  placebo.est <- augsynth(people_fully_vaccinated_per_hundred ~ placebo_post,
    unit = state, time = centered_week,
    data = placebo.panel, progfun = "None", fixedeff = FALSE
  )
  cbind(summary(placebo.est)$att, state = s)
}

placebo_test_weights <- function(s) {
  # Takes in a state abbreviation  and runs an augsynth estimate with that state

  placebo.panel <- dat %>% 
    mutate(placebo_post = if_else(state == s & centered_week >= 0, 1L, 0L))
  placebo.est <- augsynth(people_fully_vaccinated_per_hundred ~ placebo_post,
    unit = state, time = centered_week,
    data = placebo.panel, progfun = "None", fixedeff = FALSE
  )
  cbind(summary(placebo.est)$att, state = s)
  
  mat<-cbind(placebo.est$weights,"OH",row.names(placebo.est))# %>% as.data.frame(donor_state=rownames(.)) %>% as_tibble()
  mat[,1] %>% enframe() %>% mutate(synthetic_state=s)
}

# Estimate growth rates ---------------------------------------------------

growth_mat <- dat %>% select(people_fully_vaccinated_per_hundred,
               state,
               centered_week) %>% 
  group_by(state) %>%
  mutate(growth_rate = people_fully_vaccinated_per_hundred - 
           lag(people_fully_vaccinated_per_hundred)) %>%
  select(-people_fully_vaccinated_per_hundred) %>%
  pivot_wider(id_cols = "centered_week",names_from = state,values_from = growth_rate) 

mu <- growth_mat %>%
  select(-centered_week) %>%
  colMeans(na.rm = TRUE)

cov_matrix <- growth_mat %>%
  select(-centered_week) %>%
  drop_na() %>%
  cov()

# iterate over non-treated states -----------------------------------------

placebo_results <- names(mu) %>% 
  map_dfr(placebo_test) %>% 
  select(Time, Estimate, state)

weights <- names(mu) %>%
  map_dfr(placebo_test_weights) %>% 
  rename(donor_weight = value, donor_state = name)

# Simulate differences in vax rates ---------------------------------------

run_mv_sim_last_period<-function(i, effect_size){
  
  mu_treatment <- mu # global
  mu_treatment["OH"] <- mu["OH"] + effect_size
  sim_growth <- MASS::mvrnorm(7, mu_treatment, cov_matrix) %>% 
    as_tibble() %>% 
    mutate_all(cumsum) %>%
    mutate(centered_week=row_number() - 1) %>% 
    pivot_longer(-"centered_week",
                 names_to ="state", 
                 values_to = "people_fully_vaccinated_per_hundred")
  
  growth_dat <- dat %>% 
    select(state, centered_week, people_fully_vaccinated_per_hundred) %>% 
    filter(centered_week < 0) %>% 
    bind_rows(sim_growth)
  
  with_composite_weights<- growth_dat %>%
    left_join(weights, by = c("state"="synthetic_state"))
  
  donor_and_synthetic_values <- with_composite_weights %>% 
    left_join(growth_dat %>% 
                rename(donor_value = people_fully_vaccinated_per_hundred), 
              by = c("centered_week","donor_state"="state"))
  
  donor_and_synthetic_values %>%
    filter(centered_week==6) %>%
    group_by(state) %>%
    summarise(truth = mean(people_fully_vaccinated_per_hundred), 
              estimate = weighted.mean(donor_value, as.numeric(donor_weight))) %>%
    mutate(run = i, es = effect_size)
}

# Define effect estimate - this is our primary result
pr_effect_estimate = -1.3

set.seed(12345)
ef_null <- (1:1000) %>% map_dfr(run_mv_sim_last_period, effect_size = 0) 
set.seed(12345)
ef_b_and_w <- (1:1000) %>% map_dfr(run_mv_sim_last_period, effect_size = .7/7 ) 
set.seed(12345)
ef_pre_registered <- (1:1000) %>% map_dfr(run_mv_sim_last_period, effect_size = pr_effect_estimate / 7) 


# Estimate bayes factor  --------------------------------------------------

scenarios <- bind_rows(
  ef_null %>% 
    mutate(difference = truth - estimate) %>% 
    filter(state == "OH") %>% 
    summarise(prob = mean(difference < pr_effect_estimate), 
              effect_size = mean(es),
              scenario = "Sharp null"),
  
  ef_b_and_w %>%
    mutate(difference = truth - estimate) %>% 
    filter(state == "OH") %>% 
    summarise(prob = mean(difference < pr_effect_estimate), 
              effect_size = mean(es),
              scenario = "Barber and West"),
  
  ef_pre_registered %>% 
    mutate(difference = truth - estimate) %>%
    filter(state == "OH") %>% 
    summarise(prob = mean(difference < pr_effect_estimate),
              effect_size = mean(es), 
              scenario = "Pre-registered")
)

pre_prob <- scenarios %>% 
  filter(scenario == "Pre-registered") %>% 
  pull(prob)

null_prob <- scenarios %>% 
  filter(scenario == "Sharp null") %>% 
  pull(prob) 

b_and_w_prob <- scenarios %>%
  filter(scenario == "Barber and West") %>% 
  pull(prob) 

pre_prob/null_prob # Sharp Null Prior

pre_prob/b_and_w_prob # B&W Prior

