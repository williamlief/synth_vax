# Defines and runs the multiverse analysis. Note that this script takes
# substantial processing time to run. On a 2015 macbook pro it took approximately
# 14 hours.

library(here)
library(tidyverse)
library(furrr)
library(augsynth)
library(lubridate)

annual_cov <- read_csv(here("python/annual_dataset_processed.csv"))
daily_cov <- read_csv(here("python/daily_dataset_processed.csv"))
vax_case <- readRDS(here("data/daily_data_2021-09-12.rds"))

analysisdat <- vax_case %>%
  select(state, fips, day, date, people_fully_vaccinated_per_hundred, 
         people_vaccinated_per_hundred, total_vaccinations_per_hundred) %>% 
  tidylog::left_join(annual_cov %>% select(-state_abb), 
                     by = "fips") %>% 
  tidylog::left_join(daily_cov %>% select(-state_abb), 
                     by = c("fips", "date")) 
# NOTE: There are some positional references to variables in this script!
# Be very careful when modifying the input data to ensure that nothing breaks

announce_dates <- read_csv("data-raw/lottery_announce_dates.csv") %>% 
  mutate(state = str_trim(state))

# Ohio announce date 
announce_date <- announce_dates %>% filter(state == "OH") %>% 
  pull(lottery_announce_date) %>% mdy(.)


# Data parameters ---------------------------------------------------
# parameters for data processing: donor states, time window, covariates, outcome variable

data_params <- list(
  
  states_to_include = list(
    full = sort(unique(analysisdat$state)), 
    no_lottery = announce_dates %>% 
      filter(is.na(lottery_announce_date) | state == "OH") %>% 
      pull(state)),
  
  pretreat_start = list(
    full = "2021-01-12",
    bw_start = "2021-02-19" # B&W start - CDC data
  ),
  
  post_stop = list(
    lottery_end = "2021-06-24", # lottery end
    lottery_4w = "2021-07-23", # 4 weeks after lottery end per Moderna regimen
    lottery_6w = "2021-08-22" # Last day before Full FDA approval
  ),
  
  # Modelling Parameters
  covariates = list(
    none = "NULL",
    annual_demo = names(annual_cov[c(3, 5:6, 8:20)]), # POSITIONAL REFERENCES
    annual_mobility = c(names(annual_cov[c(3, 5:6, 8:20)]), names(daily_cov)[4:9])
  ),
  
  outcome = list(
    full_vax = "people_fully_vaccinated_per_hundred", 
    first_dose = "people_vaccinated_per_hundred", 
    total_doses = "total_vaccinations_per_hundred"
  )
)

data_spec <- 
  crossing(
    states_to_include = data_params$states_to_include,
    pretreat_start = data_params$pretreat_start, 
    post_stop = data_params$post_stop, 
    covariates = data_params$covariates, 
    outcome = data_params$outcome) 


# Modelling parameters ----------------------------------------------------

method = list( 
  augsynth = "augsynth"
)

# augsynth
augsynth_opts <- list(
  progfunc = list(
    ridge = "ridge",
    none  = "none"
  ),
  fixedeff = list(
    true = TRUE,
    false = FALSE
  )
)

model_spec <-
  crossing(method = method, 
           as_progfunc = augsynth_opts$progfunc, 
           as_fixedeff = augsynth_opts$fixedeff) %>% 
  # No progfunc and no fixedeff is the same as tidysynth
  filter((as_progfunc == "none" & as_fixedeff == FALSE) |
           as_progfunc == "ridge" & as_fixedeff == TRUE) 

multiverse_spec <- 
  crossing(model_spec, data_spec) %>% 
  mutate(model_num = as.character(row_number())) %>% 
  as.data.frame() 

# define function for multiverse of models ------------------------------------

multiverse_run <- function(model_num, 
                           method, 
                           as_progfunc, 
                           as_fixedeff,
                           states_to_include,
                           pretreat_start, 
                           post_stop, 
                           covariates, 
                           outcome, 
                           data, 
                           output = "output-multiverse",
                           verbose = F) {
  
  if(verbose) {
    print(paste("model_num:", model_num))
    print(paste("method:", method))
    print(paste("method_opts:", as_progfunc, as_fixedeff))
    print(paste("states_to_include, count:", length(states_to_include)))
    print(paste("pretreat_start:", pretreat_start))
    print(paste("post_stop:", post_stop))
    print(paste("covariates:", covariates))
    print(paste("outcome:", outcome))
    print("--------------------------")
  }
  
  # Treatment window and data collapse to week
  data <- data %>% filter(
    day >= pretreat_start, 
    day <= post_stop,
    state %in% states_to_include
  ) %>% 
    # note: this was much cleaner w/o aggregation inside the loop, but because of 
    # the day cutoffs it is more accurate to do it here
    mutate(week = isoweek(day)) %>% 
    group_by(state, week) %>% 
    summarize(across(c(people_vaccinated_per_hundred, 
                       total_vaccinations_per_hundred,
                       people_fully_vaccinated_per_hundred), 
                     max, na.rm = T),
              across(any_of(covariates), mean, na.rm = T),
              .groups = "drop") %>% 
    mutate(centered_week = week - isoweek(announce_date),
           treat = state == "OH" & centered_week >= 0)
  
  data$outcome = data[[outcome]]
  
  if(method == "augsynth") {
    # Covariate inclusion 
    formula = paste0(paste0(outcome, collapse = "+"), " ~ treat")
    if(covariates[[1]] != "NULL") {
      formula = paste(formula, paste0(covariates, collapse = "+"), sep = "|")
    }
    
    # Permute the augsynth
    names(states_to_include) <- states_to_include
    wrap_augsynth <- function(treated_state) {
      data <- data %>% 
        mutate(treat = state == treated_state & centered_week >= 0)
      
      augsynth(as.formula(formula), 
               # Fixed params          
               unit = state, time = centered_week, data = data,
               t_int = 0, 
               # variable params
               progfunc = as_progfunc, fixedeff = as_fixedeff,
               scm = T)
    }
    augsynth_out <- states_to_include %>% 
      map(quietly(wrap_augsynth))
    
    # augsynth_out structured as: 
    # unit_name, type, pre_mspe, post_mspe, mspe_ratio, mspe_rank, 
    # fischers_exact_pvalue, z_score, average_difference, average_rank, 
    # last_period_diff, last_period_rank, weight
    
    # only care about weights for the OH model
    weights <- augsynth_out[["OH"]]$result$weights %>% 
      as.data.frame(.) %>% 
      rownames_to_column(var = "unit_name") %>% 
      rename(weight = V1)
    
    as_sum <- augsynth_out %>%
      map("result") %>% 
      map(summary)
    
    get_augsynth_metrics <- function(as_sum) {
      
      pre_mspe <- as_sum$att %>% 
        filter(Time < 0) %>% 
        summarize(pre_mspe = mean(Estimate^2))
      
      post_mspe <- as_sum$att %>% 
        filter(Time >= 0) %>% 
        summarize(post_mspe = mean(Estimate^2))
      
      average_diff = as_sum$att %>% 
        filter(Time >= 0) %>% 
        summarize(average_diff = mean(Estimate))
      
      last_period_diff = as_sum$att %>% 
        filter(Time == max(Time)) %>% 
        select(last_period_diff = Estimate)
      
      cbind(pre_mspe, post_mspe, average_diff, last_period_diff)
    }
    
    as_results <- as_sum %>% 
      map_dfr(get_augsynth_metrics, .id = "unit_name") 
    
    as_results <- as_results %>% 
      mutate(mspe_ratio = post_mspe/pre_mspe,
             mspe_rank = rank(-mspe_ratio), 
             average_rank = rank(average_diff), 
             last_period_rank = rank(last_period_diff), 
             type = ifelse(unit_name == "OH", "Treated", "Donor"), 
             fishers_exact_pvalue = mspe_rank / n()) %>% 
      left_join(weights, by = "unit_name") %>% 
      mutate(model_num = model_num)
    
    # write the full augsynth output to disk
    saveRDS(augsynth_out, paste0(output, "/model_", model_num, ".rds"))
    # function returns selected metrics in memory
    return(as_results)
  }
}


# Run Multiverse Models ---------------------------------------------------

# use furrr package to leverage multiple cores, may need to
# adjust for your machine
plan(multisession, workers = availableCores())
set.seed(428438)

multiverse_output <- multiverse_spec %>%
  future_pmap(., possibly(multiverse_run, otherwise = "error"), data = analysisdat,
              .options = furrr_options(seed = TRUE),
              .progress = T)

names(multiverse_output) <- multiverse_spec$model_num

# Note on output: weight column captures each state's weight in the OH (true)
# synthetic control model. All other columns hold the specific states output from
# the permutation test
saveRDS(multiverse_output, "output-multiverse/multiverse_output.RDS")

saveRDS(multiverse_spec, "output-multiverse/multiverse_spec.RDS")