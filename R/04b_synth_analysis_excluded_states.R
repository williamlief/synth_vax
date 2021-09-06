#This script reruns the synthetic analysis excluding states that adopted lotteries 
# after Ohio.
# This analysis is reported in the main body of the paper. 

library(here)
library(tidyverse)
library(tidysynth)
library(stargazer)
library(augsynth)


dat <- readRDS(here("data/weekly_data_2021-06-24.rds")) 

announce_dates <- read_csv("data-raw/lottery_announce_dates.csv") %>% 
  mutate(state = str_trim(state))

excluded_states <- announce_dates %>% 
  filter(!is.na(lottery_announce_date)) %>% 
  filter(state != "OH")
print(excluded_states$state)
# [1] "AR" "CA" "CO" "DE" "IL" "KY" "LA" "ME" "MD" "MA" "MI" "NV" "NM"
# [14] "NY" "NC" "OR" "WA" "WV" 

dat <- dat %>% 
  tidylog::anti_join(excluded_states)

# Train Synthetic Control Model
vaccine_out <-
  dat  %>%
  # initial the synthetic control object
  synthetic_control(outcome = people_fully_vaccinated_per_hundred, # outcome
                    unit = state, # unit index in the panel data
                    time = centered_week, # time index in the panel data
                    i_unit = "OH", # unit where the intervention occurred
                    i_time = 0, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  ) %>%
  # Matching on fully vaccinated the weeks before the intervention  
  generate_predictor(time_window = -17, lagged_vaccinations_week17 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -16, lagged_vaccinations_week16 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -15, lagged_vaccinations_week15 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -14, lagged_vaccinations_week14 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -13, lagged_vaccinations_week13 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -12, lagged_vaccinations_week12 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -11, lagged_vaccinations_week11 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -10, lagged_vaccinations_week10 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -09, lagged_vaccinations_week09 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -08, lagged_vaccinations_week08 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -07, lagged_vaccinations_week07 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -06, lagged_vaccinations_week06 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -05, lagged_vaccinations_week05 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -04, lagged_vaccinations_week04 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -03, lagged_vaccinations_week03 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -02, lagged_vaccinations_week02 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -01, lagged_vaccinations_week01 = people_fully_vaccinated_per_hundred) %>%
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = -17:-1, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  # Generate the synthetic control
  generate_control()

# Unit weights 
vaccine_out %>%
  grab_unit_weights() %>%
  mutate(weights = round(weight, digits = 4)) %>%
  select(unit, weights) %>%
  filter(weights>0.0001) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, rownames = FALSE)

vaccine_out %>% plot_weights() + 
  labs(title="Synthetic Control Weights")   
ggsave(here("figures/ex_lotto_weights.jpg"))

# Balance Table
balance_table <- vaccine_out %>%
  grab_balance_table() %>%
  mutate(difference = OH - synthetic_OH) %>%
  select(variable, OH, synthetic_OH, difference, donor_sample) %>%
  as.data.frame()

balance_table %>%
  stargazer(summary = FALSE, rownames = FALSE, 
            caption = "Balance Table", 
            label = "balancetable")

mean(abs(balance_table$difference))

# Plot Model Trends
vaccine_out %>% plot_trends() +
  scale_x_continuous(breaks = c(-15,-10,-5,0,5)) +
  labs(
    title = "Ohio and Synthetic Ohio",
    caption = "Timing of The Lottery Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Percent Fully Vaccinated"
  ) 
ggsave("figures/ex_lotto_treatment_trends.jpg")

vaccine_out %>% plot_differences() +
  scale_x_continuous(breaks = c(-15,-10,-5,0,5)) +
  labs(
    title = "Ohio and Synthetic Ohio",
    caption = "Timing of The Lottery Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Percent Fully Vaccinated"
  ) 
ggsave("figures/ex_lotto_treatment_differences.jpg")

# Main result values
mspe <- vaccine_out %>% 
  grab_signficance() %>% 
  rename(mspe_rank=rank) 

average_diff <- vaccine_out %>%
  unnest(.synthetic_control) %>%
  filter(time_unit >= 1) %>%
  group_by(.id) %>%
  summarise(average_difference = mean(real_y - synth_y)) %>%
  arrange(desc(average_difference)) %>%
  mutate(average_rank=row_number()) 

last_period_diff <- vaccine_out %>%
  unnest(.synthetic_control) %>%
  filter(time_unit == max(time_unit)) %>%
  group_by(.id) %>%
  summarise(last_period_diff = (real_y - synth_y)) %>%
  distinct() %>%
  ungroup() %>%
  arrange(desc(last_period_diff)) %>% 
  mutate(last_period_rank=row_number())

state_performance_metrics <- 
  mspe %>% 
  left_join(average_diff,by=c("unit_name"=".id")) %>% 
  left_join(last_period_diff,by=c("unit_name"=".id"))

state_performance_metrics %>% filter(unit_name=="OH")
# NOTE: latex table created by hand

# Permutation Test Plot
vaccine_out %>% plot_placebos() +
  labs(
    title = "Ohio and Synthetic Ohio",
    caption = "Timing of The Lottery Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Percent Fully Vaccinated"
  ) 
ggsave(here("figures/alt_pretreatment_synth.jpg"))


# Permutation Test and Conformal Inference --------------------------------
# This section was added per suggestions from reviewer 2
# It adds conformal inference based standard errors to the synthetic control analysis 
# We use the augsynth package to do so

dat <- dat %>% mutate(post_ohio = state == "OH" & centered_week >= 0)
# Augsynth Conformal Confidence Intervals

asynth <- augsynth::augsynth(
  people_fully_vaccinated_per_hundred ~ post_ohio,
  unit = state,
  time = centered_week,
  data = dat,
  progfunc = "none", fixedeff = FALSE)

# Confirm that unit weights from augsynth match tidysynth unit weights
state_names <- rownames(asynth$weights)

augsynth_weights <- asynth$weights %>% 
  as_tibble() %>% 
  bind_cols(state_names) %>% 
  select(state=...2, a_weight=V1)

weight_differences <- vaccine_out %>%
  grab_unit_weights() %>%
  left_join(augsynth_weights, by = c("unit" = "state")) %>%
  mutate(diff = weight - a_weight) 

weight_differences %>% summarise(sum(abs(diff))) # Check that differenace

weight_differences %>% arrange(desc(diff)) # differences are all less than 0.0001

summary(asynth,alpha=.05)

# Standard Error Plot
plot(asynth) +
  labs(
    title="Percent Difference in Fully Vaccinated Rates",
    subtitle="Confidence Intervals Estimated Using Conformal Inference",
    x="Weeks Relative to Lottery Announcement"
  )
ggsave(here("figures/conformal_inference_asynth_ex_lotto.jpg"))


# Permutation Tests with augsynth -----------------------------------------
# Here we test that augsynth returns the same results from permutation based 
# inference as the tidysynth package.

donor_states_list <- unique(dat$state)

permutation_test <- function(s) {
  # Takes in a state abbreviation  and runs an augsynth estimate with that state
  
  permute.panel <- dat %>% mutate(permute_post = if_else(state == s & centered_week >= 0, 1L, 0L))
  permute.est <- augsynth(people_fully_vaccinated_per_hundred ~ permute_post,
                          unit = state, time = centered_week,
                          data = permute.panel, progfun = "None", fixedeff = FALSE
  )
  cbind(summary(permute.est)$att, state = s)
}

permute_aug_synth <- donor_states_list %>% map_dfr(permutation_test)

pre_mspe <- permute_aug_synth %>% 
  group_by(state) %>% 
  filter(Time<=0) %>% 
  summarise(premspe=sqrt(mean(Estimate^2)))

post_mspe <- permute_aug_synth %>% 
  group_by(state) %>% 
  filter(Time>0) %>% 
  summarise(postmspe=sqrt(mean(Estimate^2)), 
            average_diff=mean(Estimate)) %>%
  arrange(desc(average_diff)) %>% 
  mutate(average_rank=row_number())

last_period <- permute_aug_synth %>% 
  group_by(state) %>% filter(Time==6) %>% 
  summarise(last_period_diff=Estimate) %>% 
  arrange(desc(last_period_diff)) %>%
  mutate(last_period_rank=row_number())

inference_tbl <- post_mspe %>%
  left_join(pre_mspe, by = "state") %>%
  left_join(last_period, by = "state") %>%
  mutate(mspe_ratio = (postmspe / premspe)^2) %>%
  arrange(desc(mspe_ratio)) %>%
  mutate(mspe_rank=row_number())

inference_tbl %>% filter(state=="OH")
# note this is exact match for state_performance_metrics calculated with tidy_synth


#Bayesian Estimates  Do Not Use
# library(bpCausal)
#
# placebo.est
# inference_tbl %>% mutate(mspe_squared=mspe_ratio^2) %>% filter(state=="OH")
# out1 <- bpCausal(data = dat %>% as.data.frame(), ## simulated dataset  
#                  index = c("fips", "week"), ## names for unit and time index
#                  Yname = "people_fully_vaccinated_per_hundred", ## outcome variable
#                  Dname = "post_ohio", ## treatment indicator  
#                   Xname = c(),
#                   Zname = c(),
#                   Aname = c(),
#                  re = "both",   # two-way random effect: choose from ("unit", "time", "none", "both") 
#                  ar1 = TRUE,    # whether the time-level random effects is ar1 process or jsut multilevel (independent)
#                  r = 10,        # factor numbers 
#                  niter = 15000, # number of mcmc draws
#                  burn = 5000,   # burn-in draws 
#                  xlasso = 0,    ## whether to shrink constant coefs (1 = TRUE, 0 = FALSE)
#                  zlasso = 0,    ## whether to shrink unit-level random coefs (1 = TRUE, 0 = FALSE)
#                  alasso = 0,    ## whether to shrink time-level coefs (1 = TRUE, 0 = FALSE)
#                  flasso = 0,    ## whether to shrink factor loadings (1 = TRUE, 0 = FALSE)
#                  a1 = 0.001, a2 = 0.001, ## parameters for hyper prior shrink on beta (diffuse hyper priors)
#                  b1 = 0.001, b2 = 0.001, ## parameters for hyper prior shrink on alpha_i
#                  c1 = 0.001, c2 = 0.001, ## parameters for hyper prior shrink on xi_t
#                  p1 = 0.001, p2 = 0.001
#                  ) ## parameters for hyper prior shrink on factor terms
# 
# out1
# coefSummary(out1)
# 
# 
# eout1 <- effSummary(out1,   ## summary treatment effects
#                     usr.id = 39, ## treatment effect for individual treated units, if input NULL, calculate average TT
#                     cumu = FALSE,  ## whether to calculate culmulative treatment effects
#                     rela.period = TRUE) ## whether to use time relative to the occurence of treatment (1 is the first post-treatment
