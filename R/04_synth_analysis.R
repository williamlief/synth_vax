# This script runs the synth model with all states
# This analysis is reported in Appendic C: Fifty State Specification

library(here)
library(tidyverse)
library(tidysynth)
library(stargazer)
library(augsynth)

dat <- readRDS(here("data/weekly_data_2021-06-24.rds")) 

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

# NOTE: instead of loading the pre-registered weights we simply rerun the model
# this results in the exact same weights since the weights are based only on 
# pre-treatment data. 
# Confirm in table below that weights are identical to three decimal places to 
# the pre-registered weights.
vaccine_out %>%
  grab_unit_weights() %>%
  mutate(weights = round(weight, digits = 4)) %>%
  select(unit, weights) %>%
  filter(weights>0.0001) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, rownames = FALSE)

vaccine_out %>% plot_weights() + 
  labs(title="Synthetic Control Weights")   
# Note: not included in paper writeup
ggsave(here("figures/weights.jpg"))

# Balance Table
vaccine_out %>%
  grab_balance_table() %>%
  mutate(difference = OH - synthetic_OH) %>%
  select(variable, OH, synthetic_OH, difference, donor_sample) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, rownames = FALSE, 
            caption = "Balance Table", 
            label = "balancetable")

# Plot Model Trends
vaccine_out %>% plot_trends() +
  scale_x_continuous(breaks = c(-15,-10,-5,0,5)) +
  labs(
    title = "Ohio and Synthetic Ohio",
    caption = "Timing of The Lottery Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Percent Fully Vaccinated"
  ) 
# NOTE: not included in paper writeup
ggsave("figures/treatment_trends.jpg")

# Plot Model Differences
vaccine_out %>% plot_differences() +
  scale_x_continuous(breaks = c(-15,-10,-5,0,5)) +
  labs(
    title = "Difference between Ohio and Synthetic Ohio",
    caption = "Timing of The Lottery Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Difference in Percent Fully Vaccinated"
  ) 
ggsave("figures/treatment_differences.jpg")





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

weight_differences %>% arrange(desc(diff)) # differences are all less than 0.00011

summary(asynth,alpha=.05)

# Standard Error Plot
plot(asynth) +
  labs(
    title="Percent Difference in Fully Vaccinated Rates",
    subtitle="Confidence Intervals Estimated Using Conformal Inference",
    x="Weeks Relative to Lottery Announcement"
  )
ggsave(here("figures/conformal_inference_asynth_including_lotto.jpg"))


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


# Placebo Test ------------------------------------------------------------
# This test shifts the pre-treatment window back five weeks.
# This analysis was included in our pre-registration as a demonstration of the 
# method and to show that we did not find treatment effects before the lottery 
# was announced. 

placebo_out <-
  dat %>% 
  filter(centered_week <= 0) %>% 
  # initial the synthetic control object
  synthetic_control(outcome = people_fully_vaccinated_per_hundred, # outcome
                    unit = state, # unit index in the panel data
                    time = centered_week, # time index in the panel data
                    i_unit = "OH", # unit where the intervention occurred
                    i_time = -5, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
                    ) %>%
# Matching on fully vaccination the weeks before the intervention  
  generate_predictor(time_window = -17, people_fully_vaccinated_per_hundred17 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -16, people_fully_vaccinated_per_hundred16 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -15, people_fully_vaccinated_per_hundred15 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -14, people_fully_vaccinated_per_hundred14 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -13, people_fully_vaccinated_per_hundred13 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -12, people_fully_vaccinated_per_hundred12 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -11, people_fully_vaccinated_per_hundred11 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -10, people_fully_vaccinated_per_hundred10 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -09, people_fully_vaccinated_per_hundred09 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -08, people_fully_vaccinated_per_hundred08 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -07, people_fully_vaccinated_per_hundred07 = people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window = -06, people_fully_vaccinated_per_hundred06 = people_fully_vaccinated_per_hundred) %>%
  # Generate the fitted weights for the synthetic control
  generate_weights(optimization_window = -17:-6, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  # Generate the synthetic control
  generate_control()


placebo_out %>% plot_trends()  + 
  labs(
    title = "Placebo Analysis: Ohio and Synthetic Ohio",
    caption = "Timing of The Placebo Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Percent Fully Vaccinated"
  )
ggsave(here("figures/placebo_analysis.jpg"))


placebo_out %>% plot_differences()  + 
  labs(
    title = "Placebo Analysis:  Difference between Ohio and Synthetic Ohio",
    caption = "Timing of The Placebo Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Percent Fully Vaccinated"
  )
ggsave(here("figures/placebo_difference.jpg"))

placebo_out %>% grab_signficance() %>% filter(unit_name=="OH")
placebo_out %>% grab_unit_weights() %>% arrange(desc(weight))

placebo_out %>% plot_mspe_ratio() 
ggsave(here("figures/placebo_mspe.jpg"))





