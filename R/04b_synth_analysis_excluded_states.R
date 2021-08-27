This script reruns the synthetic analysis excluding states that adopted lotteries 
# after Ohio.

library(here)
library(tidyverse)
library(tidysynth)
library(stargazer)

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


dat<- dat %>% mutate(post_ohio=state=="OH" & centered_week>=0)
# Augsynth Conformal Confidence Intervals
asynth<-augsynth::augsynth(people_fully_vaccinated_per_hundred~post_ohio,unit = state,time = centered_week,data = dat)
state_names<-rownames(asynth$weights)
augsynth_weights<-asynth$weights %>% as_tibble() %>% bind_cols(state_names) %>% select(state=...2,a_weight=V1)


weight_differences<-vaccine_out %>%
  grab_unit_weights() %>%
  left_join(augsynth_weights, by = c("unit" = "state")) %>%
  mutate(diff = weight - a_weight) 

weight_differences %>% summarise(sum(abs(diff))) # Check that differenace

weight_differences %>% arrange(desc(diff))

summary(asynth,alpha=.05)
plot(asynth) +
  labs(
    title="Percent Difference in Fully Vaccinated Rates",
    subtitle="Confidence Intervals Estimated Using Conformal Inference",
    x="Weeks Relative to Lottery Announcement"
  )
ggsave(here("figures/conformal_inference_asynth_ex_lotto.jpg"))
