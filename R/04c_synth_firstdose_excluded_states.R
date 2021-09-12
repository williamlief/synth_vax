# This script runs the synthetic control model with first doses as the outcome
# It excludes all other lottery adopting states

library(here)
library(tidyverse)
library(tidysynth)

dat <- readRDS(here("data/weekly_data_2021-08-18.rds")) 

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
  synthetic_control(outcome = people_vaccinated_per_hundred, # !!! Only difference in code is this outcome switch
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

vaccine_out %>%
  grab_unit_weights() %>%
  mutate(weights = round(weight, digits = 4)) %>%
  select(unit, weights) %>%
  write_csv(here("output/unit_weights_first_dose_ex_lotto.csv"))