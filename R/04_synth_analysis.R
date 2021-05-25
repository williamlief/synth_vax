# This script runs the synth model and creates the pre-registered synthetic weights
# It also runs a placebo synthetic model. 

library(here)
library(tidyverse)
library(tidysynth)
library(stargazer)


dat <- readRDS(here("data/weekly_data_2021-05-17.rds")) 


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
  # Matching on FUlly vaccination the weeks before the intervention  
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

# Plot Model


vaccine_out %>% plot_trends() +
  labs(
    title = "Ohio and Synthetic Ohio",
    caption = "Timing of The Lottery Announcement",
    x="Weeks Relative to Lottery Announcement",
    y="Percent Fully Vaccinated"
  ) 

ggsave(here("figures/pretreatment_synth.jpg"))


vaccine_out %>% plot_differences() +
  labs(
    title = "Ohio and Synthetic Ohio",
    caption = "Timing of The Lottery Announcement",
    x = "Weeks Relative to Lottery Announcement",
    y = "Percent Fully Vaccinated"
  ) 


vaccine_out %>% plot_weights() + 
  labs(title="Synthetic Control Weights")   

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



# Unit weights 

vaccine_out %>%
  grab_unit_weights() %>%
  mutate(weights = round(weight, digits = 4)) %>%
  select(unit, weights) %>%
  as.data.frame() %>%
  stargazer(summary = FALSE, rownames = FALSE)


# Unit weights to csv

vaccine_out %>%
  grab_unit_weights() %>%
  mutate(weights = round(weight, digits = 4)) %>%
  select(unit, weights) %>%
  write_csv(here("output/unit_weights.csv"))



# Placebo Test ------------------------------------------------------------


placebo_out <-
  
  dat %>%
  
  # initial the synthetic control object
  synthetic_control(outcome = people_fully_vaccinated_per_hundred, # outcome
                    unit = state, # unit index in the panel data
                    time = centered_week, # time index in the panel data
                    i_unit = "OH", # unit where the intervention occurred
                    i_time = -5, # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
                    ) %>%
# Matching on FUlly vaccination the weeks before the intervention  
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

placebo_out %>% unnest(.unit_weights) %>% filter(weight>.001) %>% count()

placebo_out %>% plot_mspe_ratio() 

ggsave(here("figures/placebo_mspe.jpg"))


