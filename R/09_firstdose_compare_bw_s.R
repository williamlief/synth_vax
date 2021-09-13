# This script compares synthetic control models from Barber and West and Sehgal 
# to our estimates. Weights are taken from July versions of papers from both 
# authors

library(here)
library(tidyverse)
library(lubridate)

fips <- read_csv(here("data-raw/fips.csv"))

dat <- readRDS(here("data/weekly_data_2021-08-18.rds")) %>% 
  select(state, fips, week, centered_week, last_day,
         people_vaccinated_per_hundred, 
         people_fully_vaccinated_per_hundred) %>% 
  tidylog::left_join(fips %>% 
              select(state = abb, location = state))

barber_weights <- read_csv(here("data-raw/barber_west_weights_wp_july_2021.csv")) 
sehgal_weights <- read_csv(here("data-raw/sehgal_weights_july_2021.csv"))
this_weights <- read_csv(here("output/unit_weights_first_dose_ex_lotto.csv"))

bw_synth <- dat %>% 
  tidylog::left_join(barber_weights %>% 
                       rename(weight = Vaccinations), 
                  by = c("location" = "State")) %>% 
  mutate(synth = weight * people_vaccinated_per_hundred) %>% 
  group_by(centered_week) %>% 
  summarize(bw_synth = sum(synth, na.rm = T),
            bw_n = sum(weight, na.rm = T)) # bw_n lets us confirm that we dont have any missing data problems

s_synth <- dat %>% 
  tidylog::left_join(sehgal_weights %>% 
                       rename(weight = Weight), 
                     by = c("location" = "State")) %>% 
  mutate(synth = weight * people_vaccinated_per_hundred) %>% 
  group_by(centered_week) %>% 
  summarize(s_synth = sum(weight * people_vaccinated_per_hundred, na.rm = T),
            s_n = sum(weight, na.rm = T))

this_synth <- dat %>% 
  tidylog::left_join(this_weights %>% 
                       rename(weight = weights), 
                     by = c("state" = "unit")) %>% 
  mutate(synth = weight * people_vaccinated_per_hundred) %>% 
  group_by(centered_week) %>% 
  summarize(t_synth = sum(weight * people_vaccinated_per_hundred, na.rm = T),
            t_n = sum(weight, na.rm = T))

oh <- dat %>% 
  filter(state == "OH") 

compare <- oh %>% 
  left_join(bw_synth) %>% 
  left_join(s_synth) %>% 
  left_join(this_synth) %>% 
  mutate(`Barber-West` = people_vaccinated_per_hundred - bw_synth, 
         `Sehgal` = people_vaccinated_per_hundred - s_synth,
         `This Paper` = people_vaccinated_per_hundred - t_synth) %>% 
  pivot_longer(c(`Barber-West`, `Sehgal`, `This Paper`), names_to = "estimate")

ggplot(compare, 
       aes(x = last_day, y = value, color = estimate)) +
  geom_line() +
  theme_bw() +
  geom_label(aes(x = make_date(2021, 4, 1), y = 1),
             label = "Pre-Lottery Announcment", color = "black") +
  geom_vline(xintercept = make_date(2021, 5, 12), linetype = "solid") +
  geom_label(aes(x = make_date(2021, 5, 12) + 20, y = 1), 
                label = "Lottery Period", color = "black") +
  geom_vline(xintercept = make_date(2021, 6, 23), linetype = "dashed") +
  geom_label(aes(x = make_date(2021, 6, 23) + 18, y = 1), 
                label = "Post Lottery", color = "black") +
  labs(x = "Data Aggregated to Week Level", 
       y = "Difference in First Doses (OH - Synthetic OH)", 
       color = "Effect Estimate", 
       title = "Extended Time Analysis of First Dose Effects", 
       subtitle = "Observed Ohio First Dose Vaccination Rate Minus Synthetic Counterfactual")

ggsave(here("figures/first_dose_compare.jpg"), bg = "white")





