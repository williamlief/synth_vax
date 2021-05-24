# This file creates some basic descritpive analyses of the vaccination rate data

library(dplyr)
library(ggplot2)
library(gghighlight)
library(here)
library(lubridate)

dat <- readRDS(here("data/weekly_data_2021-05-17.rds")) 

ggplot(dat, aes(x = last_day, 
                y = people_fully_vaccinated_per_hundred, 
                color = state)) +
  geom_line() + 
  gghighlight(state == "OH") +
  geom_vline(xintercept = lubridate::make_date(2021, 5, 12), linetype = "dotted") +
  labs(
    title = "Vaccination Rates by State by Week",
    caption = "Timing of The Lottery Announcement",
    x = "Date",
    y = "Percent Fully Vaccinated"
  ) 

ggsave(here("figures/state_plot_lines.jpg"))

dat %>% 
  filter(centered_week == 0)  %>% 
  arrange(desc(people_fully_vaccinated_per_hundred)) %>% 
  mutate(rank=row_number()) %>% 
  select(state,people_fully_vaccinated_per_hundred,rank)

