# This file creates some basic descritpive analyses of the vaccination rate data

library(dplyr)
library(ggplot2)
library(gghighlight)
library(here)
library(lubridate)

dat <- readRDS(here("data/weekly_data_2021-06-24.rds")) 

ggplot(dat, aes(x = last_day, 
                y = people_fully_vaccinated_per_hundred, 
                group = state)) +
  geom_line() + 
  gghighlight(state=="OH",
              label_params = list(fill = NA, alpha=1)) +
  geom_vline(xintercept = lubridate::make_date(2021, 5, 12), linetype = "solid") +
  labs(
    title = "Vaccination Rates by State by Week",
    caption = "Timing of The Ohio Lottery Announcement",
    x = "Date",
    y = "Percent Fully Vaccinated"
  ) +
  theme_minimal()

ggsave(here("figures/state_plot_lines.jpg"))

dat %>% 
  filter(centered_week == 4)  %>% 
  arrange(desc(people_fully_vaccinated_per_hundred)) %>% 
  mutate(rank=row_number()) %>% 
  select(state,people_fully_vaccinated_per_hundred,rank)