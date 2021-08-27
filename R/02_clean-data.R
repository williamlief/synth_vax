# This script processes the raw data and saves clean analysis data sets into the
# data/ directory

library(dplyr)
library(lubridate)
library(readr)
library(stringr)
library(here)
library(glue)

# This is the date that raw vax data was pulled
# used in name when writing clean data
date <- "2021-08-18"

fips <- read_csv(here::here("data-raw/fips.csv"))
raw_cases <- read_csv(here::here(str_glue("data-raw/us_cases_{date}.csv")))
raw_vax <- read_csv(here::here(str_glue("data-raw/us_vaccines_{date}.csv")))

# Lottery Announcement Date
announce_date = mdy("05/12/2021")

cases <- raw_cases %>% 
  mutate(day = mdy(submission_date)) %>% 
  tidylog::inner_join(fips %>% select(abb, fips),
             by = c("state" = "abb"))

vax <- raw_vax %>% 
  mutate(day = ymd(date), 
         location = recode(location, "New York State" = "New York")) %>% 
  tidylog::inner_join(fips %>% select(state, fips),
             by = c("location" = "state"))

daily <- cases %>% 
  inner_join(vax, by = c("fips", "day")) %>% 
  mutate(centered_time = day - announce_date, 
         # use vax per million rate to convert cases to per millions
         pop_pmr = daily_vaccinations / daily_vaccinations_per_million, 
         across(c(tot_cases, new_case, tot_death, new_death), 
                function(x) x / pop_pmr, .names = "{.col}_per_million"))

# Note: all data is within 2021, so don't need to worry about years
weekly <- daily %>% 
  mutate(week = isoweek(day)) %>% 
  group_by(state, fips, week) %>% 
  summarize(across(c(new_case_per_million, new_death_per_million, 
                     daily_vaccinations_per_million), 
                   sum, na.rm = T), 
            across(c(tot_cases_per_million, tot_death_per_million, 
                     people_fully_vaccinated_per_hundred,total_vaccinations_per_hundred,people_vaccinated_per_hundred), 
                   max, na.rm = T),
            n_rec = n(), 
            first_day = min(day), last_day = max(day),
            .groups = "drop") %>% 
  mutate(centered_week = week - isoweek(announce_date))

write_rds(daily, str_glue(here("data/daily_data_{date}.rds")))
write_rds(weekly, str_glue(here("data/weekly_data_{date}.rds")))


  