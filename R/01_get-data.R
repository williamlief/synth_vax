# This script pulls raw data directly from online sources and saves to the 
# data-raw/ directory. 
# This file pulls vaccination rate and case data. 
# Note: auxillary covariate data is obtained in the python/data_processing.py

library(readr)
library(here)

# Vaccination Data --------------------------------------------------------

# Data is downloaded from https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations
# https://www.nature.com/articles/s41562-021-01122-8

us_vaccines <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/us_state_vaccinations.csv?raw=true")
write_csv(us_vaccines, here(paste0("data-raw/us_vaccines_", Sys.Date(), ".csv")))


# Case Data ---------------------------------------------------------------

# Data is downloaded from the CDC: https://dev.socrata.com/foundry/data.cdc.gov/9mfq-cb36
# Note: we did not use this data in the pre-registration, opting for a parsimonious 
# model without covariates.

us_cases <- read_csv("https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD&api_foundry=true")
write_csv(us_cases, here(paste0("data-raw/us_cases_", Sys.Date(), ".csv")))

