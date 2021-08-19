# Overview 

This directory obtains auxillary covariate data. Covariate use was excluded from our initial pre-registration but included to inform the multiverse analysis of different reasonable modelling decisions. 

# Data sources and variable information

## Data sources
- ### Population density
	- Data downloaded directly within code, drawn from 2020 Census redistricting data
	- More information available [here](https://www.census.gov/data/tables/time-series/dec/density-data-text.html)

- ### Race/Ethnicity and age buckets
	- User must download data (instructions below), drawn from 2019 ACS survey
	- The following link selects the desired demographics from the 2019 ACS data.
		- [Direct link to table](https://data.census.gov/cedsci/table?q=United%20States&g=0100000US,.04000.001&tid=ACSDP1Y2019.DP05&hidePreview=true)
	- At the top of the table, click the download button.
	- Click download in the window that pops-up. 
		- Settings should be preset to download ACS 1-Year Estimates Data Profiles from 2019 in CSV format. 
	- Unzip downloaded folder, data file will be called "ACSDP1Y2019.DP05_data_with_overlays_2021-08-06T104400.csv"

- ### Income, poverty levels, educational attainment
	- Data downloaded directly within code, drawn from 2019 ACS survey
	- More information available [here](https://www.census.gov/acs/www/data/data-tables-and-tools/ranking-tables/)

- ### 2020 Republican Presidential vote share
	- User must download data (instructions below), provided by MIT Election and Data Science Lab (MEDSL)
	- The following url links directly to data file
		- [Link to data file](https://dataverse.harvard.edu/file.xhtml?fileId=4299753&version=6.0)
		- [Permanent DOI link](https://doi.org/10.7910/DVN/42MVDX)
	- Using above link to datafile, select access file
	- Select "Comma Separated Values (Original File Format)"
	- File will be called "1976-2020-president.csv"

- ### 2019 Influenza Vaccination Rates
	- User must download data (instructions below), provided by Centers for Medicare and Medicaid Services (CMS)
	- Start at the following link
		- [Link to data](https://data.cms.gov/tools/mapping-medicare-disparities-by-population)
	- Enter the following parameters
		- Geography = State/Territory
		- Measure = Preventive Services
		- Condition = Influenza Virus Vaccine
	- Click Download Data 
	- File will be called "mmd_data.csv"

- ### Daily Mobility Data January 1 2021 through August 6 2021
	- User must download data (instructions below), provided by Google
	- Start at the following link
		- [Link to Google Mobility page](https://www.google.com/covid19/mobility/)
	- Click the Region CSVs button under Community Mobility Reports
		- [Direct download link](https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip)
	- Unzip downloaded folder, desired file is called "2021_US_Region_Mobility_Report.csv"


## Variable information

- ### annual_dataset_processed.csv
	- 'fips' = State fips code 
	- 'state_abb' = 2-letter state name abbreviations 
	- 'pop_density' = Population density (population / square mile) from 2020 Census data
	- 'pct_under_18' = Percent of individual under 18 years old from 2019 ACS data
	- 'pct_18_64' = Percent of individuals between 18 and 64 years old from 2019 ACS data
	- 'pct_65_over' = Percent of individuals 65 years and older from 2019 ACS data
    - 'pct_white' = Percent of individuals who identify as White (only) from 2019 ACS data
    - 'pct_black', = Percent of individuals who identify as (only) Black or African American from 2019 ACS data
    - 'pct_amer_indian' = Percent of individuals who identify as (only) American Indian or Alaska Native from 2019 ACS data
    - 'pct_asian' = Percent of individuals who identify as Asian (only) from 2019 ACS data
    - 'pct_pacific_islander' = Percent of individuals who identify as (only) Native Hawaiian or Other Pacific Islander from 2019 ACS data
    - 'pct_other_race' = Percent of individuals who identify as (only) some other race than those listed above from 2019 ACS data
    - 'pct_two_or_more_races' = Percent of individuals who identify as members of two or more races from 2019 ACS data
    - 'pct_hispanic' = Percent of individuals who identify as Hispanic from 2019 ACS data
    - 'med_household_income' = Median household income from 2019 ACS data
    - 'pct_in_poverty' = Percent of people below poverty level in past 12 months from 2019 ACS data
    - 'pct_not_hs' = Percent of people 25 years or older who have not completed High School from 2019 ACS data
    - 'pct_bachelors_deg' = Percent of people 25 years or older who have completed a Bachelors Degree from 2019 ACS data
    - 'repub_vote_share_2020' = Percent of total votes cast in 2020 Presidential election for Republican Candidate from MEDSL data
    - 'flu_vaccination_rate_2019' = Percent of population who received an Influenza vaccination in 2019 from CMS data

- ### daily_dataset_processed.csv
- #### CSV documentation and methodolody from Google available [here](https://www.google.com/covid19/mobility/data_documentation.html?hl=en)
	- 'fips' = State fips code
	- 'state_abb' = 2-letter state name abbreviations 
	- 'date' = 'date of data observation'
	- 'retail_and_recreation_percent_change_from_baseline' = Mobility trends for places like restaurants, cafes, shopping centers, theme parks, museums, libraries, and movie theaters.
	- 'grocery_and_pharmacy_percent_change_from_baseline' = Mobility trends for places like grocery markets, food warehouses, farmers markets, specialty food shops, drug stores, and pharmacies.
	- 'parks_percent_change_from_baseline' = Mobility trends for places like local parks, national parks, public beaches, marinas, dog parks, plazas, and public gardens.
	- 'transit_stations_percent_change_from_baseline' = Mobility trends for places like public transport hubs such as subway, bus, and train stations.	
	- 'workplaces_percent_change_from_baseline' = Mobility trends for places of work.	
	- 'residential_percent_change_from_baseline' = Mobility trends for places of residence.
