import os
import pandas as pd

df_vars = []

### STATE LEVEL POPULATION DENSITY DATA, 2020 CENSUS
## Data downloaded directly, more info on these data can be found here: https://www.census.gov/data/tables/time-series/dec/density-data-text.html
density_df = pd.read_csv('https://www2.census.gov/programs-surveys/decennial/2020/data/apportionment/apportionment.csv')
#select desired columns
density_df = density_df.loc[(density_df['Year'] == 2020) & (density_df['Geography Type'] == 'State')]
density_df.set_index('Name',inplace = True)
#drop PR from list of states
density_df.drop(['Puerto Rico'], inplace = True)
pop_density = {}
#create list of dicts for {state: population density} to ensure values stay associated with correct state
for i in range(len(density_df)):
    pop_density[density_df.index[i].lower()] = float(density_df['Resident Population Density'][i].replace(',',''))
    
df_vars.append({'pop_density':pop_density})

### RACE AND ETHNICITY DATA FROM 2019 ACS SURVEY
## Warning: data must be downloaded by user, see readme for instructions

df = pd.read_csv(os.path.join('python/raw_data','ACSDP1Y2019.DP05_data_with_overlays_2021-08-06T104400.csv'))
#delete column labels/metadata from dataset
df.drop([0], inplace=True)
#create list of states to use in dict with relevant data
states = df['NAME'].iloc[:51].tolist()
states = [x.lower() for x in states]

#gather percentage of people under 18 in each state
#only the first 51 rows are used so that US territories are not included in dataset
ls_u18 = list(map(float,df['DP05_0019PE'].iloc[:51].tolist()))
#gather percentage of people over 18 in each state
ls_o18 = list(map(float,df['DP05_0021PE'].iloc[:51].tolist()))
#gather percentage of people over 65 in each state
ls_o65 = list(map(float,df['DP05_0024PE'].iloc[:51].tolist()))
#calculate percentage of people between 18 and 64 in each state
ls_18_64 = list(map(lambda x, y:round(x-y,2), ls_o18,ls_o65))
#create dictionary {variable name: {state : value}} for each age bucket
df_vars.append({'pct_under_18':dict(zip(states,ls_u18))})
df_vars.append({'pct_18_64':dict(zip(states,ls_18_64))})
df_vars.append({'pct_65_over':dict(zip(states,ls_o65))})

#gather percentage of people identifying as given race/ethnicity in each state using same process as above
white = list(map(float,df['DP05_0037PE'].iloc[:51].tolist()))
black = list(map(float,df['DP05_0038PE'].iloc[:51].tolist()))
#American Indian and Alaska Native
amer_ind = list(map(float,df['DP05_0039PE'].iloc[:51].tolist()))
asian = list(map(float,df['DP05_0044PE'].iloc[:51].tolist()))
#Native Hawaiian and other Pacific Islanders
nat_hawaiian = list(map(float,df['DP05_0052PE'].iloc[:51].tolist()))
#Other races not included above
other = list(map(float,df['DP05_0057PE'].iloc[:51].tolist()))
#Two or more races
two_race = list(map(float,df['DP05_0058PE'].iloc[:51].tolist()))
#Hispanic
hisp = list(map(float,df['DP05_0071PE'].iloc[:51].tolist()))

df_vars.append({'pct_white':dict(zip(states,white))})
df_vars.append({'pct_black':dict(zip(states,black))})
df_vars.append({'pct_amer_indian':dict(zip(states,amer_ind))})
df_vars.append({'pct_asian':dict(zip(states,asian))})
df_vars.append({'pct_pacific_islander':dict(zip(states,nat_hawaiian))})
df_vars.append({'pct_other_race':dict(zip(states,other))})
df_vars.append({'pct_two_or_more_races':dict(zip(states,two_race))})
df_vars.append({'pct_hispanic':dict(zip(states,hisp))})

del df

### EDUCATIONAL ATTAINMENT DATA FROM 2019 ACS SURVEY
#links to two data tables 
files = ['https://www2.census.gov/programs-surveys/acs/summary_file/2019/data/1_year_ranking/R1501.xlsx','https://www2.census.gov/programs-surveys/acs/summary_file/2019/data/1_year_ranking/R1502.xlsx']
labels = ['pct_not_hs','pct_bachelors_deg']
#loop through files from 2019 ACS -- all have same format
for i in range(len(files)):
    label = labels[i]
    #import excel table, including only necessary info
    df = pd.read_excel(files[i], header = 6,usecols = 'A:C',nrows=52)
    #remove first row which contains national total
    df.drop([0],inplace=True)
    #sort states alphabetically
    df.sort_values(by=['GEOGRAPHY'], inplace = True)
    #reset index to match alphabetically sorted states
    df.reset_index(drop=True,inplace = True)
    #create dict of {state : value} for each variable of interest (in labels list)
    values = {}
    for j in range(len(df)):
        #dataset shows % adults who are HS and college grads. We want % who haven't graduated from HS
        if i == 0:
            values[df['GEOGRAPHY'][j].lower()] = float(100 - df['ESTIMATE'][j])
        #and % who have earned a degree, requiring slightly different methods
        else:
            values[df['GEOGRAPHY'][j].lower()] = float(df['ESTIMATE'][j])
    df_vars.append({label:values})
    del df


### MED. HOUSEHOLD INCOME AND POVERTY DATA FROM 2019 ACS SURVEY
files = ['https://www2.census.gov/programs-surveys/acs/summary_file/2019/data/1_year_ranking/R1901.xlsx','https://www2.census.gov/programs-surveys/acs/summary_file/2019/data/1_year_ranking/R1701.xlsx']
labels = ['med_household_income','pct_in_poverty']
#loop through files from 2019 ACS -- all have same format
for i in range(len(files)):
    label = labels[i]
    #import excel table, including only necessary info
    df = pd.read_excel(files[i], header = 6,usecols = 'A:C',nrows=52)
    #remove first row which contains national total
    df.drop([0],inplace=True)
    #sort states alphabetically
    df.sort_values(by=['GEOGRAPHY'], inplace = True)
    #reset index to match alphabetically sorted states
    df.reset_index(drop=True,inplace = True)
    #create dict of {state : value} for each variable of interest (in labels list)
    values = {}
    for j in range(len(df)):
        values[df['GEOGRAPHY'][j].lower()] = float(df['ESTIMATE'][j])
    df_vars.append({label:values})
    del df

### 2020 REPUBLICAN VOTE SHARE BY STATE FROM MIT ELECTION DATA + SCIENCE LAB
## Warning: data must be downloaded by user, see readme for instructions

df = pd.read_csv(os.path.join('python/raw_data','1976-2020-president.csv'))
#select only votes for Trump in 2020 election
df2 = df.loc[(df['year'] == 2020) & (df['party_detailed'] == 'REPUBLICAN')]
df2.reset_index(drop=True,inplace = True)
#create dict of {state : % of votes cast for trump} rounded to 2 decimal points
rep_vote = {}
for i in range(len(df2)):
    state = df2['state'][i].lower()
    rep_share = round(float((int(df2['candidatevotes'][i]) / int(df2['totalvotes'][i])) * 100),2)
    rep_vote[state] = rep_share
    
del df
del df2

df_vars.append({'repub_vote_share_2020':rep_vote})


### INFLUENZA VACCINATION RATES IN 2019 (PRE-PANDEMIC) FROM CENTERS FOR MEDICARE AND MEDICAID SERVICES (CMS)
## Warning: data must be downloaded by user, see readme for instructions

df = pd.read_csv(os.path.join('python/raw_data','mmd_data.csv'))
# only keep data from US states, drop territories
df2 = df.iloc[:51]
#create dict of {state : flu vax rate 2019}
vax_rate = {}
for i in range(len(df2)):
    state = df2['state'][i].lower()
    rate = float(df2['analysis_value'][i])
    vax_rate[state] = rate
del df
del df2
    
df_vars.append({'flu_vaccination_rate_2019':vax_rate})


### CREATE DATAFRAME FOR ANNUAL DATA, CHANGE STATE NAMES TO FIPS CODES, SAVE OUTPUT
## TODO: SET THIS TO WORK WITHIN GITHUB DIR
fips = pd.read_csv(os.path.join('data-raw','fips.csv'))
fips['state'] = fips['state'].str.lower()
fips = fips.set_index('state')
fips.rename(columns={'abb':'state_abb'},inplace=True)

final_df = fips.copy(deep=True)
for i in range(len(df_vars)):
    sub_df = pd.DataFrame.from_dict(df_vars[i])
    final_df = final_df.join(sub_df)
final_df.set_index('fips', inplace=True)

final_df.to_csv('python/annual_dataset_processed.csv')

### CLEAN DATA AND CREATE DATAFRAME OF DAILY MOBILITY DATA
## Warning: data must be downloaded by user, see readme for instructions
daily_df = pd.read_csv(os.path.join('python/raw_data','2021_US_Region_Mobility_Report.csv'))
#select only state-level data, drop county/region level data
daily_df = daily_df.loc[(daily_df['sub_region_1'].notnull()) & (daily_df['sub_region_2'].isna())]
#remove unneeded columns
daily_df.drop(['country_region_code','country_region','sub_region_2','metro_area', 'iso_3166_2_code', 'census_fips_code', 'place_id',], axis=1, inplace = True)
daily_df['sub_region_1'] = daily_df['sub_region_1'].str.lower()
daily_df.set_index('sub_region_1',inplace=True)
#add fips codes to dataset
daily_df = daily_df.join(fips)
daily_df.set_index('fips',inplace=True)
#move state name abbreviations to first column
state_abbs = daily_df.pop('state_abb')
daily_df.insert(0,'state_abb',state_abbs)
#export data to csv
daily_df.to_csv('python/daily_dataset_processed.csv')

