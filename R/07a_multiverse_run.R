# Defines and runs the multiverse analysis. Note that this script takes
# substantial processing time to run. On a 2015 macbook pro it took approximately
# 14 hours.

library(here)
library(tidyverse)
library(furrr)
library(augsynth)
library(lubridate)
library(tidysynth)

annual_cov <- read_csv(here("python/annual_dataset_processed.csv"))
daily_cov <- read_csv(here("python/daily_dataset_processed.csv"))
vax_case <- readRDS(here("data/daily_data_2021-08-18.rds"))

analysisdat <- vax_case %>%
  select(state, fips, day, date, people_fully_vaccinated_per_hundred, 
         people_vaccinated_per_hundred, total_vaccinations_per_hundred) %>% 
  tidylog::left_join(annual_cov %>% select(-state_abb), 
                     by = "fips") %>% 
  tidylog::left_join(daily_cov %>% select(-state_abb), 
                     by = c("fips", "date")) 
# NOTE: the mobility covs start on 1/1 while the vax data starts on 1/12 for 11 days that dont match
# the mobility covs stop on 8/6 and the vax data stops on 8/17, also 11 days. 
# So the mismatch counts are identical

announce_dates <- read_csv("data-raw/lottery_announce_dates.csv") %>% 
  mutate(state = str_trim(state))

# Ohio announce date 
announce_date <- announce_dates %>% filter(state == "OH") %>% 
  pull(lottery_announce_date) %>% mdy(.)


# Data parameters ---------------------------------------------------
# parameters for data processing: donor states, time window, covariates, outcome variable

data_params <- list(
  
  states_to_include = list(
    full = sort(unique(analysisdat$state)), 
    no_lottery = announce_dates %>% 
      filter(is.na(lottery_announce_date) | state == "OH") %>% 
      pull(state)),
  
  pretreat_start = list(
    full = "2021-01-12",
    bw_start = "2021-02-19", # B&W start
    post_ration = "2021-03-25" # Press release on mass vaccination sites in OH: https://coronavirus.ohio.gov/wps/portal/gov/covid-19/resources/news-releases-news-you-can-use/covid-19-update-03-25-21
  ),
  
  post_stop = list(
    lottery_end = "2021-06-24", # lottery end
    lottery_4w = "2021-07-23", # 4 weeks after lottery end per Moderna regimen
    lottery_6w = "2021-08-06" # 4+2 week buffer
  ),
  
  # Modelling Parameters
  covariates = list(
    none = "NULL",
    annual_demo = names(annual_cov[c(3, 5:6, 8:20)]),
    annual_mobility = c(names(annual_cov[c(3, 5:6, 8:20)]), names(daily_cov)[4:9])
  ),
  
  outcome = list(
    full_vax = "people_fully_vaccinated_per_hundred", 
    first_dose = "people_vaccinated_per_hundred", 
    total_doses = "total_vaccinations_per_hundred"
  )
)

data_spec <- 
  crossing(
    states_to_include = data_params$states_to_include,
    pretreat_start = data_params$pretreat_start, 
    post_stop = data_params$post_stop, 
    covariates = data_params$covariates, 
    outcome = data_params$outcome) 


# Modelling parameters ----------------------------------------------------

method = list( 
  tidysynth = "tidysynth", 
  augsynth = "augsynth"
)

# method specific opts, this is inelegant and handcoded with if/else
# tidysynth
tidysynth_opts <- list(
  cov_use = list(
    full_path = "full_path", 
    use_covs = "use_covs"
  )
)

# augsynth
augsynth_opts <- list(
  progfunc = list(
    none = "none",
    ridge = "ridge"
  ),
  fixedeff = list(
    true = TRUE, 
    false = FALSE
  )
)

model_spec = bind_rows(
  crossing(method = method$tidysynth, 
           ts_cov_use = tidysynth_opts$cov_use),
  crossing(method = method$augsynth, 
           as_progfunc = augsynth_opts$progfunc, 
           as_fixedeff = augsynth_opts$fixedeff))

multiverse_spec <- 
  crossing(model_spec, data_spec) %>% 
  # exclusions
  tidylog::filter(!(ts_cov_use == "full_path" & covariates != "NULL")) %>% # no covariates with full_path
  mutate(model_num = as.character(row_number())) %>% 
  as.data.frame() 

# define function for multiverse of models ------------------------------------

multiverse_run <- function(model_num, 
                           method, 
                           ts_cov_use, 
                           as_progfunc, 
                           as_fixedeff,
                           states_to_include,
                           pretreat_start, 
                           post_stop, 
                           covariates, 
                           outcome, 
                           data, 
                           output = "output-multiverse",
                           verbose = F) {
  
  if(verbose) {
    print(paste("model_num:", model_num))
    print(paste("method:", method))
    print(paste("method_opts:", ts_cov_use, as_progfunc, as_fixedeff))
    print(paste("states_to_include, count:", length(states_to_include)))
    print(paste("pretreat_start:", pretreat_start))
    print(paste("post_stop:", post_stop))
    print(paste("covariates:", covariates))
    print(paste("outcome:", outcome))
    print("--------------------------")
  }
  
  # Treatment window and data collapse to week
  data <- data %>% filter(
    day >= pretreat_start, 
    day <= post_stop,
    state %in% states_to_include
  ) %>% 
    # note: this was much cleaner w/o aggregation inside the loop, but because of 
    # the day cutoffs it is more accurate to do it here
    mutate(week = isoweek(day)) %>% 
    group_by(state, week) %>% 
    summarize(across(c(people_vaccinated_per_hundred, 
                       total_vaccinations_per_hundred,
                       people_fully_vaccinated_per_hundred), 
                     max, na.rm = T),
              across(any_of(covariates), mean, na.rm = T),
              .groups = "drop") %>% 
    mutate(centered_week = week - isoweek(announce_date),
           treat = state == "OH" & centered_week >= 0)
  
  data$outcome = data[[outcome]]
  
  if(method == "tidysynth") {
    
    setup_synth <- data  %>%
      # initial the synthetic control object
      synthetic_control(outcome = outcome, # outcome
                        unit = state, # unit index in the panel data
                        time = centered_week, # time index in the panel data
                        i_unit = "OH", # unit where the intervention occurred
                        i_time = 0, # time period when the intervention occurred
                        generate_placebos = T # generate placebo synthetic controls (for inference)
      ) 
    
    pre_window <- min(data$centered_week)
    
    if(ts_cov_use == "full_path") {
      if (!pre_window %in% c(-17, -12, -7)) {
        stop("full path is hardcoded to specific windows. Specified time window not in hard coded options of -17, -12, -5")
      }
      if(pre_window == -17) {
        setup_synth <- setup_synth %>% 
          generate_predictor(time_window = -17, lagged_outcome17 = outcome) %>%
          generate_predictor(time_window = -16, lagged_outcome16 = outcome) %>%
          generate_predictor(time_window = -15, lagged_outcome15 = outcome) %>%
          generate_predictor(time_window = -14, lagged_outcome14 = outcome) %>%
          generate_predictor(time_window = -13, lagged_outcome13 = outcome)
      } 
      if (pre_window %in% c(-17, -12)) {
        setup_synth <- setup_synth %>% 
          generate_predictor(time_window = -12, lagged_outcome12 = outcome) %>%
          generate_predictor(time_window = -11, lagged_outcome11 = outcome) %>%
          generate_predictor(time_window = -10, lagged_outcome10 = outcome) %>%
          generate_predictor(time_window = -09, lagged_outcome09 = outcome) %>%
          generate_predictor(time_window = -08, lagged_outcome08 = outcome)       
      }
      if (pre_window %in% c(-17, -12, -7)) {
        setup_synth <- setup_synth %>% 
          generate_predictor(time_window = -07, lagged_outcome07 = outcome) %>%
          generate_predictor(time_window = -06, lagged_outcome06 = outcome) %>%
          generate_predictor(time_window = -05, lagged_outcome05 = outcome) %>%
          generate_predictor(time_window = -04, lagged_outcome04 = outcome) %>%
          generate_predictor(time_window = -03, lagged_outcome03 = outcome) %>%
          generate_predictor(time_window = -02, lagged_outcome02 = outcome) %>%
          generate_predictor(time_window = -01, lagged_outcome01 = outcome)
      }
    }
    
    if(ts_cov_use == "use_covs") {
      
      # with covariates, use the five weeks lagged outcome to match on
      setup_synth <- setup_synth %>% 
        generate_predictor(time_window = -05, lagged_outcome05 = outcome) %>%
        generate_predictor(time_window = -04, lagged_outcome04 = outcome) %>%
        generate_predictor(time_window = -03, lagged_outcome03 = outcome) %>%
        generate_predictor(time_window = -02, lagged_outcome02 = outcome) %>%
        generate_predictor(time_window = -01, lagged_outcome01 = outcome)
      
      
      if(covariates[[1]] != "NULL") {
        setup_synth <- setup_synth %>% 
          generate_predictor(time_window = pre_window:-1,
                             across(all_of(covariates), mean, na.rm = T))
      }
    }
    
    synth_out <- setup_synth %>% 
      generate_weights(optimization_window = pre_window:-1, # time to use in the optimization task
                       margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
      ) %>%
      # Generate the synthetic control
      generate_control()
    
    # Extract relevent output
    # weights, average: att, mspe, pval, last_period: att, mspe, pval.
    weights <- synth_out %>%
      grab_unit_weights()
    
    mspe <- synth_out %>% 
      grab_signficance() %>% 
      rename(mspe_rank=rank) 
    
    average_diff <- synth_out %>%
      unnest(.synthetic_control) %>%
      filter(time_unit >= 1) %>%
      group_by(.id) %>%
      summarise(average_difference = mean(real_y - synth_y)) %>%
      arrange(desc(average_difference)) %>%
      mutate(average_rank=row_number()) 
    
    last_period_diff <- synth_out %>%
      unnest(.synthetic_control) %>%
      filter(time_unit == max(time_unit)) %>%
      group_by(.id) %>%
      summarise(last_period_diff = (real_y - synth_y),
                .groups = "drop") %>%
      distinct() %>%
      ungroup() %>%
      arrange(desc(last_period_diff)) %>% 
      mutate(last_period_rank=row_number())
    
    state_performance_metrics <- 
      mspe %>% 
      left_join(average_diff,by=c("unit_name"=".id")) %>% 
      left_join(last_period_diff,by=c("unit_name"=".id")) %>% 
      left_join(weights, by = c("unit_name" = "unit")) %>% 
      mutate(model_num = model_num)
    
    # write the full synth output to disk
    saveRDS(synth_out, paste0(output, "/model_", model_num, ".rds"))
    # function returns selected metrics in memory
    return(state_performance_metrics)
  }
  
  if(method == "augsynth") {
    # Covariate inclusion 
    formula = paste0(paste0(outcome, collapse = "+"), " ~ treat")
    if(covariates[[1]] != "NULL") {
      formula = paste(formula, paste0(covariates, collapse = "+"), sep = "|")
    }
    
    # Permute the augsynth
    names(states_to_include) <- states_to_include
    wrap_augsynth <- function(treated_state) {
      data <- data %>% 
        mutate(treat = state == treated_state & centered_week >= 0)
      
      augsynth(as.formula(formula), 
               # Fixed params          
               unit = state, time = centered_week, data = data,
               t_int = 0, 
               # variable params
               progfunc = as_progfunc, fixedeff = as_fixedeff,
               scm = T)
    }
    augsynth_out <- states_to_include %>% 
      map(quietly(wrap_augsynth))
    
    # augsynth_out structured as: 
    # unit_name, type, pre_mspe, post_mspe, mspe_ratio, mspe_rank, 
    # fischers_exact_pvalue, z_score, average_difference, average_rank, 
    # last_period_diff, last_period_rank, weight
    
    # only care about weights for the OH model
    weights <- augsynth_out[["OH"]]$result$weights %>% 
      as.data.frame(.) %>% 
      rownames_to_column(var = "unit_name") %>% 
      rename(weight = V1)
    
    as_sum <- augsynth_out %>%
      map("result") %>% 
      map(summary)
    
    get_augsynth_metrics <- function(as_sum) {
      
      pre_mspe <- as_sum$att %>% 
        filter(Time < 0) %>% 
        summarize(pre_mspe = mean(Estimate^2))
      
      post_mspe <- as_sum$att %>% 
        filter(Time >= 0) %>% 
        summarize(post_mspe = mean(Estimate^2))
      
      average_diff = as_sum$att %>% 
        filter(Time >= 0) %>% 
        summarize(average_diff = mean(Estimate))
      
      last_period_diff = as_sum$att %>% 
        filter(Time == max(Time)) %>% 
        select(last_period_diff = Estimate)
      
      cbind(pre_mspe, post_mspe, average_diff, last_period_diff)
    }
    
    as_results <- as_sum %>% 
      map_dfr(get_augsynth_metrics, .id = "unit_name") 
    
    as_results <- as_results %>% 
      mutate(mspe_ratio = post_mspe/pre_mspe,
             mspe_rank = rank(mspe_ratio), 
             average_rank = rank(average_diff), 
             last_period_rank = rank(last_period_diff), 
             type = ifelse(unit_name == "OH", "Treated", "Donor"), 
             fishers_exact_pvalue = mspe_rank / n()) %>% 
      left_join(weights, by = "unit_name") %>% 
      mutate(model_num = model_num)
    
    # write the full augsynth output to disk
    saveRDS(augsynth_out, paste0(output, "/model_", model_num, ".rds"))
    # function returns selected metrics in memory
    return(as_results)
  }
}


# Run Multiverse Models ---------------------------------------------------

# use furrr package to leverage multiple cores, may need to
# adjust for your machine
plan(multisession, workers = availableCores())

multiverse_output <- multiverse_spec %>%
  future_pmap(., possibly(multiverse_run, otherwise = "error"), data = analysisdat,
              .options = furrr_options(seed = TRUE),
              .progress = T)

names(multiverse_output) <- multiverse_spec$model_num

# Note on output: weight column captures each state's weight in the OH (true)
# synthetic control model. All other columns hold the specific states output from
# the permutation test
saveRDS(multiverse_output, "output-multiverse/multiverse_output.RDS")

saveRDS(multiverse_spec, "output-multiverse/multiverse_spec.RDS")