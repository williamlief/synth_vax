# Overview 

Directory contains R-code used to run analyses and obtain data. A brief description of each file is below, followed by the exact location of code to generate each figure and table presented in the paper. 

Each script can be run without running any other scripts. To replicate a specific table or figure, please see below and run from the start of the script until the indicated line to re-generate the output. 

# File Info 
* 00_libraries.R -> list of libraries used in this package, see `R/sessioninfo.txt` for exact version numbers
* 01_get-data.R -> code to download vaccination data from Our World in Data, writes to `data-raw/`
* 02_clean-data.R -> code to clean `data-raw/` files and create analysis files, writes to `data/`
* 03_descriptives.R -> Creates descriptive plot of vax rates
* 04_synth_analysis.R -> Original pre-registered synthetic control analysis with all 50 states
* 04b_synth_analysis_excluded_states.R -> Modified pre-registration excluding other lottery states
* 04c_synth_firstdose_excluded_states.R -> Synthetic control on first dose outcome
* 05_power_analysis.Rmd -> power analysis for original pre-registration. Can take many hours to run. 
* 05b_power_analysis_all_states.Rmd -> power analysis excluding additional lottery adopting states
* 06_multistate.R -> multi state synthetic control analysis 
* 07a_multiverse_run.R -> multiverse analysis specification and run (can take multiple hours depending on your hardware!), writes to `output-multiverse/`
* 07b_multiverse_analysis.R -> analysis of multiverse output
* 08_bayes_factor.R -> bayes factor analysis as part of peer review, not included in paper
* 09_firstdose_compare_bw_s.R -> comparison of first dose estimate models with Barber & West 2021 and Sehgal 2021 
* sessionInfo.txt -> info on R version and package versions. Check here if you have replication issues

# Figures 
* Figure 1: `R/03_descriptives.R`, saved on line 26 to `figures/state_plot_lines.jpg`
* Figure 2: `R/04b_synth_analysis_excluded_states.R`
  * Top Panel saved on line 100 to `figures/ex_lotto_treatment_trends.jpg`
  * Bottom Panel saved on line 157 to `figures/conformal_inference_asynth_ex_lotto.jpg`
* Figure 3: `R/07b_multiverse_analysis.R` saved on line 116 to `figures/multiverse_weights.png`
* Figure 4: `R/07b_multiverse_analysis.R` saved on line 150 to `figures/multiverse_estimates.png`
* Figure 5: `R/04_synth_analysis.R` saved on line 255 to `figures/placebo_difference.jpg`.
* Figure 6: `R/04_synth_analysis.R` saved on line 261 to `figures/placebo_mspe.jpg`.
* Figure 7: `R/05_power_analysis.RMD` saved on line 224 to `figures/alt_effect_size_plot.jpg`
* Figure 8: `R/04_synth_analysis.R` saved on line 147 to `figures/conformal_inference_asynth_including_lotto.jpg`
* Figure 9: `R/09_firstdose_compare_bw_s.R` saved on line 84 to `figures/first_dose_compare.jpg`
* Figure 10: `R/06_multistate.R` saved on line 51 to `figures/vax_byannounce_bystate.jpg`
* Figure 11: `R/06_multistate.R` saved on line 69 to `figures/multisynth_full_vax.jpg`

# Tables
* Table 1: `R/04b_synth_analysis_excluded_states.R`, latex output to console lines 64-70
* Table 2: `R/04b_synth_analysis_excluded_states.R`, latex output to console lines 84-87
* Table 3: `R/04b_synth_analysis_excluded_states.R`, text output to console line 206. Note: exact p-values are not included in the code output, but are calculated as rank divided by the number of states (33).
* Table 4: Modelling decision table, not created by code. `R/07a_multiverse_run.R` line 104 defines object `multiverse_spec` which contains the code definition of these decisions.
* Table 5: `R/05_power_analysis.RMD` tex output to console on line 101
* Table 6: Lottery announcement dates, created through author's review of news articles. Links to news sources can be found in `data-raw/lottery_announce_dates.csv`.
* Table 7: 
  * Including lottery state weights output to console in `R/04_synth_analysis.R` on line 53-59. 
  * Excluding lottery state weights output to console in `R/04b_synth_analysis_excluded_states.R`, on lines 64-70 (Table 1)
  * Combined by hand.
* Table 8:`R/04_synth_analysis.R` output to console lines 68-75
* Table 9: `R/04_synth_analysis.R` output to console on line 196. Note: exact p-values are not included in the code output, but are calculated as rank divided by the number of states (51).

