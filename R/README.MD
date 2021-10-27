File Info 
* 00_libraries.R -> list of libraries used in this package, see session info for exact version numbers
* 01_get-data.R -> code to download vaccination data from Our World in Data, writes to `data-raw/`
* 02_clean-data.R -> code to clean `data-raw/` files and create analysis files, writes to `data/`
* 03_descriptives.R -> Creates descriptive plot of vax rates
* 04_synth_analysis.R -> Original pre-registered synthetic control analysis with all 50 states
* 04b_synth_analysis_excluded_states.R -> Modified pre-registration excluding other lottery states
* 04c_synth_firstdose_excluded_states.R -> Synthetic control on first dose outcome
* 05_power_analysis.Rmd -> power analysis for original pre-registration
* 05b_power_analysis_all_states.Rmd -> power analysis excluding additional lottery adopting states
* 06_multistate.R -> multi state synthetic control analysis 
* 07a_multiverse_run.R -> multiverse analysis specification and run (can take multiple hours depending on your hardware!), writes to `output-multiverse/`
* 07b_multiverse_analysis.R -> analysis of multiverse output
* 08_bayes_factor.R -> bayes factor analysis as part of peer review, not included in paper
* 09_firstdose_compare_bw_s.R -> comparison of first dose estimate models with Barber & West 2021 and Sehgal 2021 
* sessionInfo.txt -> info on R version and package versions. Check here if you have replication issues
