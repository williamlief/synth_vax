# Multiverse of Models 

Model summary statistics are saved in `multiverse_output.RDS`. This file contains a list of `data.frames`. Each dataframe is named for the corresponding model specification found in `multiverse_spec.RDS`. Refer to `multiverse_spec.RDS` for the exact details of each model run. 

Each `data.frame` with `multiverse_output.RDS` contains one row of output from a permutation test that used each state in turn as the treated state. The `weight` variable however contains each donor states (i.e not Ohio) weight in the synthetic counterfactual to Ohio. The permuted weights are not saved. 

Full model output is not saved. However, if you run `07_multiverse_analysis.R` the code will write the full model output to this location in a series of files titled `model_{model_num}.RDS`