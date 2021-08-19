library(tidyverse)

multiverse_output <- readRDS("output-multiverse/multiverse_output.RDS")
multiverse_spec <- readRDS("output-multiverse/multiverse_spec.RDS")

multiverse_stack <- multiverse_output %>% 
  bind_rows(.id = "model_num") %>% 
  group_by(model_num) %>% 
  mutate(norm_weight = abs(weight) / sum(abs(weight), na.rm = T)) %>% 
  full_join(multiverse_spec %>% 
              mutate(across(where(is.list), function(x) names(x))))


# check pre-registered model ----------------------------------------------

pre_reg_model <- multiverse_spec %>% 
  filter(method == 'tidysynth', 
         ts_cov_use == "full_path",
         pretreat_start == "2021-01-12",
         post_stop == "2021-06-24", 
         outcome == "people_fully_vaccinated_per_hundred") %>% 
  filter(row_number() == 1) %>%  # this is a hack, having issues subsetting to correct states_to_include
  pull(model_num)

pre_reg <- multiverse_output[[pre_reg_model]]

# this is very close on all measures to the pre-registration.
# Output values are within a few hundreths 
# Weight for WI and HI are off by 0.001
pre_reg %>% filter(unit_name == "OH") %>% t()

registered_weights <- read_csv('output/unit_weights.csv') %>% 
  rename(unit_name = unit, 
         prereg_w = weights)

weight_compare <- pre_reg %>% 
  filter(unit_name != "OH") %>% 
  left_join(registered_weights, by = "unit_name") %>% 
  mutate(dif = weight - prereg_w)

summary(weight_compare$dif)


# Compare Weights Across Multiverse -----------------------------------------

ggplot(data = multiverse_stack %>% 
         filter(unit_name != "OH"),
       aes(x = fct_rev(unit_name), y = norm_weight)) +
  facet_wrap(.~outcome) +
  geom_boxplot() + 
  coord_flip() +
  theme_minimal() +
  labs(title = "Boxplot of State Weights in Synthetic Counterfactuals for Multiverse of Models", 
       x = NULL, y = "Weight normalized by sum of absolute model weights")


# Compare Estimates -------------------------------------------------------
# this is some crude plotting code, need to revise
ggplot(data = multiverse_stack %>% 
         filter(unit_name == "OH"),
       aes(51-mspe_rank, last_period_diff, 
           color = post_stop)) +
  facet_wrap(.~outcome) +
  geom_point() +
  geom_vline(aes(xintercept = 45))

