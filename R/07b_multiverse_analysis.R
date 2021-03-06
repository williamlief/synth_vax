library(tidyverse)
library(here)

multiverse_output <- readRDS(here::here("output-multiverse/multiverse_output.RDS"))

multiverse_spec <- readRDS(here::here("output-multiverse/multiverse_spec.RDS")) %>% 
unnest(method,as_progfunc,pretreat_start) %>%
  mutate(method = if_else(as_progfunc == "none" & as_fixedeff == "FALSE", 
                          "Classical SCM", 
                          method))


multiverse_spec %>% count(method)
multiverse_stack <- multiverse_output %>% 
  bind_rows(.id = "model_num") %>% 
  group_by(model_num) %>% 
  mutate(norm_weight = abs(weight) / sum(abs(weight), na.rm = T)) %>% 
  inner_join(multiverse_spec %>% 
               mutate(
                 across(c(pretreat_start, post_stop), function(x) str_remove(unlist(x), "2021-")),
                 across(where(is.list), function(x) names(x))))


model_fit <- multiverse_stack %>% 
  filter(unit_name != "OH") %>% 
  group_by(model_num) %>% 
  summarize(avg_post_mspe = mean(post_mspe))

dat <- multiverse_stack %>% 
  filter(unit_name == "OH") %>% 
  tidylog::left_join(model_fit) %>%
  # Formatting vars for plotting
  mutate(long_time = paste(pretreat_start, post_stop, sep = ": "), 
         covariates = case_when(
           covariates == "annual_demo" ~ "Annual Covariates", 
           covariates == "annual_mobility" ~ "Annual + Mobility", 
           covariates == "none" ~ "None"
         ), 
         covariates = factor(covariates, levels = c(
           "None", 
           "Annual Covariates", 
           "Annual + Mobility"
         )),
         method = case_when(method == "augsynth" ~ "Augsynth", 
                            method == "Classical SCM" ~ "Classical SCM"),
         states_to_include = case_when(
           states_to_include == "full" ~ "All States + DC",
           states_to_include == "no_lottery" ~ "Non-Lottery Adopting"))


# Labelling stuff
outcome.labs <- c("Fully Vaccinated", "First Dose", "Total Doses")
names(outcome.labs) <- c("full_vax", "first_dose", "total_doses")

states.labs <- c("All States + DC", "Non Lottery Adopting States")
names(states.labs) <- c("full", "no_lottery")

# check pre-registered model ----------------------------------------------
# Here we check the pre-registered 50 state model to confirm that the multiverse
# code is running as intended

pre_reg_model <- multiverse_spec %>% 
  filter(method == 'Classical SCM', 
         
         pretreat_start == "2021-01-12",
         post_stop == "2021-06-24", 
         outcome == "people_fully_vaccinated_per_hundred",
         covariates=="NULL",
         #str_detect(as.character(states_to_include),"CA"),negate=TRUE,
         map_dbl(states_to_include,length)==33,
         )  %>% pull(model_num)
  

pre_reg_model
pre_reg <- dat %>% filter(model_num == pre_reg_model)

# this is very close on all measures to the pre-registration.
# Output values are within a few hundreths 
# Weight for WI and HI are off by 0.001
t(pre_reg)

registered_weights <- read_csv('output/unit_weights.csv') %>% 
  rename(unit_name = unit, 
         prereg_w = weights)

weight_compare <- multiverse_output[[pre_reg_model]] %>% 
  filter(unit_name != "OH") %>% 
  left_join(registered_weights, by = "unit_name") %>% 
  mutate(dif = weight - prereg_w)

summary(weight_compare$dif)




# Compare Weights Across Multiverse -----------------------------------------

ggplot(data = multiverse_stack %>% 
         filter(unit_name != "OH") %>% 
         filter(model_num %in% dat$model_num) %>% 
         # filter(outcome == !!outcome) %>% 
         select(-post_stop) %>% # post period doesnt impact weights, so all weights are triplicated
         distinct(),
       aes(x = fct_rev(unit_name), y = norm_weight)) +
  facet_wrap(states_to_include~outcome,
             scales = "free_y",
             labeller = labeller(outcome = outcome.labs,
                                 states_to_include = states.labs)) +
  geom_boxplot() + 
  coord_flip() +
  theme_bw() +
  theme(axis.text.y = element_text(size = 6)) +
  labs(title = "Boxplot of State Weights in Synthetic Counterfactuals for Multiverse of Models", 
       x = NULL, y = "Weight normalized by sum of absolute model weights")

ggsave("figures/multiverse_weights.png", width = 20, height = 11)

# Compare Estimates -------------------------------------------------------


ggplot(data  = dat %>% filter(avg_post_mspe < 50), 
       aes(long_time, last_period_diff, 
           color = covariates
           #alpha = avg_post_mspe
           )) +
  geom_point(size=2.5) +
  # highlight prereg model
  geom_point(data = dat %>% filter(model_num == pre_reg_model), 
             size = 4.5, shape = 8,stroke=1.5,show.legend = FALSE) +
  # highlight lowest mspe models
  geom_point(data = dat %>% group_by(outcome) %>% 
               filter(avg_post_mspe == min(avg_post_mspe)), 
             size = 4.5, shape = 4,stroke=1.5,show.legend = FALSE) +
  facet_grid(outcome~method*states_to_include, 
             labeller = labeller(outcome = outcome.labs)) +
  geom_hline(yintercept = 0) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Multiverse Estimates for Final Difference in Outcomes between Ohio and Synthetic Comparison",
      # subtitle = "Models with higher post period MSPE in non-treated states have lower alpha values",
       x = NULL,
       y = "Estimated Difference in Outcome in Final Time Period",
       color = "Covariate Adjustment",
       caption = "Pre-registered model indicated with *, best fitting models indicated with X") +
  scale_color_brewer(palette="Dark2") +
  scale_alpha(range = c(1, 0.1), guide = "none")
  


ggsave("figures/multiverse_estimates.png", width = 20, height = 11)


# MSPE stats --------------------------------------------------------------

summary(dat$avg_post_mspe)
dat %>% filter(model_num == pre_reg_model) %>% pull(avg_post_mspe)




dat %>% group_by(outcome) %>% 
  filter(avg_post_mspe == min(avg_post_mspe)) %>% 
  t()         

### Last Period Diff Stats
dat %>% group_by(outcome) %>% 
  summarise(mean(last_period_diff<0) ) %>%
  t()         


dat %>% group_by(outcome) %>% 
  summarise(sum(last_period_diff<0) ) %>%
  t()         

model_fit %>%mutate(percent_rank(avg_post_mspe)) %>% arrange(-avg_post_mspe) %>% filter(model_num==163)

model_fit %>% summarise(mean(avg_post_mspe))

model_fit %>% summarise(median(avg_post_mspe))

multiverse_spec


multiverse_stack %>% filter(model_num=="163") %>% arrange(fishers_exact_pvalue
                                                          )



