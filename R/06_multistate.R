library(tidyverse)
library(lubridate)
library(here)
library(gghighlight)
library(augsynth)
library(fixest)

# announce data for all states
raw_announce_dates <- read_csv("data-raw/lottery_announce_dates.csv")



announce_dates <- raw_announce_dates %>% 
  mutate(state_announce_date = mdy(lottery_announce_date), 
         state_announce_week = isoweek(state_announce_date),
         state = str_trim(state)) %>% 
  select(state, state_announce_date, state_announce_week) 


dat <- readRDS(here("data/weekly_data_2021-08-18.rds")) %>% 
  tidylog::left_join(announce_dates) %>% 
  mutate(
    centered_week = week - state_announce_week,
    post_announce = if_else(is.na(state_announce_week), 0, 
                            as.numeric(week > state_announce_week))) 


# Double checking the treatment states and first week of treatment (always week after announcement)
dat %>% filter(post_announce == 1) %>% 
  group_by(state) %>% 
  filter(last_day == min(last_day)) %>% 
  select(state, fips, week, state_announce_week) %>% 
  arrange(state_announce_week)

# descriptive plot
ggplot(dat, aes(x = last_day, 
                y = people_fully_vaccinated_per_hundred, 
                group = state)) +
  geom_line() + 
  gghighlight(post_announce == 1, use_direct_label = TRUE, 
              label_params = list(
                nudge_x = 30, 
                min.segment.length = 0, 
                seed = 342)) +
  scale_x_date(expand = expansion(add = c(0, 50))) +
  labs(
    color = "States with Lotteries",
    title = "Vaccination Rates by State by Week",
    subtitle = "Highlighted post lottery announcement weeks",
    x = "Date",
    y = "Percent Fully Vaccinated"
  ) +
  theme_minimal()

ggsave(here("figures/vax_byannounce_bystate.jpg"))



# augsynth model ----------------------------------------------------------

ppool_syn_full <- multisynth(people_fully_vaccinated_per_hundred ~ post_announce, 
                        state, week, n_leads = max(dat$centered_week, na.rm = T),
                        dat)
# nu is set to default, which is a heuristic balance between completely separate 
# comparisons and a fully pooled model. In this case it comes out to 0.27. 
print(ppool_syn_full$nu)
(ppool_syn_full_summ <- summary(ppool_syn))

plot(ppool_syn_summ) + 
  labs(y = "Difference in Percent Fully Vaccinated", 
       title = "Multistate Augmented Synthetic Control (Fully Vaccinated)",
       subtitle = "Difference between Treated States and Synthetic Comparisons")
ggsave(here("figures/multisynth_full_vax.jpg"))



ppool_syn_initial_dose <- multisynth(people_vaccinated_per_hundred ~ post_announce, 
                        state, week, n_leads = max(dat$centered_week, na.rm = T),
                        dat)
# nu is set to default, which is a heuristic balance between completely separate 
# comparisons and a fully pooled model. In this case it comes out to 0.27. 
print(ppool_syn_initial_dose$nu)
(ppool_syn_initial_dose_summ <- summary(ppool_syn_initial_dose))

plot(ppool_syn_initial_dose_summ) + 
  labs(y = "Difference in  Percent First Doses Doses", 
       title = "Multistate Augmented Synthetic Control (First Dose)",
       subtitle = "Difference between Treated States and Synthetic Comparisons")
ggsave(here("figures/multisynth_initial_dose.jpg"))




ppool_syn_total_dose <- multisynth(total_vaccinations_per_hundred ~ post_announce, 
                                     state, week, n_leads = max(dat$centered_week, na.rm = T),
                                     dat)
# nu is set to default, which is a heuristic balance between completely separate 
# comparisons and a fully pooled model. In this case it comes out to 0.27. 
print(ppool_syn_total_dose$nu)
(ppool_syn_total_dose_summ <- summary(ppool_syn_total_dose))

plot(ppool_syn_total_dose_summ) + 
  labs(y = "Difference in Total Doses per Hundred", 
       title = "Multistate Augmented Synthetic Control (Total Doses)",
       subtitle = "Difference between Treated States and Synthetic Comparisons")
ggsave(here("figures/multisynth_all.jpg"))


ppool_syn_hundred <- multisynth(total_vaccinations_per_hundred ~ post_announce, 
                                state, week, n_leads = max(dat$centered_week, na.rm = T),
                                dat)
# nu is set to default, which is a heuristic balance between completely separate 
# comparisons and a fully pooled model. In this case it comes out to 0.27. 
print(ppool_syn$nu)
(ppool_syn_summ <- summary(ppool_syn))

plot(ppool_syn_summ) + 
  labs(y = "Initial Doese", 
       title = "Multistate Augmented Synthetic Control",
       subtitle = "Difference between Treated States and Synthetic Comparisons")
ggsave(here("figures/multisynth_total_doses.jpg"))






# basic two-way fe --------------------------------------------------------

setFixest_dict(c(people_fully_vaccinated_per_hundred = "Vaccination Rate",
                 daily_vaccinations_per_million = "Weekly Vaccinations (per million)",
                 post_announce = "Average Treatment Effect", 
                 treat = "Lottery State", 
                 centered_week = "Relative Week"))

dat <- replace_na(dat, list(centered_week = 0)) %>% 
  mutate(treat = !is.na(state_announce_week))

avg_est <- feols(c(people_fully_vaccinated_per_hundred, daily_vaccinations_per_million) ~ 
        post_announce  
      | week + state, 
      cluster = ~state, 
      data = dat) 

dynamic_est <- feols(c(people_fully_vaccinated_per_hundred, daily_vaccinations_per_million) ~ 
        i(centered_week, treat, ref = 0)  
        | week + state, 
      cluster = ~state, 
      data = dat) 

etable(avg_est[1], dynamic_est[1], avg_est[2], dynamic_est[2],
       digits = 2, digits.stats = 2, 
       drop = c(as.character(5:14), as.character(-24:-5)),
       label = "tab:ddest",
       title = "Difference in Difference Estimates",
       tex = TRUE,
       notes = "Weekly effect estimates relative to the announcement week. Effects for relative weeks less than -4 or greater than 4 omitted from table for legibility. Models include state clustered standard errors. Models include data from 01/12/2021 to 08/18/2021")
  


