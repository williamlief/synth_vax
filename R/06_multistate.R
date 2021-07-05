library(tidyverse)
library(lubridate)
library(here)
library(gghighlight)
library(bacondecomp)
library(augsynth)
library(fixest)

# announce data for all states
raw_announce_dates <- read_csv("data-raw/lottery_announce_dates.csv")

announce_dates <- raw_announce_dates %>% 
  mutate(state_announce_date = mdy(lottery_announce_date), 
         state_announce_week = isoweek(state_announce_date),
         state = str_trim(state)) %>% 
  select(state, state_announce_date, state_announce_week) 


dat <- readRDS(here("data/weekly_data_2021-07-04.rds")) %>% 
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

ppool_syn <- multisynth(people_fully_vaccinated_per_hundred ~ post_announce, 
                        state, week, n_leads = max(dat$centered_week, na.rm = T),
                        dat)
ppool_syn_summ <- summary(ppool_syn)

plot(ppool_syn_summ) + 
  labs(y = "Difference in Synthetic and Observed", 
       title = "Multistate Augmented Synthetic Control")
ggsave(here("figures/multisynth_avg.jpg"))

plot(ppool_syn_summ, levels = "Average")  + 
  labs(y = "Difference in Synthetic and Observed", 
       title = "Multistate Augmented Synthetic Control")
ggsave(here("figures/multisynth_avg.jpg"))


ppool_syn_time <- multisynth(people_fully_vaccinated_per_hundred ~ post_announce, 
                             state, week, n_leads = max(dat$centered_week, na.rm = T),
                             dat, nu = 0,
                             time_cohort = TRUE)

ppool_syn_time_summ <- summary(ppool_syn_time)
ppool_syn_time_summ

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
       drop = c(as.character(5:7), as.character(-24:-5)),
       label = "tab:ddest",
       title = "Difference in Difference Estimates",
       tex = TRUE,
       notes = "Effects for relative weeks less than -4 or greater than 4 omitted from table for legibility. Models include state clustered standard errors.")
  

# Bacon Decomp ------------------------------------------------------------

df_bacon <- bacon(people_fully_vaccinated_per_hundred ~ post_announce,
                  data = dat,
                  id_var = "state",
                  time_var = "week")

dd_estimate <- sum(df_bacon$estimate*df_bacon$weight)


ggplot(data = df_bacon) +
  geom_point(aes(x = weight, y = estimate, 
                 color = type, shape = type), size = 2) +
  xlab("Weight") +
  ylab("2x2 DD Estimate") +
  geom_hline(yintercept = dd_estimate, color = "red") +
  theme_minimal() + 
  theme(
    legend.title = element_blank(),
    legend.background = element_rect(
      fill="white", linetype="solid"),
    legend.justification=c(1,1), 
    legend.position=c(1,1)
  )

ggsave(here("figures/bacon_ddplot.jpg"))