library(skimr)

source("load.R")

hydro_observations %>%
  group_by(year, wday) %>%
  skim(consumption_k_wh)

hydro_observations %>%
  group_by(rate_type) %>%
  summarize(usage = sum(consumption_k_wh), cost = sum(cost))

hydro_observations %>%
  group_by(wday) %>%
  summarize(usage = sum(consumption_k_wh), cost = sum(cost))

hydro_observations %>%
  mutate(month = floor_date(date, "months")) %>%
  group_by(month) %>%
  summarize(usage = sum(consumption_k_wh)) %>%
  filter(month < "2020-04-01") %>%
  ggplot(aes(x = month, y = usage)) +
  geom_line() +
  geom_smooth() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b '%y")

hydro_observations %>%
  filter(hour(time) > 8) %>%
  filter(hour(time) <= 18) %>%
  mutate(month = floor_date(date, "months")) %>%
  group_by(month) %>%
  summarize(usage = sum(consumption_k_wh)) %>%
  filter(month < "2020-04-01") %>%
  ggplot(aes(x = month, y = usage)) +
  geom_line() +
  geom_smooth() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b '%y")

hydro_observations %>%
  filter(rate_type == "On-Peak") %>%
  mutate(month = floor_date(date, "months")) %>%
  group_by(month) %>%
  summarize(usage = sum(consumption_k_wh)) %>%
  filter(month < "2020-04-01") %>%
  ggplot(aes(x = month, y = usage)) +
  geom_line() +
  geom_smooth() +
  scale_x_date(date_breaks = "6 months", date_labels = "%b '%y")

hydro_observations %>%
  mutate(usage_bin = cut(consumption_k_wh, breaks = 5)) %>%
  mutate(hr = hour(time)) %>%
  count_group(usage_bin, hr)











hydro_observations %>%
  index_by(ymonth = ~ yearmonth(.)) %>%
  group_by(rate_type) %>%
  summarize(usage = sum(consumption_k_wh)) %>%
  ggplot(aes(x = ymonth, y = usage, color = rate_type)) +
  geom_line()

hydro_observations %>%
  index_by(ymonth = ~ yearmonth(.)) %>%
  summarize(usage = sum(consumption_k_wh)) %>%
  ggplot(aes(x = ymonth, y = usage)) +
  geom_line()



hydro_observations %>%
  index_by(dweek = ~ wday(., week_start = 1, label = TRUE)) %>%
  ggplot(aes(x = dweek, y = consumption_k_wh)) +
  geom_jitter()

hydro_observations %>%
  index_by(dweek = ~ wday(., week_start = 1, label = TRUE)) %>%
  mutate(date = yearmonth(time)) %>%
  ggplot(aes(x = date, y = consumption_k_wh)) +
  geom_jitter() +
  geom_smooth() +
  facet_grid(rows = vars(dweek))

hydro_observations %>%
  index_by(dweek = ~ wday(., week_start = 1, label = TRUE)) %>%
  mutate(ymonth = yearmonth(time)) %>%
  group_by(ymonth) %>%
  summarize(consumption_k_wh = sum(consumption_k_wh), days_included = n() / 24) %>%
  filter(days_included > 3) %>%
  ggplot(aes(x = ymonth, y= consumption_k_wh)) +
  geom_point() +
  geom_smooth() +
  facet_grid(rows = vars(dweek))

hydro_observations %>%
  mutate(
    during_working_hours = hour(time) >= 8 & hour(time) <= 17,
    during_wkday = wday(time, week_start = 1) < 6,
    ymonth = yearmonth(time)
  ) %>%
  ggplot(aes(x = time, y = consumption_k_wh, color = during_wkday)) +
  geom_point() +
  geom_smooth() +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%b '%y", date_minor_breaks = "1 months") +
  facet_grid(rows = vars(during_wkday))

## some experiments, but eh
hydro_observations %>%
  index_by(ymonth = ~ yearmonth(.)) %>%
  mutate(
    during_working_hours = hour(time) >= 8 & hour(time) <= 17,
    during_wkday = wday(time, week_start = 1) < 6,
  ) %>%
  group_by(during_wkday, during_working_hours) %>%
  summarize(consumption_k_wh = sum(consumption_k_wh)) %>%
  ggplot(aes(x = ymonth, y = consumption_k_wh, color = during_working_hours)) +
  geom_point()
  

hydro_observations %>%
  index_by(month = ~ month(.)) %>%
  mutate(is_summer_month = month(time) >= 5 & month(time) <= 9) %>%
  filter(is_summer_month) %>%
  ggplot(aes(x = time, y = consumption_k_wh, color = is_summer_month)) +
  geom_point() +
  geom_smooth()

hydro_observations %>%
  index_by(month = ~ month(.)) %>%
  mutate(is_summer_month = month(time) >= 5 & month(time) <= 9) %>%
  filter(is_summer_month) %>%
  ggplot(aes(x = time, y = consumption_k_wh, color = year(time))) +
  geom_point()

hydro_observations %>%
  index_by(month = ~ month(.)) %>%
  mutate(is_summer_month = month(time) >= 5 & month(time) <= 9) %>%
  mutate(consumption_k_wh = ifelse(is_summer_month, consumption_k_wh, NA_real_)) %>%
  gg_season(consumption_k_wh, period = "month")

hydro_observations %>%
  index_by(day = ~ year(.) + yday(.)) %>%
  summarize(consumption_k_wh = sum(consumption_k_wh))
  mutate(is_summer_month = month(time) >= 5 & month(time) <= 9) %>%
  mutate(consumption_k_wh = ifelse(is_summer_month, consumption_k_wh, NA_real_)) %>%
  gg_season(consumption_k_wh, period = "month")
  

