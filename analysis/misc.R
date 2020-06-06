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
  summarize(usage = sum(consumption_k_wh)) %>%
  ggplot(aes(x = ymonth, y = usage)) +
  geom_line()



hydro_observations %>%
  index_by(dweek = ~ wday(., week_start = 1, label = TRUE)) %>%
  ggplot(aes(x = dweek, y = consumption_k_wh)) +
  geom_jitter()

hydro_observations %>%
  mutate(
    during_working_hours = hour(time) >= 8 & hour(time) <= 17,
    during_wkday = wday(time, week_start = 1) < 6,
  ) %>%
  ggplot(aes(x = time, y = consumption_k_wh, color = during_wkday)) +
  geom_point() +
  geom_smooth() +
  scale_x_datetime(date_breaks = "3 months", date_labels = "%b '%y", date_minor_breaks = "1 months") +
  facet_grid(rows = vars(during_wkday))

