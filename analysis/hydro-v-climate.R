source("load.R")

hydro_climate_observations <- hydro_observations %>%
  left_join(
    climate_observations %>%
      select(date_time, temp_c:wind_chill),
    by = c("time" = "date_time")
  ) %>%
  mutate(c19_lockdown = time >= "2020-03-16")

hydro_climate_observations %>%
  mutate(date = date(time)) %>%
  index_by(date) %>%
  summarize(consumption_k_wh = sum(consumption_k_wh)) %>%
  gg_season(consumption_k_wh)

hydro_climate_observations %>%
  mutate(ma_consumption_k_wh = slide_dbl(consumption_k_wh, mean, .size = 31 * 24 + 1, .align = "center")) %>%
  gg_season(ma_consumption_k_wh)

hydro_climate_observations %>%
  mutate(date = date(time)) %>%
  index_by(date) %>%
  summarize(consumption_k_wh = sum(consumption_k_wh)) %>%
  mutate(ma_consumption_k_wh = slide_dbl(consumption_k_wh, mean, .size = 7, .align = "center")) %>%
  gg_season(ma_consumption_k_wh)


hydro_climate_observations %>%
  mutate(date = date(time)) %>%
  index_by(date) %>%
  summarize(consumption_k_wh = sum(consumption_k_wh)) %>%
  gg_lag(consumption_k_wh)

hydro_climate_observations %>%
  mutate(date = date(time)) %>%
  index_by(date) %>%
  summarize(consumption_k_wh = sum(consumption_k_wh, na.rm = TRUE), temp_c = mean(temp_c, na.rm = TRUE)) %>%
  mutate(
    ma_consumption_k_wh = slide_dbl(consumption_k_wh, mean, .size = 7, .align = "center", na.rm = TRUE),
    ma_temp_c = slide_dbl(temp_c, mean, .size = 7, .align = "center", na.rm = TRUE)
  )

hydro_climate_observations %>%
  mutate(date = date(time)) %>%
  index_by(date) %>%
  summarize(consumption_k_wh = sum(consumption_k_wh, na.rm = TRUE), temp_c = mean(temp_c, na.rm = TRUE)) %>%
  mutate(
    ma_consumption_k_wh = slide_dbl(consumption_k_wh, mean, .size = 7, .align = "center", na.rm = TRUE),
    ma_temp_c = slide_dbl(temp_c, mean, .size = 7, .align = "center", na.rm = TRUE)
  ) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = consumption_k_wh, colour = "kw/h")) +
  geom_line(aes(y = temp_c, colour = "degC"))

hydro_climate_observations %>%
  mutate(date = date(time)) %>%
  index_by(date) %>%
  summarize(consumption_k_wh = sum(consumption_k_wh, na.rm = TRUE), temp_c = mean(temp_c, na.rm = TRUE)) %>%
  left_join(climate_indicator_by_day) %>%
  mutate(c19_lockdown = date >= "2020-03-16") %>%
  mutate(is_summer_month = month(date) %in% c(6, 7, 8)) %>%
  mutate(
    ma_consumption_k_wh = slide_dbl(consumption_k_wh, mean, .size = 7, .align = "center", na.rm = TRUE),
    ma_temp_c = slide_dbl(temp_c, mean, .size = 7, .align = "center", na.rm = TRUE),
    ma_climate_indicator = slide_dbl(climate_indicator, mean, .size = 7, .align = "center", na.rm = TRUE)
  ) %>%
  ggplot(aes(x = ma_climate_indicator, y = consumption_k_wh)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  facet_wrap(vars(c19_lockdown, is_summer_month))

climate_indicator_by_day <- hydro_climate_observations %>%
  mutate(date = date(time)) %>%
  index_by(date) %>%
  top_n(6, temp_c) %>%
  summarize(climate_indicator = mean(temp_c, na.rm = TRUE))

hmdx_indicators_by_day <- hydro_climate_observations %>%
  mutate(date = date(time)) %>%
  index_by(date) %>%
  select(temp_c, hmdx) %>%
  filter(! is.na(hmdx)) %>%
  mutate(hmdx_diff = hmdx - temp_c) %>%
  summarize(hmdx_hrs = n(), avg_hmdx_diff = round(mean(hmdx_diff), 1))

hydro_climate_observations %>%
  mutate(date = date(time)) %>%
  index_by(date) %>%
  summarize(consumption_k_wh = sum(consumption_k_wh, na.rm = TRUE), temp_c = mean(temp_c, na.rm = TRUE)) %>%
  left_join(climate_indicator_by_day) %>%
  left_join(hmdx_indicators_by_day) %>%
  mutate(hmdx_hrs = ifelse(is.na(hmdx_hrs), 0, hmdx_hrs)) %>%
  mutate(avg_hmdx_diff = ifelse(is.na(avg_hmdx_diff), 0, avg_hmdx_diff)) %>%
  mutate(c19_lockdown = date >= "2020-03-16") %>%
  mutate(is_summer_month = month(date) %in% c(6, 7, 8)) %>%
  mutate(year = year(date) - 2016) %>%
  mutate(away_from_apt = date %within% list(
    "2020-12-19" %--% "2020-01-02",
    "2020-06-26" %--% "2020-07-17"
  )) %>%
  mutate(
    ma_consumption_k_wh = slide_dbl(consumption_k_wh, mean, .size = 7, .align = "center", na.rm = TRUE),
    ma_temp_c = slide_dbl(temp_c, mean, .size = 7, .align = "center", na.rm = TRUE),
    ma_climate_indicator = slide_dbl(climate_indicator, mean, .size = 7, .align = "center", na.rm = TRUE)
  ) %>%
  model(TSLM(consumption_k_wh ~ year + climate_indicator + hmdx_hrs:avg_hmdx_diff + is_summer_month + away_from_apt + c19_lockdown)) %>%
  report()
  


