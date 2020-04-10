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
