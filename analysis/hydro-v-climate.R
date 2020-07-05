source("load.R")

hydro_observations %>%
  left_join(climate_observations %>% select(date_time, ))
