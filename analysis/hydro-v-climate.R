source("load.R")

hydro_climate_observations <- hydro_observations %>%
  left_join(
    climate_observations %>%
      select(date_time, temp_c:wind_chill),
    by = c("time" = "date_time")
  )
