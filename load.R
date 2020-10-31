library(tidyverse)
library(readxl)
library(janitor)

library(lubridate)
library(tsibble)
library(feasts)
library(fable)

library(helpers)

hydro_observations <- fs::dir_ls("data/source/hydroottawa.com/", regexp = "*.xls") %>%
  map_dfr(read_excel, skip = 2, .id = "source_file") %>%
  clean_names() %>%
  arrange(time) %>%
  distinct(time, rate_type, consumption_k_wh, cost) %>%
  as_tsibble(key = NULL, index = time, validate = FALSE)

climate_observations <- fs::dir_ls("data/source/climate.weather.gc.ca/", regexp = "*.csv") %>%
  map_dfr(read_csv, .id = "source_file", col_types = cols(
    .default = col_double(),
    `Station Name` = col_character(),
    `Date/Time` = col_datetime(format = ""),
    Year = col_number(),
    Month = col_number(),
    Day = col_number(),
    Time = col_time(format = ""),
    `Temp Flag` = col_character(),
    `Dew Point Temp Flag` = col_character(),
    `Rel Hum Flag` = col_character(),
    `Wind Dir Flag` = col_character(),
    `Wind Spd Flag` = col_character(),
    `Visibility Flag` = col_character(),
    `Stn Press Flag` = col_character(),
    `Hmdx Flag` = col_character(),
    `Wind Chill` = col_double(),
    `Wind Chill Flag` = col_character(),
    Weather = col_character()
  )) %>%
  clean_names() %>%
  remove_extra_columns() %>%
  select(-source_file) %>%
  arrange(date_time) %>%
  as_tsibble(key = NULL, index = date_time) %>%
  filter(date_time >= (hydro_observations %>% slice(1) %>% pull(time))) %>% ## remove climate data before hydro data starts
  filter(date_time <= (hydro_observations %>% slice(n()) %>% pull(time))) ## remove climate data after data ends


tou_rates <- read_tsv("data/source/oeb.ca/tou-rates.tsv", skip = 1) %>%
  clean_names %>%
  mutate(effective_date = mdy(effective_date))

tiered_rates <- read_tsv("data/source/oeb.ca/tiered-rates.tsv", skip = 1) %>%
  clean_names %>%
  mutate(effective_date = mdy(effective_date))
