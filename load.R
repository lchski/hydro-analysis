library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(helpers)

hydro_observations <- fs::dir_ls("data/source/hydroottawa.com/", regexp = "*.xls") %>%
  map_dfr(read_excel, skip = 2, .id = "source_file") %>%
  clean_names() %>%
  arrange(time) %>%
  mutate(date = as_date(date)) %>%
  distinct(date, time, rate_type, consumption_k_wh, cost) %>%
  mutate(year = year(date), month = month(date), wday = wday(date, week_start = 1, label = TRUE))
