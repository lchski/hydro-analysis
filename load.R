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
  as_tsibble(index = time, validate = FALSE)
