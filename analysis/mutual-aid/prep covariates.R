library(tidyverse)
library(geographr)
library(demographr)
library(nomisr)

demographr::ethnicity21_msoa21


library(asylum)
supp <- asylum::fetch_asylum_support()
supp |>
  filter(Date == max(Date)) |>
  filter(Nationality == "Iran") |>
  group_by(`Support Type`) |>
  summarise(total = sum(People))
