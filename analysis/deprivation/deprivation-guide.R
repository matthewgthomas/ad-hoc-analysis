library(tidyverse)
library(geographr)
library(IMD)

# library(sf)

# ---- Analyse income and employment deprivation ----
imd_income <-
  imd2025_england_lsoa21 |>
  select(lsoa21_code, IMD_decile) |>
  left_join(
    imd2025_england_lsoa21_indicators |>
      select(lsoa21_code, income_domain_numerator)
  ) |>
  left_join(
    lookup_lsoa21_ward24_ltla24 |>
      select(lsoa21_code, lad_code = ltla24_code, lad_name = ltla24_name)
  ) |>
  left_join(
    lookup_ltla24_region24 |>
      select(lad_code = ltla24_code, region_name = region24_name)
  ) |>
  rename(lsoa_code = lsoa21_code)

imd_income |> 
  mutate(Core20 = if_else(IMD_decile <= 2, "20% most deprived", "Less-deprived areas")) |>
  group_by(Core20) |>
  summarise(n = sum(income_domain_numerator, na.rm = TRUE)) |> 
  mutate(prop = n / sum(n))

# Wrangle and save for Flourish
imd_income |>
  mutate(Core20 = if_else(IMD_decile <= 2, "20% most deprived", "Less-deprived areas")) |>
  group_by(region_name, Core20) |>
  summarise(income_domain_numerator = sum(income_domain_numerator, na.rm = TRUE)) |> 
  pivot_wider(names_from = region_name, values_from = income_domain_numerator) |> 
  write_csv("analysis/deprivation/regional-income-deprivation.csv")

# ---- Check Community Needs Index in Hastings neighbourhoods ----
# Broomgrove Community Centre is in LSOA Hastings 005A (E01020972)
# Source: https://www.doogal.co.uk/ShowMap?postcode=TN34+3PY
broomgrove <- "E01020972"

# Grumpy Cook is in LSOA Hastings 009B (E01020979)
# Source: https://www.doogal.co.uk/ShowMap?postcode=TN34+1HL
grumpy_cook <- "E01020979"

imd2025_england_lsoa21 |> 
  filter(lsoa21_code %in% c(broomgrove, grumpy_cook)) |> 
  View()

# Calculate deciles from ranks
cni2023_england_lsoa21 <-  cni2023_england_lsoa21 |> 
  mutate(
    CNI_decile = ntile(`Community Needs Index 2023 Rank`, n = 10),
    Assets_decile = ntile(`Civic Assets Domain Rank`, n = 10),
    Connectedness_decile = ntile(`Connectedness Domain Rank`, n = 10),
    Engaged_decile = ntile(`Active and Engaged Community Domain Rank`, n = 10)
  )

cni2023_england_lsoa21 |> 
  filter(lsoa21_code %in% c(broomgrove, grumpy_cook)) |> 
  View()

# Check historical deprivation (their LSOA codes haven't changed)
imd2019_england_lsoa11 |> 
  filter(lsoa11_code %in% c(broomgrove, grumpy_cook)) |> 
  select(IMD_decile, IMD_rank)

imd2015_england_lsoa11 |> 
  filter(lsoa11_code %in% c(broomgrove, grumpy_cook)) |> 
  select(IMD_decile, IMD_rank)

imd2010_england_lsoa01 |> 
  filter(lsoa01_code %in% c(broomgrove, grumpy_cook)) |> 
  select(IMD_decile, IMD_rank)

imd2004_england_lsoa01 |> 
  filter(lsoa01_code %in% c(broomgrove, grumpy_cook)) |> 
  select(IMD_decile, IMD_rank)
