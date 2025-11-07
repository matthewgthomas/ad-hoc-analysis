library(tidyverse)
library(IMD)

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
