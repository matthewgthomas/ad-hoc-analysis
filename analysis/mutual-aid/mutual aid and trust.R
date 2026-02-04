library(tidyverse)
library(jsonlite)
library(geographr)
library(rio)
library(sf)

# ---- Load mutual aid groups in the UK ----
# Mutual aid groups (example: from a JSON/CSV export you create from Mutual Aid Wiki)
# Source: https://github.com/Covid-Mutual-Aid/mutual-aid-wiki
# mutual_aid_json <- "https://github.com/Covid-Mutual-Aid/mutual-aid-wiki/raw/refs/heads/master/data/groups.json"
mutual_aid_json <- "analysis/mutual-aid/data/groups.json"

mutual_aid <- read_json(mutual_aid_json, simplifyVector = TRUE)

mutual_aid <- mutual_aid |>
  as_tibble() |>
  bind_cols(mutual_aid$location_coord)

# Convert to sf points
mutual_aid_sf <- st_as_sf(mutual_aid, coords = c("lng", "lat"), crs = 4326)

# Lookup which MSOA each mutual aid group is in
# then count the number of mutual aid groups in each MSOA
mutual_aid_msoa <- st_join(
  mutual_aid_sf,
  boundaries_msoa11,
  join = st_within
) |>
  st_drop_geometry() |>
  count(msoa11_code, name = "n_groups") |>
  mutate(any_group = n_groups > 0) |>
  filter(!is.na(msoa11_code))

# List the presence and number of mutual aid groups in each MSOA (including those with none)
mutual_aid_msoa_all <- boundaries_msoa11 |>
  st_drop_geometry() |>
  left_join(mutual_aid_msoa) |>
  mutate(
    n_groups = replace_na(n_groups, 0),
    any_group = replace_na(any_group, FALSE)
  )

# ---- Load MSOA-level trust data ----
# Source: https://github.com/ukonward/good_neighbours
# trust_url <- "https://github.com/ukonward/good_neighbours/raw/refs/heads/main/good_neighbours_full_data_by_msoa.xlsx"
trust_url <- "analysis/mutual-aid/data/good_neighbours_full_data_by_msoa.xlsx"

trust <- import(trust_url)

mutual_aid_trust <- mutual_aid_msoa_all |>
  left_join(trust, by = join_by(msoa11_code == MSOA_code)) |>
  filter(!is.na(Net_trust))

# ---- Analysis ----
# Do MSOAs with mutual aid groups have higher net trust?
mutual_aid_trust |>
  ggplot(aes(x = any_group, y = Net_trust)) +
  geom_boxplot()

summary(lm(Net_trust ~ any_group, data = mutual_aid_trust))
