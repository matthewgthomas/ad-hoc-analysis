remotes::install_github("humaniverse/IMD")
remotes::install_github("humaniverse/geographr")

library(tidyverse)
library(geographr)
library(IMD)
library(rio)
library(sf)

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

# How many income-deprived people and what % don't live in the 20% most deprived neighbourhoods?
imd_income |>
  mutate(
    Core20 = if_else(
      IMD_decile <= 2,
      "20% most deprived",
      "Less-deprived areas"
    )
  ) |>
  group_by(Core20) |>
  summarise(n = sum(income_domain_numerator, na.rm = TRUE)) |>
  mutate(prop = n / sum(n))

# Wrangle and save for Flourish
imd_income |>
  mutate(
    Core20 = if_else(
      IMD_decile <= 2,
      "20% most deprived",
      "Less-deprived areas"
    )
  ) |>
  group_by(region_name, Core20) |>
  summarise(
    income_domain_numerator = sum(income_domain_numerator, na.rm = TRUE)
  ) |>
  pivot_wider(
    names_from = region_name,
    values_from = income_domain_numerator
  ) |>
  write_csv("analysis/deprivation/regional-income-deprivation.csv")

# ---- Geographical barriers ----
geog_barriers_ruc <-
  imd2025_england_lsoa21_subdomains |>
  select(lsoa21_code, geographical_barriers_decile) |>
  # mutate(geographical_barriers_quintile = ntile(geographical_barriers_rank, n = 5)) |>
  left_join(ruc21_lsoa21) |>
  count(ruc, geographical_barriers_decile)

geog_barriers_ruc |>
  group_by(geographical_barriers_decile) |>
  mutate(prop = n / sum(n))

geog_barriers_ruc |>
  ggplot(aes(x = factor(geographical_barriers_decile), y = n, fill = ruc)) +
  geom_col()

# ---- Geographical barriers in previous IMDs ----
# Plot the relationship between IMD rank and the geographical and wider barriers subdomains
plot_subdomains <- function(imd, subdomains) {
  imd_data <-
    imd |>
    select(starts_with("lsoa"), IMD_rank) |>
    left_join(
      subdomains |>
        select(
          starts_with("lsoa"),
          geographical_barriers_sub_domain_rank,
          wider_barriers_sub_domain_rank
        )
    )

  cor_geog <- cor.test(
    imd_data$IMD_rank,
    imd_data$geographical_barriers_sub_domain_rank
  )

  cor_wider <- cor.test(
    imd_data$IMD_rank,
    imd_data$wider_barriers_sub_domain_rank
  )

  plt <- imd_data |>
    pivot_longer(
      cols = contains("sub_domain"),
      names_to = "subdomain",
      values_to = "rank"
    ) |>

    ggplot(aes(x = IMD_rank, y = rank)) +
    geom_point(alpha = 0.1) +
    geom_smooth(method = "lm") +
    facet_wrap(~subdomain) +
    labs(
      subtitle = str_glue(
        "Correlation between IMD rank and geographical barriers = {cor_geog$estimate}\nCorrelation between IMD rank and wider barriers = {cor_wider$estimate}"
      )
    )

  return(plt)
}

plot_subdomains(imd2004_england_lsoa01, imd2004_england_lsoa01_subdomains)
plot_subdomains(imd2007_england_lsoa01, imd2007_england_lsoa01_subdomains)
plot_subdomains(imd2010_england_lsoa01, imd2010_england_lsoa01_subdomains)
plot_subdomains(imd2015_england_lsoa11, imd2015_england_lsoa11_subdomains)
plot_subdomains(
  imd2019_england_lsoa11,
  imd2019_england_lsoa11_subdomains |>
    rename(
      geographical_barriers_sub_domain_rank = Geographical_barriers_rank,
      wider_barriers_sub_domain_rank = Wider_barriers_rank
    )
)

# ---- Geographical barriers and low income ----
imd_income_barriers <-
  imd2025_england_lsoa21 |>
  select(lsoa21_code, IMD_decile) |>
  left_join(
    imd2025_england_lsoa21_indicators |>
      select(lsoa21_code, income_domain_numerator)
  ) |>
  left_join(
    imd2025_england_lsoa21_subdomains |>
      select(lsoa21_code, geographical_barriers_decile)
  )

imd_income_barriers |>
  filter(IMD_decile > 2 & geographical_barriers_decile <= 2) |>
  summarise(n_income_deprived = sum(income_domain_numerator, na.rm = TRUE))

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
cni2023_england_lsoa21 <- cni2023_england_lsoa21 |>
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

# ---- Hidden deprivation ----
# Count the number of households experiencing different amounts of deprivation
# in otherwise less-deprived neighbourhoods in each Local Authority
hidden_deprivation_ltla <-
  census21_deprivation_england_wales_lsoa21 |>
  filter(str_detect(lsoa21_code, "^E")) |>
  left_join(imd2025_england_lsoa21 |> select(lsoa21_code, IMD_decile)) |>
  left_join(lookup_lsoa21_ward24_ltla24) |>

  # Keep only the least-deprived 50% of neighbourhoods
  filter(IMD_decile > 5) |>

  group_by(
    ltla24_code,
    ltla24_name,
    households_number_deprivation_dimensions
  ) |>
  summarise(count = sum(count)) |>
  ungroup() |>
  group_by(ltla24_code, ltla24_name) |>
  mutate(prop = count / sum(count)) |>
  ungroup()

# Counts and percents of households experiencing multiple deprivation in each LA
hidden_deprivation_ltla_summary <-
  hidden_deprivation_ltla |>
  filter(str_detect(ltla24_code, "^E")) |>
  filter(households_number_deprivation_dimensions >= 2) |>
  group_by(ltla24_code, ltla24_name) |>
  summarise(
    percent_household_deprivation = sum(prop, na.rm = TRUE)
  ) |>
  ungroup() |>
  left_join(imd2025_england_ltla24 |> select(ltla24_code, imd25_extent))

# Which LAs have the highest % of households in hidden multiple deprivation
# but the lowest % of people living in deprived neighbourhoods?
hidden_deprivation_ltla_summary |>
  arrange(desc(percent_household_deprivation), imd25_extent) |>
  mutate(cum_best_p2 = cummin(imd25_extent)) |>
  filter(imd25_extent <= cum_best_p2) # nondominated set for (max p1, min p2)

# Adur has 15.3% of households in hidden deprivation and 3.3% of people in the most deprived areas so will use this for the article
household_deprivation_adur <-
  census21_deprivation_england_wales_lsoa21 |>
  filter(households_number_deprivation_dimensions >= 2) |>
  group_by(lsoa21_code) |>
  summarise(percent_household_deprivation = sum(percent, na.rm = TRUE)) |>
  ungroup() |>

  left_join(imd2025_england_lsoa21 |> select(lsoa21_code, IMD_decile)) |>
  left_join(lookup_lsoa21_ward24_ltla24) |>

  filter(ltla24_name == "Adur")

# Save hidden deprivation data for Flourish
household_deprivation_adur |>
  mutate(
    percent_hidden_deprivation = if_else(
      IMD_decile > 5,
      percent_household_deprivation,
      NA
    ),
    no_popup = if_else(is.na(percent_hidden_deprivation), "No popup", "")
  ) |>
  select(
    lsoa21_code,
    lsoa21_name,
    percent_hidden_deprivation,
    IMD_decile,
    no_popup
  ) |>
  write_csv("analysis/deprivation/adur.csv")

# Fetch LSOA 2021 boundaries and make a GeoJSON for Adur to upload to Flourish
lsoa21_sf <- read_sf(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Lower_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V5/FeatureServer/0/query?outFields=LSOA21CD,LSOA21NM&where=1%3D1&f=geojson"
)

lsoa21_sf |>
  select(lsoa21_code = LSOA21CD, lsoa21_name = LSOA21NM) |>
  filter(str_detect(lsoa21_name, "^Adur")) |>
  write_sf("analysis/deprivation/adur.geojson")

# What is Adur ranked?
imd2025_england_ltla24 |>
  select(ltla24_name, imd_rank_of_average_score) |>
  filter(ltla24_name == "Adur")

# ---- Local Authorities ----
# Compare extent and population-weighted average scores
imd2025_england_ltla24 |>
  select(ltla24_name, imd25_extent, imd_rank_of_average_score) |>
  arrange(desc(imd25_extent))

# Which LAs have the highest extents but are not in the worst 10% for average scores?
imd2025_england_ltla24 |>
  select(
    ltla24_name,
    imd_rank_of_average_score,
    imd25_extent,
    imd25_rank_of_extent
  ) |>
  mutate(
    avg_decile = ntile(imd_rank_of_average_score, n = 10),
    ext_decile = ntile(imd25_rank_of_extent, n = 10),
  ) |>
  filter(avg_decile > 1 & ext_decile == 1)

# Load population for LSOAs
# Source: File 6: Population denominators @ https://www.gov.uk/government/statistics/english-indices-of-deprivation-2025
lsoa21_pop <- import(
  "https://assets.publishing.service.gov.uk/media/68ff5c8c394b8c2a6ddf5d90/File_6_IoD2025_Population_Denominators.xlsx",
  sheet = "ID 2025 Population Denominators"
) |>
  select(
    lsoa21_code = `LSOA code (2021)`,
    population = `Total population: mid 2022`
  )

ltla24_pop <-
  lsoa21_pop |>
  left_join(lookup_lsoa21_ward24_ltla24) |>
  group_by(ltla24_code) |>
  summarise(population = sum(population)) |>
  ungroup()

imd2025_england_ltla24 |>
  left_join(ltla24_pop) |>
  select(ltla24_name, imd25_extent, imd_rank_of_average_score, population) |>
  mutate(deprived_population = population * imd25_extent) |>
  arrange(desc(imd25_extent))

# Difference between the number of people in Manchester and Blackpool living in England's most deprived neighbourhoods
scales::comma(333095 - 73898)
