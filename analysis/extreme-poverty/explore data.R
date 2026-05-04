library(tidyverse)
library(plotly)

# Load data from Our World in Data
# Source: https://ourworldindata.org/grapher/share-of-population-in-extreme-poverty
extreme_poverty <- read_csv(
  "https://ourworldindata.org/grapher/share-of-population-in-extreme-poverty.csv?v=1&csvType=full&useColumnShortNames=true"
) |>
  rename(
    extreme_poverty_pct = headcount_ratio__ppp_version_2021__poverty_line_300__welfare_type_income_or_consumption__table_income_or_consumption_consolidated__survey_comparability_no_spells
  )

# ---- Which countries/regions have no extreme poverty data? ----
# Use a dummy value for missing data
missing_countries <-
  extreme_poverty |>
  replace_na(list(extreme_poverty_pct = -99999)) |>
  group_by(entity, owid_region) |>
  filter(extreme_poverty_pct == max(extreme_poverty_pct)) |>
  summarise(extreme_poverty_pct = first(extreme_poverty_pct)) |>
  ungroup() |>
  filter(extreme_poverty_pct == -99999)

nrow(missing_countries)

missing_countries |>
  count(owid_region, sort = TRUE)

missing_countries |>
  filter(owid_region == "Africa")

# ---- Extreme poverty trends in countries with protracted humanitarian crises ----
# Source: https://www.rescue.org/watchlist
crises <- c(
  "AFG",
  "BFA",
  "CMR",
  "COD",
  "COL",
  "ETH",
  "HTI",
  "LBN",
  "MLI",
  "MMR",
  "NER",
  "NGA",
  "PSE",
  "SDN",
  "SOM",
  "SSD",
  "SYR",
  "TCD",
  "UKR",
  "YEM"
)

extreme_poverty |>
  filter(code %in% crises) |>
  filter(year >= 1990) |>
  drop_na(extreme_poverty_pct) |>

  ggplot(aes(
    x = year,
    y = extreme_poverty_pct,
    colour = entity
  )) +
  geom_line(aes(group = entity), show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~entity, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA))

# ---- Explore trends in Africa ----
extreme_poverty_africa_wb <-
  extreme_poverty |>
  filter(str_detect(entity, "Western and Central|Eastern and Southern")) |>
  mutate(region = str_remove(entity, " \\(WB\\)"))

extreme_poverty_africa_wb |>
  ggplot(aes(
    x = year,
    y = extreme_poverty_pct,
    colour = entity
  )) +
  geom_line(aes(group = entity), show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  scale_y_continuous(limits = c(0, NA))

# Eastern and Southern Africa
# https://www.worldbank.org/ext/en/region/afr/eastern-and-southern-africa
es_africa <- c(
  "AGO",
  "BDI",
  "BWA",
  "COM",
  "COD",
  "DJI",
  "ERI",
  "SWZ",
  "ETH",
  "KEN",
  "LSO",
  "MDG",
  "MWI",
  "MUS",
  "MOZ",
  "NAM",
  "RWA",
  "STP",
  "SYC",
  "SOM",
  "ZAF",
  "SSD",
  "SDN",
  "TZA",
  "UGA",
  "ZMB",
  "ZWE"
)

# Western and Central Africa
# https://www.worldbank.org/ext/en/region/afr/western-and-central-africa
wc_africa <- c(
  "BEN",
  "BFA",
  "CPV",
  "CMR",
  "CAF",
  "TCD",
  "COG",
  "CIV",
  "GNQ",
  "GAB",
  "GMB",
  "GHA",
  "GIN",
  "GNB",
  "LBR",
  "MLI",
  "MRT",
  "NER",
  "NGA",
  "SEN",
  "SLE",
  "TGO"
)

# Check
extreme_poverty |>
  filter(code %in% es_africa) |>
  distinct(code) |>
  count() ==
  length(es_africa)

extreme_poverty |>
  filter(code %in% wc_africa) |>
  distinct(code) |>
  count() ==
  length(wc_africa)

# Plot
extreme_poverty_africa <-
  extreme_poverty |>
  filter(code %in% c(es_africa, wc_africa)) |>
  filter(year >= 1980) |>
  drop_na(extreme_poverty_pct) |>
  mutate(
    region = if_else(
      code %in% es_africa,
      "Eastern and Southern Africa",
      "Western and Central Africa"
    )
  ) |>
  mutate(
    pop_in_poverty = population_historical * (extreme_poverty_pct / 100)
  )

extreme_poverty_africa |>
  ggplot(
    aes(
      x = year,
      y = extreme_poverty_pct
      #colour = entity
    )
  ) +
  geom_vline(xintercept = 2000, lty = 2) +
  geom_line(
    aes(group = entity),
    colour = "grey",
    alpha = 0.6,
    show.legend = FALSE
  ) +
  geom_point(
    aes(text = entity),
    colour = "grey",
    alpha = 0.6,
    show.legend = FALSE
  ) +

  # Plot regions
  geom_line(data = extreme_poverty_africa_wb, colour = "black") +

  facet_wrap(~region, scales = "fixed") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal()

ggplotly()

# What's driving these differences between regions?
extreme_poverty_africa |>
  ggplot(aes(x = year, y = pop_in_poverty)) +
  geom_col(aes(fill = entity), position = "stack", show.legend = FALSE) +
  facet_wrap(~region, scales = "fixed") +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  theme_minimal()

ggplotly()

# Area/stacked chart with %s of population
extreme_poverty_africa |>
  group_by(year, region, entity) |>
  summarise(prop = pop_in_poverty / sum(pop_in_poverty))

# Get the latest figures for each nation
extreme_poverty_africa |>
  group_by(region, entity) |>
  filter(year == max(year)) |>

  ggplot(aes(x = reorder(entity, pop_in_poverty), y = pop_in_poverty)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~region, scales = "free_y") +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma) +
  theme_minimal()
