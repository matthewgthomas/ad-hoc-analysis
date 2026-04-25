library(tidyverse)

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

  ggplot(aes(
    x = year,
    y = extreme_poverty_pct,
    colour = entity
  )) +
  geom_line(aes(group = entity), show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~entity, scales = "free") +
  scale_y_continuous(limits = c(0, NA))
