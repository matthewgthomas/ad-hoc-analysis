library(tidyverse)

indices <- read_rds(
  "analysis/humanitarian-intensity/output/humanitarian_indices_country.rds"
)

brc_partners <- read_csv(
  "analysis/humanitarian-intensity/data/BRC_partnerships.csv"
)

hyper_prioritised <- read_csv(
  "analysis/humanitarian-intensity/data/hyper_prioritised_countries.csv"
) |>
  janitor::clean_names()

indices <- left_join(indices, brc_partners) |>
  left_join(hyper_prioritised) |>
  mutate(
    partnership_category = replace_na(partnership_category, "No partnership")
  ) |>
  mutate(top_10_pct = top_10_count / indices_ranked_count)

indices |>
  filter(partnership_category != "No partnership") |>
  View()

indices |>
  filter(partnership_category == "No partnership") |>
  View()
