library(tidyverse)
library(readxl)
library(IMD)
library(sf)
library(ggbeeswarm)

# ---- Calculate %-point change in Labour vote share ----
# Load constituency results 2019 and 2024
results_2019 <- read_csv("analysis/elections/data/constituency-results-2019.csv")
results_2024 <- read_csv("analysis/elections/data/constituency-results-2024.csv")

labour_swing <-
  results_2024 |>
  select(cons_code, cons_name, winner, Lab_pc_2024 = Lab_pc) |>
  left_join(
    results_2019 |>
      select(cons_code, Lab_pc_2019 = Lab_pc, winner_2019 = winner)
  ) |>
  mutate(Lab_swing = Lab_pc_2024 - Lab_pc_2019)

# ---- LSOA to constituency lookup ----
# Source: https://geoportal.statistics.gov.uk/datasets/ons::lsoa-2021-to-westminster-parliamentary-constituencies-july-2024-best-fit-lookup-in-ew/about
lookup_lsoa_cons <- read_sf("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LSOA21_PCON25_LAD21_EW_LU/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")

lookup_lsoa_cons <-
  lookup_lsoa_cons |>
  st_drop_geometry() |>
  select(lsoa21_code = LSOA21CD, cons_code = PCON25CD)

# ---- Calculate proportions of left-behind areas in each constituency ----
cni2023_cons <-
  cni2023_england_lsoa21 |>
  mutate(CNI_decile = ntile(`Community Needs Index 2023 Rank`, n = 10)) |>
  left_join(lookup_lsoa_cons) |>

  select(lsoa21_code, cons_code, `Left Behind Area?`) |>

  count(cons_code, `Left Behind Area?`) |>
  pivot_wider(names_from = `Left Behind Area?`, values_from = n) |>
  mutate(`TRUE` = if_else(is.na(`TRUE`), 0, `TRUE`)) |>
  mutate(Proportion = `TRUE` / (`TRUE` + `FALSE`))

  # IMD:::calculate_proportion(CNI_decile, cons_code)

# ---- Explore Labour swing in left-behind areas ----
labour_swing_cni <-
  labour_swing |>
  left_join(cni2023_cons)

cons_to_highlight <- c(
  # Places that swung to pro-Palestinian candidates
  "Birmingham Perry Barr",
  "Blackburn",
  "Dewsbury and Batley",
  "Leicester South",

  # Places with large reductions in Labour majorities
  "Bradford West",
  "Bethnal Green and Stepney"

  # Places where Labour MPs won with small margins against pro-Palestinian candidates
  # "Ilford North", # Wes Streeting
  # "Birmingham Yardley" # Jess Phillips
)

seats_to_highlight <-
  labour_swing_cni |>
  filter(cons_name %in% cons_to_highlight) |>
  filter(Proportion > 0) |>
  mutate(Lab_swing = Lab_swing * 100)

labour_swing_cni |>
  filter(winner_2019 == "Lab") |>
  filter(Proportion > 0) |>
  mutate(Lab_swing = Lab_swing * 100) |>

  ggplot(aes(x = Proportion, y = Lab_swing)) +
  geom_hline(yintercept = 0, colour = "black") +
  geom_point(aes(colour = winner), alpha = 0.3, size = 1.2) +
  geom_smooth(method = "lm", colour = "black", linetype = 2, se = FALSE) +
  geom_point(data = seats_to_highlight, aes(colour = winner), shape = 21, alpha = 1, size = 1.3) +
  geom_text(data = seats_to_highlight, aes(label = cons_name), size = 3, hjust = -0.1) +
  scale_color_manual(values = c("#0087dc", "gray40", "#d50000", "#F6B527", "#12B6CF", "black")) +
  scale_x_continuous(labels = scales::percent) +
  #scale_y_continuous(position = "right") +
  theme_minimal() +
  theme(
    plot.title.position = "plot",
    legend.position = "top",
    plot.background = element_rect(fill = "white", colour = NA)
  ) +
  labs(
    title = "Labour swing slightly increased in 'left-behind' areas",
    subtitle = "2019-24, %-point change in Labour vote share among Labour-held seats in 2019",
    x = "Left-behind neighbourhoods in each constituency",
    y = "%-point change in Labour vote share",
    colour = "2024 election results",
    caption = "@matthewgthomas analysis of Democracy Club, Rallings and Thrasher, and OCSI data"
  )

ggsave("analysis/elections/labour swing in left-behind areas.png", width = 142, height = 120, units = "mm")
