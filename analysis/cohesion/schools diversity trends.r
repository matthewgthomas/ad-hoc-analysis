library(tidyverse)

# ---- Function to calculate diversity index -----------------------------------
# Source: https://gedi.ac.uk/wp-content/uploads/2024/06/GEDIGuidance_MethodsMeasures_Final.pdf
calculate_rdi <- function(
    data,
    var,
    groups,
    higher_level_geography,
    normalise_rdi = TRUE
) {
  # capture the user’s current grouping (e.g., time_period)
  existing_groups <- dplyr::group_vars(data)
  geo_name <- rlang::as_name(rlang::enquo(higher_level_geography))
  by_keys <- c(existing_groups, geo_name)

  # how many distinct categories (M) in `groups` per slice?
  n_groups_df <-
    data |>
    group_by(across(all_of(by_keys))) |>
    summarise(
      n_groups = n_distinct({{ groups }}),
      .groups = "drop"
    )

  # compute RDI per slice (existing groups + geography)
  rdi_df <-
    data |>
    group_by(across(all_of(by_keys))) |>
    summarise(
      rdi = 1 / sum(({{ var }} / sum({{ var }}, na.rm = TRUE))^2, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(n_groups_df, by = by_keys) |>
    mutate(
      rdi = if (normalise_rdi) {
        # ((RDI - 1) / (M - 1)) * 100, safely handle M = 1
        dplyr::if_else(n_groups > 1, ((rdi - 1) / (n_groups - 1)) * 100, 0)
      } else rdi
    )

  # return grouped the same way the user passed it in
  rdi_df |> group_by(across(all_of(existing_groups)))
}

# ---- Prep school data --------------------------------------------------------
schools_raw <- read_csv("analysis/cohesion/data/data-school-pupils-and-their-characteristics.csv")

schools_ethnicity <-
  schools_raw |>
  filter(language == "Total") |>
  filter(ethnicity_minor != "Total") |>
  select(time_period, new_la_code, la_name, ethnicity_minor, headcount)

schools_ethnic_diversity <-
  schools_ethnicity |>
  group_by(time_period) |>
  calculate_rdi(headcount, ethnicity_minor, new_la_code) |>
  ungroup()

schools_ethnic_diversity |>
  ggplot(aes(x = time_period, y = rdi, group = time_period)) +
  geom_boxplot() +
  geom_jitter(width = 0.1, alpha = 0.3) +
  labs(
    title = "Trends in ethnic diversity in schools",
    subtitle = "Each dot = a local authority",
    y = "Ethnic diversity (0-100, higher = more diverse)"
  )

schools_ethnic_diversity |>
  ggplot(aes(x = time_period, y = rdi)) +
  geom_jitter() +
  geom_smooth() +
  labs(
    title = "Trends in ethnic diversity in schools",
    subtitle = "Each dot = a local authority",
    y = "Ethnic diversity (0-100, higher = more diverse)"
  )
