library(tidyverse)
library(demographr)
library(nomisr)
library(IMD)

# ---- Function to calculate diversity index -----------------------------------
# Source: https://gedi.ac.uk/wp-content/uploads/2024/06/GEDIGuidance_MethodsMeasures_Final.pdf
calculate_rdi <-
  function(
    data,
    var,
    groups,
    higher_level_geography,
    normalise_rdi = TRUE
  ) {
    n_groups <-
      data |>
      distinct({{ groups }}) |>
      count() |>
      pull(n)

    data_rdi <-
      data |>
      group_by({{ higher_level_geography }}) |>
      mutate(
        prop_group = {{ var }} / sum({{ var }}),
        prop_group2 = prop_group^2
      ) |>
      summarise(rdi = 1 / sum(prop_group2)) |>
      ungroup()

    # "RDI can be standardised to range from zero to one by subtracting one and dividing by M-1.
    #  To ease interpretation, the output can be multiplied by 100. This allows RDI scores to range from zero
    #  (only one ethnic group in an area) to 100 (each group makes up an equal share of the total)"
    # See also: https://academic.oup.com/jrsssa/article/170/2/405/7085282
    if (normalise_rdi) {
      data_rdi <-
        data_rdi |>
        mutate(rdi = ((rdi - 1) / (n_groups - 1)) * 100)
    }

    data_rdi
  }

# ---- Neighbourhood cohesion data ---------------------------------------------
# Higher scores (and lower ranks) = worse
cohesion <- cni2023_england_lsoa21 |>
  select(lsoa21_code, lsoa21_name, participation = `Active and Engaged Community Domain Score`)

# ---- Deprivation data (in 2021 and change since 2011) ------------------------
# Deprivation (% households deprived in at least 1 dimension)
deprivation <- census21_deprivation_england_wales_lsoa21 |>
  # Split into 0, 1 or multiple (2+) dimensions of deprivation
  mutate(households_number_deprivation_dimensions = if_else(households_number_deprivation_dimensions == 0, 0, 1)) |>

  group_by(lsoa21_code, households_number_deprivation_dimensions) |>
  summarise(count = sum(count)) |>
  ungroup() |>

  group_by(lsoa21_code) |>
  mutate(total = sum(count)) |>
  ungroup() |>

  mutate(percent = count / total) |>

  filter(households_number_deprivation_dimensions == 1) |>
  select(lsoa21_code, pct_deprived_households = percent)

hist(deprivation$pct_deprived_households)
summary(deprivation$pct_deprived_households)

#TODO: Use percentage point change in deprivation since 2011 as a predictor
# Need to recast the 2011 data to match 2021 LSOAs
# census11_deprivation_england_wales_lsoa11

# ---- % foreign-born ----------------------------------------------------------
# Get data on country of birth by MSOA (2021) from Nomis
# nomis_get_metadata(id = "NM_2024_1", "geography", "TYPE")
# nomis_get_metadata(id = "NM_2024_1", "c2021_cob_12")
# nomis_get_metadata(id = "NM_2024_1", "measures")

cob_raw <-
  nomis_get_data(
    id = "NM_2024_1",
    date = "latest",
    geography = "TYPE151", # LSOA (2021)
    c2021_cob_12 = "1",  # Born in the UK
    measures = "20301" # Percent
  )

pct_foreign_born <- 100 - cob_raw$OBS_VALUE

range(pct_foreign_born)
#--> Between 0.5% and 89.6% of people in LSOAs were not born in the UK

hist(pct_foreign_born)
summary(pct_foreign_born)

foreign_born <- cob_raw |>
  mutate(pct_foreign_born = (100 - OBS_VALUE) / 100) |>
  select(lsoa21_code = GEOGRAPHY_CODE, pct_foreign_born)

# ---- Ethnic diversity --------------------------------------------------------
# RDI scores range from zero (only one ethnic group in an area) to
# 100 (each group makes up an equal share of the total)
rdi_ethnicity <- ethnicity21_lsoa21 |>
  calculate_rdi(n, ethnic_group, lsoa21_code) |>
  mutate(rdi = rdi / 100) |>
  rename(rdi_ethnicity = rdi)

hist(rdi_ethnicity$rdi_ethnicity)
summary(rdi_ethnicity$rdi_ethnicity)

# ---- Explore associations ----------------------------------------------------
neighbourhoods <- cohesion |>
  left_join(deprivation, by = "lsoa21_code") |>
  left_join(foreign_born, by = "lsoa21_code") |>
  left_join(rdi_ethnicity, by = "lsoa21_code")

# - Civic engagement, deprivation and % foreign-born -
neighbourhoods |>
  pivot_longer(cols = c(participation, pct_deprived_households),
               names_to = "measure", values_to = "value") |>

  ggplot(aes(x = pct_foreign_born, y = value)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  facet_wrap(~measure, scales = "free_y")

neighbourhoods |>
  mutate(pct_deprived_households_cat = cut(pct_deprived_households,
                                        breaks = c(-0.01, 0.2, 0.4, 0.6, 0.8, 1),
                                        labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"))) |>
  ggplot(aes(x = pct_foreign_born, y = participation, colour = pct_deprived_households_cat)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ pct_deprived_households_cat, nrow = 1) +
  labs(x = "% foreign-born",
       y = "Participation score",
       colour = "% deprived households",
       title = "Neighbourhood participation score vs % foreign-born",
       subtitle = "Stratified by % deprived households") +
  theme_minimal()

# - Civic engagement, deprivation and ethnic diversity -
neighbourhoods |>
  pivot_longer(cols = c(participation, pct_deprived_households),
               names_to = "measure", values_to = "value") |>

  ggplot(aes(x = rdi_ethnicity, y = value)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  facet_wrap(~measure, scales = "free_y")

neighbourhoods |>
  mutate(pct_deprived_households_cat = cut(pct_deprived_households,
                                           breaks = c(-0.01, 0.2, 0.4, 0.6, 0.8, 1),
                                           labels = c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"))) |>
  ggplot(aes(x = rdi_ethnicity, y = participation, colour = pct_deprived_households_cat)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ pct_deprived_households_cat, nrow = 1) +
  labs(x = "Ethnic diversity index (higher = more diverse)",
       y = "Participation score",
       colour = "% deprived households",
       title = "Neighbourhood participation score vs ethnic diversity",
       subtitle = "Stratified by % deprived households") +
  theme_minimal()

# ---- How does deprivation modify the effect of % foreign-born on participation? ----
mod_cohesion <- lm(participation ~ pct_foreign_born * pct_deprived_households, data = neighbourhoods)

summary(mod_cohesion)

# Predictions across a grid of values
# Use a realistic range of % foreign-born and % deprived households
pred_grid <- expand.grid(
  pct_foreign_born = seq(0, 0.9, by = 0.05),
  pct_deprived_households = seq(0.2, 0.9, by = 0.1)
)

# pred_grid$predicted_participation = predict(mod_cohesion, newdata = pred_grid)
pred_grid <- bind_cols(
    pred_grid,
    predict(mod_cohesion, newdata = pred_grid, interval = "confidence")
  ) |>
  rename(predicted_participation = fit)

# slice_levels <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
# pred_slices <- pred_grid |>
#   filter(pct_deprived_households %in% slice_levels)

# Predict effects of foreign-born population and deprivation on civic participation
pred_grid |>
  ggplot(aes(x = pct_foreign_born, y = predicted_participation, group = factor(pct_deprived_households))) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line() +
  # geom_point(data = neighbourhoods, aes(x = pct_foreign_born, y = participation), alpha = 0.05, colour = "red") +
  facet_wrap(~ pct_deprived_households, nrow = 1,
             labeller = label_bquote(Deprivation == .(pct_deprived_households))) +
  labs(x = "% foreign-born",
       y = "Predicted participation score",
       title = "Predicted neighbourhood participation score",
       subtitle = "From linear model with interaction between % foreign-born and % deprived households") +
  theme_minimal()

# ---- Effects of ethnic diversity and deprivation on participation ------------
mod_cohesion_ethnicity <- lm(participation ~ rdi_ethnicity * pct_deprived_households, data = neighbourhoods)

summary(mod_cohesion_ethnicity)

# Predictions across a grid of values
# Use a realistic range of % foreign-born and % deprived households
pred_grid <- expand.grid(
  rdi_ethnicity = seq(0, 0.9, by = 0.1),
  pct_deprived_households = seq(0.2, 0.9, by = 0.1)
)

pred_grid <- bind_cols(
  pred_grid,
  predict(mod_cohesion_ethnicity, newdata = pred_grid, interval = "confidence")
) |>
  rename(predicted_participation = fit)

# Predict effects of ethnic diversity and deprivation on civic participation
pred_grid |>
  ggplot(aes(x = rdi_ethnicity, y = predicted_participation, group = factor(pct_deprived_households))) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line() +
  facet_wrap(~ pct_deprived_households, nrow = 1,
             labeller = label_bquote(Deprivation == .(pct_deprived_households))) +
  labs(x = "Ethnic diversity (higher = more diverse)",
       y = "Predicted participation score",
       title = "Neighbourhood ethnic diversity has no effect on civic participation",
       subtitle = "From linear model with interaction between ethnic diversity and % deprived households") +
  theme_minimal()

# ---- Ideas for future analyses -----------------------------------------------
# Crime stats in LSOAs
# Source: https://data.police.uk/data/fetch/c900f9c3-1fab-4e6c-b024-f9b0a7e5c18f/

# Pupil characteristics - number of pupils by ethnicity and language
# Source: https://explore-education-statistics.service.gov.uk/data-tables/permalink/06eae795-539d-4a40-8bd3-08ddfddcf85d
