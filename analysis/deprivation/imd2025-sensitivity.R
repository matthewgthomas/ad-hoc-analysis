#!/usr/bin/env Rscript

# Sensitivity analysis for the English Index of Multiple Deprivation 2025.
#
# Inputs:
#   * IMD::imd2025_england_lsoa21_indicators
#   * IMD::imd2025_england_lsoa21
#   * MHCLG File 9: exponentially transformed domain scores
#
# The script uses the exact IMD exponential transformation and runs:
#   * deterministic methodological scenarios;
#   * one-at-a-time domain-weight perturbations;
#   * leave-one-indicator-out checks for Health and Crime, the two domains
#     fully reconstructible from the published IMD package indicators;
#   * Monte Carlo weight and joint weight/transformation stress tests.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readxl)
  library(scales)
  library(tidyr)
})

set.seed(20250705)

output_dir <- "output"
table_dir <- file.path(output_dir, "tables")
figure_dir <- file.path(output_dir, "figures")
download_dir <- file.path("tmp", "downloads")
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

official_transformed_url <- paste0(
  "https://assets.publishing.service.gov.uk/media/691ded670dcbf6343e9a2a6c/",
  "File_9_IoD2025_Transformed_Domain_Scores.xlsx"
)
official_transformed_file <- file.path(
  download_dir,
  "File_9_IoD2025_Transformed_Domain_Scores.xlsx"
)

if (!file.exists(official_transformed_file)) {
  download.file(official_transformed_url, official_transformed_file, mode = "wb")
}

load_imd_object <- function(object_name) {
  if (requireNamespace("IMD", quietly = TRUE)) {
    return(getExportedValue("IMD", object_name))
  }

  source_file <- file.path(
    download_dir,
    "IMD-master",
    "data",
    paste0(object_name, ".rda")
  )
  if (file.exists(source_file)) {
    holder <- new.env(parent = emptyenv())
    load(source_file, envir = holder)
    return(holder[[object_name]])
  }

  stop(
    "Install github.com/humaniverse/IMD or extract its data files under ",
    file.path(download_dir, "IMD-master", "data"),
    call. = FALSE
  )
}

imd_indicators <- load_imd_object("imd2025_england_lsoa21_indicators")
imd_published <- load_imd_object("imd2025_england_lsoa21")

domain_names <- c(
  "Income", "Employment", "Education", "Health",
  "Crime", "Barriers", "Living environment"
)

official_weights <- c(
  Income = 0.225,
  Employment = 0.225,
  Education = 0.135,
  Health = 0.135,
  Crime = 0.093,
  Barriers = 0.093,
  `Living environment` = 0.093
)

transformed_raw <- read_excel(
  official_transformed_file,
  sheet = "IoD25 Transformed Domain Scores"
)

domain_data <- transformed_raw |>
  transmute(
    lsoa21_code = `LSOA code (2021)`,
    Income = `Income Score - exponentially transformed`,
    Employment = `Employment Score - exponentially transformed`,
    Education = `Education Score - exponentially transformed`,
    Health = `Health Score - exponentially transformed`,
    Crime = `Crime Score - exponentially transformed`,
    Barriers = `Barriers Score - exponentially transformed`,
    `Living environment` = `Living Environment Score - exponentially transformed`
  ) |>
  inner_join(
    imd_published |>
      select(lsoa21_code, IMD_score, IMD_rank, IMD_decile),
    by = "lsoa21_code"
  ) |>
  arrange(lsoa21_code)

domain_matrix <- as.matrix(domain_data[, domain_names])
storage.mode(domain_matrix) <- "double"
n_lsoa <- nrow(domain_matrix)

stable_rank_desc <- function(score) {
  # Input rows are sorted by LSOA code, providing deterministic tie-breaking.
  as.integer(rank(-score, ties.method = "first"))
}

rank_to_decile <- function(rank_value) {
  pmin(10L, as.integer(ceiling(rank_value * 10 / n_lsoa)))
}

exp_from_fractional_rank <- function(rank_fraction, constant = 23) {
  -constant * log(
    1 - rank_fraction * (1 - exp(-100 / constant))
  )
}

# Fractional rank 1/N is least deprived and 1 is most deprived.
domain_rank_fraction <- apply(
  domain_matrix,
  2,
  function(x) rank(x, ties.method = "average") / n_lsoa
)

score_with <- function(weights, constant = 23, use_published_scores = FALSE) {
  weights <- weights / sum(weights)
  if (use_published_scores && isTRUE(all.equal(constant, 23))) {
    transformed <- domain_matrix
  } else {
    transformed <- exp_from_fractional_rank(domain_rank_fraction, constant)
  }
  as.vector(transformed %*% weights)
}

baseline_rank <- domain_data$IMD_rank
baseline_decile <- domain_data$IMD_decile
# The published percentages sum to 99.9 because the three 9.3% weights are
# reported to one decimal place. MHCLG's published IMD score is reproduced by
# applying those reported weights directly, without rescaling.
baseline_score_reconstructed <- as.vector(domain_matrix %*% official_weights)
baseline_rank_reconstructed <- stable_rank_desc(baseline_score_reconstructed)

baseline_validation <- tibble(
  n_lsoa = n_lsoa,
  score_pearson = cor(
    baseline_score_reconstructed,
    domain_data$IMD_score,
    method = "pearson"
  ),
  rank_spearman = cor(
    baseline_rank_reconstructed,
    baseline_rank,
    method = "spearman"
  ),
  exact_rank_match = mean(baseline_rank_reconstructed == baseline_rank),
  mean_absolute_rank_difference = mean(
    abs(baseline_rank_reconstructed - baseline_rank)
  ),
  maximum_absolute_rank_difference = max(
    abs(baseline_rank_reconstructed - baseline_rank)
  ),
  maximum_absolute_score_difference = max(
    abs(baseline_score_reconstructed - domain_data$IMD_score)
  )
)

write.csv(
  baseline_validation,
  file.path(table_dir, "baseline_validation.csv"),
  row.names = FALSE
)

domain_association <- tibble(
  domain = domain_names,
  official_weight = as.numeric(official_weights),
  spearman_with_imd_score = vapply(
    seq_along(domain_names),
    function(j) cor(
      domain_matrix[, j],
      domain_data$IMD_score,
      method = "spearman"
    ),
    numeric(1)
  )
)
write.csv(
  domain_association,
  file.path(table_dir, "domain_association.csv"),
  row.names = FALSE
)
write.csv(
  cor(domain_matrix, method = "spearman"),
  file.path(table_dir, "domain_spearman_correlations.csv"),
  row.names = TRUE
)

scenario_metrics <- function(name, score, family, detail = NA_character_) {
  scenario_rank <- stable_rank_desc(score)
  scenario_decile <- rank_to_decile(scenario_rank)
  abs_change <- abs(scenario_rank - baseline_rank)
  top10_n <- floor(n_lsoa * 0.10)
  top20_n <- floor(n_lsoa * 0.20)

  tibble(
    scenario = name,
    family = family,
    detail = detail,
    rank_spearman = cor(scenario_rank, baseline_rank, method = "spearman"),
    mean_absolute_rank_change = mean(abs_change),
    median_absolute_rank_change = median(abs_change),
    p95_absolute_rank_change = unname(quantile(abs_change, 0.95)),
    maximum_absolute_rank_change = max(abs_change),
    decile_agreement = mean(scenario_decile == baseline_decile),
    within_one_decile = mean(abs(scenario_decile - baseline_decile) <= 1),
    top10_retention = mean(scenario_rank[baseline_rank <= top10_n] <= top10_n),
    top20_retention = mean(scenario_rank[baseline_rank <= top20_n] <= top20_n)
  )
}

scenario_rows <- list()
add_scenario <- function(name, score, family, detail = NA_character_) {
  scenario_rows[[length(scenario_rows) + 1L]] <<-
    scenario_metrics(name, score, family, detail)
}

add_scenario(
  "Official reconstruction",
  baseline_score_reconstructed,
  "Validation",
  "Published transformed scores and official weights"
)

add_scenario(
  "Equal domain weights",
  score_with(rep(1 / 7, 7), use_published_scores = TRUE),
  "Weights",
  "Each domain weighted 1/7"
)

swapped_weights <- official_weights
swapped_weights[c("Employment", "Health")] <-
  swapped_weights[c("Health", "Employment")]
add_scenario(
  "Employment/health weights swapped",
  score_with(swapped_weights, use_published_scores = TRUE),
  "Weights",
  "Employment 13.5%; Health 22.5%"
)

add_scenario(
  "Linear percentile transformation",
  as.vector((domain_rank_fraction * 100) %*% official_weights),
  "Transformation",
  "No exponential tail emphasis"
)

for (constant in c(10, 15, 35, 50)) {
  add_scenario(
    paste0("Exponential constant = ", constant),
    score_with(official_weights, constant = constant),
    "Transformation",
    paste0("Official constant is 23; lower values emphasise the deprived tail")
  )
}

for (domain in domain_names) {
  leave_one_out_weights <- official_weights
  leave_one_out_weights[domain] <- 0
  add_scenario(
    paste0("Exclude ", domain),
    score_with(leave_one_out_weights, use_published_scores = TRUE),
    "Domain exclusion",
    paste0("Remaining weights rescaled to sum to one")
  )
}

scenario_summary <- bind_rows(scenario_rows)
write.csv(
  scenario_summary,
  file.path(table_dir, "scenario_summary.csv"),
  row.names = FALSE
)

# One-at-a-time weight sensitivity: perturb each domain by +/-25%, then rescale.
weight_oat_rows <- list()
for (domain in domain_names) {
  for (change in c(-0.25, 0.25)) {
    perturbed <- official_weights
    perturbed[domain] <- perturbed[domain] * (1 + change)
    label <- paste0(domain, " ", ifelse(change > 0, "+25%", "-25%"))
    weight_oat_rows[[length(weight_oat_rows) + 1L]] <- scenario_metrics(
      label,
      score_with(perturbed, use_published_scores = TRUE),
      "One-at-a-time weight",
      paste0("All weights rescaled after perturbing ", domain)
    ) |>
      mutate(domain = domain, relative_change = change)
  }
}
weight_oat <- bind_rows(weight_oat_rows)
write.csv(
  weight_oat,
  file.path(table_dir, "weight_one_at_a_time.csv"),
  row.names = FALSE
)

# Reconstruct Health and Crime from the published indicator dataset and perform
# leave-one-indicator-out checks. Published rounding creates only tiny differences
# from the official transformed domain scores.
normal_rank_transform <- function(x) {
  qnorm((rank(x, ties.method = "average") - 0.5) / length(x))
}

health_indicators <- c(
  "comparative_illness_and_disability_ratio_indicator",
  "years_of_potential_life_lost_indicator",
  "acute_morbidity_indicator",
  "mental_health_indicator"
)
health_indicator_labels <- c(
  "Comparative illness and disability ratio",
  "Years of potential life lost",
  "Acute morbidity",
  "Mental health"
)
health_weights <- c(0.294, 0.240, 0.222, 0.244)

crime_indicators <- c(
  "violence_with_injury_rate_per_1_000_at_risk_population",
  "violence_without_injury_rate_per_1_000_at_risk_population",
  "stalking_and_harassment_rate_per_1_000_at_risk_population",
  "burglary_rate_per_1_000_at_risk_properties",
  "theft_rate_per_1_000_at_risk_population",
  "criminal_damage_rate_per_1_000_at_risk_population",
  "public_order_and_possession_of_weapons_rate_per_1_000_at_risk_population",
  "anti_social_behaviour_rate_per_1_000_at_risk_population"
)
crime_indicator_labels <- c(
  "Violence with injury",
  "Violence without injury",
  "Stalking and harassment",
  "Burglary",
  "Theft",
  "Criminal damage",
  "Public order and weapons",
  "Anti-social behaviour"
)
crime_weights <- c(0.151, 0.154, 0.132, 0.074, 0.097, 0.144, 0.145, 0.103)

indicator_data <- imd_indicators |>
  select(lsoa21_code, all_of(c(health_indicators, crime_indicators))) |>
  inner_join(domain_data |> select(lsoa21_code), by = "lsoa21_code") |>
  arrange(lsoa21_code)

reconstruct_domain <- function(columns, weights) {
  normal_scores <- vapply(
    indicator_data[, columns],
    normal_rank_transform,
    numeric(n_lsoa)
  )
  raw_domain <- as.vector(normal_scores %*% (weights / sum(weights)))
  fractional_rank <- rank(raw_domain, ties.method = "average") / n_lsoa
  exp_from_fractional_rank(fractional_rank, 23)
}

health_reconstructed <- reconstruct_domain(health_indicators, health_weights)
crime_reconstructed <- reconstruct_domain(crime_indicators, crime_weights)

indicator_domain_validation <- tibble(
  domain = c("Health", "Crime"),
  rank_spearman = c(
    cor(health_reconstructed, domain_matrix[, "Health"], method = "spearman"),
    cor(crime_reconstructed, domain_matrix[, "Crime"], method = "spearman")
  ),
  mean_absolute_score_difference = c(
    mean(abs(health_reconstructed - domain_matrix[, "Health"])),
    mean(abs(crime_reconstructed - domain_matrix[, "Crime"]))
  )
)
write.csv(
  indicator_domain_validation,
  file.path(table_dir, "indicator_domain_validation.csv"),
  row.names = FALSE
)

indicator_loo_rows <- list()
run_indicator_loo <- function(domain, columns, labels, weights) {
  domain_index <- match(domain, domain_names)
  for (j in seq_along(columns)) {
    keep <- seq_along(columns) != j
    replacement <- reconstruct_domain(columns[keep], weights[keep])
    changed_domains <- domain_matrix
    changed_domains[, domain_index] <- replacement
    changed_score <- as.vector(changed_domains %*% official_weights)

    indicator_loo_rows[[length(indicator_loo_rows) + 1L]] <<-
      scenario_metrics(
        paste0("Omit ", labels[j]),
        changed_score,
        "Indicator exclusion",
        paste0(domain, " weights rescaled after indicator omission")
      ) |>
      mutate(
        domain = domain,
        omitted_indicator = labels[j],
        official_indicator_weight = weights[j]
      )
  }
}

run_indicator_loo(
  "Health", health_indicators, health_indicator_labels, health_weights
)
run_indicator_loo(
  "Crime", crime_indicators, crime_indicator_labels, crime_weights
)

indicator_loo <- bind_rows(indicator_loo_rows)
write.csv(
  indicator_loo,
  file.path(table_dir, "indicator_leave_one_out.csv"),
  row.names = FALSE
)

# Monte Carlo uncertainty analysis. These are stress-test envelopes, not sampling
# confidence intervals: the official weights and constant are policy/method choices.
n_sims <- as.integer(Sys.getenv("IMD_SIMS", "1000"))

simulate_ranks <- function(n, vary_constant = FALSE) {
  rank_matrix <- matrix(NA_integer_, nrow = n_lsoa, ncol = n)
  parameter_rows <- vector("list", n)

  for (i in seq_len(n)) {
    weight_multiplier <- runif(7, 0.75, 1.25)
    weights <- official_weights * weight_multiplier
    weights <- weights / sum(weights)
    constant <- if (vary_constant) runif(1, 15, 35) else 23
    score <- score_with(
      weights,
      constant = constant,
      use_published_scores = !vary_constant
    )
    rank_matrix[, i] <- stable_rank_desc(score)
    parameter_rows[[i]] <- tibble(
      simulation = i,
      constant = constant,
      !!!as.list(setNames(as.numeric(weights), domain_names)),
      mean_absolute_rank_change = mean(abs(rank_matrix[, i] - baseline_rank))
    )
  }

  list(ranks = rank_matrix, parameters = bind_rows(parameter_rows))
}

weight_mc <- simulate_ranks(n_sims, vary_constant = FALSE)
joint_mc <- simulate_ranks(n_sims, vary_constant = TRUE)

summarise_rank_uncertainty <- function(rank_matrix, analysis) {
  q <- t(apply(rank_matrix, 1, quantile, probs = c(0.05, 0.5, 0.95)))
  simulated_deciles <- ceiling(rank_matrix * 10 / n_lsoa)
  same_decile_probability <- rowMeans(simulated_deciles == baseline_decile)

  tibble(
    lsoa21_code = domain_data$lsoa21_code,
    analysis = analysis,
    official_rank = baseline_rank,
    official_decile = baseline_decile,
    rank_p05 = q[, 1],
    rank_median = q[, 2],
    rank_p95 = q[, 3],
    rank_p90_interval_width = q[, 3] - q[, 1],
    same_decile_probability = same_decile_probability,
    probability_most_deprived_10pct = rowMeans(rank_matrix <= floor(n_lsoa * 0.10)),
    probability_most_deprived_20pct = rowMeans(rank_matrix <= floor(n_lsoa * 0.20))
  )
}

weight_uncertainty <- summarise_rank_uncertainty(
  weight_mc$ranks,
  "Weights +/-25%"
)
joint_uncertainty <- summarise_rank_uncertainty(
  joint_mc$ranks,
  "Weights +/-25%; exponential constant 15-35"
)

rank_uncertainty <- bind_rows(weight_uncertainty, joint_uncertainty)
write.csv(
  rank_uncertainty,
  file.path(table_dir, "lsoa_rank_uncertainty.csv"),
  row.names = FALSE
)

# Fall-out from the published most-deprived decile ---------------------------
location_lookup <- transformed_raw |>
  transmute(
    lsoa21_code = `LSOA code (2021)`,
    lsoa21_name = `LSOA name (2021)`,
    ltla24_code = `Local Authority District code (2024)`,
    ltla24_name = `Local Authority District name (2024)`
  )

decile1_fallout <- rank_uncertainty |>
  filter(official_decile == 1) |>
  transmute(
    lsoa21_code,
    analysis,
    official_rank,
    fallout_frequency = 1 - probability_most_deprived_10pct
  ) |>
  pivot_wider(
    names_from = analysis,
    values_from = fallout_frequency
  ) |>
  rename(
    weight_fallout_frequency = `Weights +/-25%`,
    joint_fallout_frequency =
      `Weights +/-25%; exponential constant 15-35`
  ) |>
  left_join(location_lookup, by = "lsoa21_code") |>
  mutate(
    weight_fallout_count = round(weight_fallout_frequency * n_sims),
    joint_fallout_count = round(joint_fallout_frequency * n_sims)
  ) |>
  select(
    lsoa21_code,
    lsoa21_name,
    ltla24_code,
    ltla24_name,
    official_rank,
    weight_fallout_count,
    weight_fallout_frequency,
    joint_fallout_count,
    joint_fallout_frequency
  ) |>
  arrange(desc(joint_fallout_frequency), desc(weight_fallout_frequency))

write.csv(
  decile1_fallout,
  file.path(table_dir, "decile1_lsoa_fallout_frequency.csv"),
  row.names = FALSE
)

ltla_fallout <- decile1_fallout |>
  group_by(ltla24_code, ltla24_name) |>
  summarise(
    published_decile1_lsoas = n(),
    mean_weight_fallout_frequency = mean(weight_fallout_frequency),
    mean_joint_fallout_frequency = mean(joint_fallout_frequency),
    maximum_joint_fallout_frequency = max(joint_fallout_frequency),
    .groups = "drop"
  )

write.csv(
  ltla_fallout,
  file.path(table_dir, "decile1_fallout_frequency_by_ltla.csv"),
  row.names = FALSE
)
write.csv(
  weight_mc$parameters,
  file.path(table_dir, "monte_carlo_weight_parameters.csv"),
  row.names = FALSE
)
write.csv(
  joint_mc$parameters,
  file.path(table_dir, "monte_carlo_joint_parameters.csv"),
  row.names = FALSE
)

mc_summary <- rank_uncertainty |>
  group_by(analysis) |>
  summarise(
    simulations = n_sims,
    median_rank_p90_interval_width = median(rank_p90_interval_width),
    p90_rank_p90_interval_width = quantile(rank_p90_interval_width, 0.90),
    p95_rank_p90_interval_width = quantile(rank_p90_interval_width, 0.95),
    median_same_decile_probability = median(same_decile_probability),
    mean_same_decile_probability = mean(same_decile_probability),
    proportion_same_decile_probability_at_least_80pct = mean(
      same_decile_probability >= 0.80
    ),
    top10_retention_probability = mean(
      probability_most_deprived_10pct[official_decile == 1]
    ),
    top20_retention_probability = mean(
      probability_most_deprived_20pct[official_decile <= 2]
    ),
    .groups = "drop"
  )
write.csv(
  mc_summary,
  file.path(table_dir, "monte_carlo_summary.csv"),
  row.names = FALSE
)

most_uncertain <- joint_uncertainty |>
  arrange(desc(rank_p90_interval_width)) |>
  slice_head(n = 100)
write.csv(
  most_uncertain,
  file.path(table_dir, "most_rank_uncertain_lsoas.csv"),
  row.names = FALSE
)

# Dataset audit: makes explicit which requested package indicators are available.
dataset_audit <- tibble(
  statistic = c(
    "LSOAs",
    "Indicator columns excluding LSOA code",
    "Indicator columns with any missing values",
    "Living Environment indicator columns in package object"
  ),
  value = c(
    nrow(imd_indicators),
    ncol(imd_indicators) - 1,
    sum(colSums(is.na(imd_indicators[, -1])) > 0),
    sum(grepl("living|noise|air|pollution|road_traffic|outdoor", names(imd_indicators)))
  )
)
write.csv(
  dataset_audit,
  file.path(table_dir, "indicator_dataset_audit.csv"),
  row.names = FALSE
)

# Figures -------------------------------------------------------------------
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title.position = "plot",
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
)

scenario_plot_data <- scenario_summary |>
  filter(scenario != "Official reconstruction") |>
  mutate(
    scenario = reorder(scenario, mean_absolute_rank_change),
    family = factor(
      family,
      levels = c("Weights", "Transformation", "Domain exclusion")
    )
  )

p_scenarios <- ggplot(
  scenario_plot_data,
  aes(x = mean_absolute_rank_change, y = scenario, colour = family)
) +
  geom_segment(aes(x = 0, xend = mean_absolute_rank_change, yend = scenario),
               colour = "grey82", linewidth = 0.6) +
  geom_point(size = 3) +
  scale_x_continuous(labels = comma) +
  scale_colour_manual(values = c(
    "Weights" = "#0072B2",
    "Transformation" = "#D55E00",
    "Domain exclusion" = "#009E73"
  )) +
  labs(
    title = "IMD ranks are most sensitive to removing high-weight domains",
    subtitle = "Mean absolute change from the published rank across 33,755 LSOAs",
    x = "Mean absolute rank change",
    y = NULL,
    colour = NULL,
    caption = "Source: MHCLG IoD 2025; analysis using IMD package data"
  )
ggsave(
  file.path(figure_dir, "scenario_rank_sensitivity.png"),
  p_scenarios,
  width = 10,
  height = 7.5,
  dpi = 180
)

p_weights <- weight_oat |>
  mutate(
    change = if_else(relative_change > 0, "+25%", "-25%"),
    domain = reorder(domain, mean_absolute_rank_change)
  ) |>
  ggplot(aes(x = mean_absolute_rank_change, y = domain, colour = change)) +
  geom_point(size = 3, position = position_dodge(width = 0.45)) +
  scale_x_continuous(labels = comma) +
  scale_colour_manual(values = c("-25%" = "#56B4E9", "+25%" = "#D55E00")) +
  labs(
    title = "One-at-a-time domain weight sensitivity",
    subtitle = "Each official weight is varied by 25%; all weights are then rescaled",
    x = "Mean absolute rank change",
    y = NULL,
    colour = "Weight change",
    caption = "Source: MHCLG IoD 2025; analysis using IMD package data"
  )
ggsave(
  file.path(figure_dir, "domain_weight_sensitivity.png"),
  p_weights,
  width = 9,
  height = 5.5,
  dpi = 180
)

p_indicator <- indicator_loo |>
  mutate(omitted_indicator = reorder(omitted_indicator, mean_absolute_rank_change)) |>
  ggplot(aes(x = mean_absolute_rank_change, y = omitted_indicator, colour = domain)) +
  geom_segment(
    aes(x = 0, xend = mean_absolute_rank_change, yend = omitted_indicator),
    colour = "grey82",
    linewidth = 0.6
  ) +
  geom_point(size = 3) +
  scale_x_continuous(labels = comma) +
  scale_colour_manual(values = c("Health" = "#CC79A7", "Crime" = "#E69F00")) +
  labs(
    title = "Indicator omission has a smaller effect on the overall IMD",
    subtitle = "Health and Crime are fully reconstructible from published package indicators",
    x = "Mean absolute IMD rank change",
    y = NULL,
    colour = NULL,
    caption = "Remaining within-domain factor weights are rescaled to sum to one"
  )
ggsave(
  file.path(figure_dir, "indicator_leave_one_out.png"),
  p_indicator,
  width = 10,
  height = 6.5,
  dpi = 180
)

p_uncertainty <- rank_uncertainty |>
  mutate(
    official_rank_group = cut(
      official_rank,
      breaks = seq(0, n_lsoa + ceiling(n_lsoa / 10), length.out = 11),
      labels = paste0("Decile ", 1:10),
      include.lowest = TRUE
    )
  ) |>
  ggplot(aes(x = official_rank_group, y = rank_p90_interval_width, fill = analysis)) +
  geom_boxplot(outlier.alpha = 0.08, outlier.size = 0.4) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("#0072B2", "#D55E00")) +
  labs(
    title = "Rank uncertainty is largest in the middle deciles",
    subtitle = paste0(
      "90% simulation interval from ", comma(n_sims),
      " illustrative Monte Carlo runs per analysis"
    ),
    x = "Published IMD decile",
    y = "Width of 5th-95th percentile rank interval",
    fill = NULL,
    caption = "Stress-test ranges are methodological choices, not statistical confidence intervals"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  file.path(figure_dir, "rank_uncertainty_by_decile.png"),
  p_uncertainty,
  width = 11,
  height = 6.5,
  dpi = 180
)

# Parse the existing LTLA GeoJSON without adding a system-level spatial
# dependency. Only outer rings are required for this national overview map.
geojson_polygons <- function(path) {
  geo <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  polygon_rows <- list()
  group_id <- 0L

  for (feature in geo$features) {
    code <- feature$properties$ltla24_code
    name <- feature$properties$ltla24_name
    geometry_type <- feature$geometry$type
    polygons <- if (geometry_type == "Polygon") {
      list(feature$geometry$coordinates)
    } else {
      feature$geometry$coordinates
    }

    for (polygon in polygons) {
      if (length(polygon) == 0) next
      outer_ring <- polygon[[1]]
      coordinates <- do.call(rbind, lapply(outer_ring, unlist))
      if (nrow(coordinates) < 3) next
      group_id <- group_id + 1L
      polygon_rows[[length(polygon_rows) + 1L]] <- tibble(
        ltla24_code = code,
        ltla24_name = name,
        group = group_id,
        longitude = coordinates[, 1],
        latitude = coordinates[, 2]
      )
    }
  }

  bind_rows(polygon_rows)
}

ltla_map_data <- geojson_polygons("ltla-england.geojson") |>
  left_join(ltla_fallout, by = c("ltla24_code", "ltla24_name"))

p_fallout_map <- ggplot(
  ltla_map_data,
  aes(x = longitude, y = latitude, group = group,
      fill = mean_joint_fallout_frequency)
) +
  geom_polygon(colour = "white", linewidth = 0.08) +
  coord_quickmap() +
  scale_fill_gradientn(
    colours = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    labels = percent_format(accuracy = 1),
    na.value = "#e5e5e5"
  ) +
  labs(
    title = "How often published decile-1 LSOAs fall out under joint stress testing",
    subtitle = paste0(
      "LTLA colour is the average across its published decile-1 LSOAs; ",
      "grey areas contain none"
    ),
    fill = "Mean fall-out\nfrequency",
    caption = paste0(
      comma(n_sims), " simulations: domain weights +/-25%; ",
      "exponential constant 15-35"
    )
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title.position = "plot",
    legend.position = "right",
    plot.caption = element_text(hjust = 0),
    text = element_text(colour = "black"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = "white", colour = NA)
  )

ggsave(
  file.path(figure_dir, "decile1_fallout_frequency_map.png"),
  p_fallout_map,
  width = 8.5,
  height = 9.5,
  dpi = 180
)

message("Analysis complete. Outputs written under ", normalizePath(output_dir))
