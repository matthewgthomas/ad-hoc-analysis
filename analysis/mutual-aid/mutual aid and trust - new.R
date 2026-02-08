suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(geographr)
  library(jsonlite)
  library(lmtest)
  library(readr)
  library(rio)
  library(sf)
  library(tidyr)
})

config <- list(
  paths = list(
    groups_json = "data/groups.json",
    trust_xlsx = "data/good_neighbours_full_data_by_msoa.xlsx",
    optional_covariates_csv = "data/msoa_covariates.csv",
    outputs_dir = "outputs",
    tables_dir = "outputs/tables",
    figures_dir = "outputs/figures",
    analysis_csv = "outputs/analysis_dataset.csv",
    model_results_csv = "outputs/tables/model_results.csv",
    data_qc_csv = "outputs/tables/data_qc_summary.csv",
    diagnostics_csv = "outputs/tables/model_diagnostics.csv",
    vif_csv = "outputs/tables/vif_results.csv",
    covariate_availability_csv = "outputs/tables/covariate_availability.csv",
    acceptance_checks_csv = "outputs/tables/acceptance_checks.csv",
    methods_writeup_md = "outputs/methods_writeup.md",
    excluded_points_csv = "outputs/tables/excluded_groups.csv",
    join_diagnostics_csv = "outputs/tables/join_diagnostics.csv"
  ),
  bbox = list(
    lat_min = 49,
    lat_max = 61,
    lon_min = -9,
    lon_max = 2.5
  ),
  external_covariate_columns = list(
    msoa_code = c("msoa11_code", "MSOA_code"),
    deprivation = c("imd_score", "imd_decile", "deprivation_score"),
    population_density = c("population_density", "pop_density"),
    age_65_plus = c("pct_age_65_plus", "age_65_plus_pct"),
    socioeconomic = c("pct_degree", "nssec_managerial_pct"),
    ethnic_diversity = c("ethnic_diversity", "pct_non_white")
  ),
  sensitivity_top_quantile = 0.99,
  moran_permutations = 199,
  random_seed = 2026
)

ensure_output_dirs <- function(cfg) {
  dir.create(cfg$paths$outputs_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cfg$paths$tables_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cfg$paths$figures_dir, recursive = TRUE, showWarnings = FALSE)
}

choose_first_existing <- function(columns, choices) {
  hit <- choices[choices %in% columns]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

safe_scale <- function(x) {
  s <- stats::sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

robust_hc3_vcov <- function(model) {
  beta <- stats::coef(model)
  keep <- !is.na(beta)
  beta <- beta[keep]
  x <- stats::model.matrix(model)[, keep, drop = FALSE]
  e <- stats::residuals(model)
  h <- stats::hatvalues(model)
  omega <- (e / pmax(1 - h, 1e-8))^2
  xtx_inv <- solve(crossprod(x))
  meat <- crossprod(x, omega * x)
  xtx_inv %*% meat %*% xtx_inv
}

tidy_lm_hc3 <- function(model, model_id, scenario) {
  beta <- stats::coef(model)
  keep <- !is.na(beta)
  beta <- beta[keep]
  vcov_hc3 <- robust_hc3_vcov(model)
  se <- sqrt(diag(vcov_hc3))
  t_stat <- beta / se
  df <- stats::df.residual(model)
  p_val <- 2 * stats::pt(abs(t_stat), df = df, lower.tail = FALSE)
  crit <- stats::qt(0.975, df = df)
  tibble::tibble(
    scenario = scenario,
    model_id = model_id,
    term = names(beta),
    estimate = unname(beta),
    std_error_hc3 = unname(se),
    statistic = unname(t_stat),
    p_value = unname(p_val),
    conf_low = unname(beta - crit * se),
    conf_high = unname(beta + crit * se),
    n_obs = stats::nobs(model),
    r_squared = summary(model)$r.squared,
    adj_r_squared = summary(model)$adj.r.squared
  )
}

compute_vif <- function(model) {
  x <- stats::model.matrix(model)
  if (ncol(x) <= 2) return(tibble::tibble(term = character(), vif = numeric()))
  out <- vector("list", ncol(x) - 1)
  terms <- colnames(x)[-1]
  for (i in seq_along(terms)) {
    y <- x[, i + 1]
    others <- x[, -(i + 1), drop = FALSE]
    fit <- stats::lm.fit(x = others, y = y)
    rss <- sum(fit$residuals^2)
    tss <- sum((y - mean(y))^2)
    r2 <- if (tss == 0) 0 else 1 - rss / tss
    vif <- if (r2 >= 1) Inf else 1 / (1 - r2)
    out[[i]] <- tibble::tibble(term = terms[[i]], vif = vif)
  }
  bind_rows(out)
}

compute_moran_i <- function(residuals, polygons_sf, permutations = 199, seed = 2026) {
  neighbors <- sf::st_touches(polygons_sf)
  z <- residuals - mean(residuals)
  n <- length(z)
  s0 <- sum(lengths(neighbors))
  if (s0 == 0 || n < 3) {
    return(tibble::tibble(
      metric = c("moran_i", "moran_expected", "moran_p_value"),
      value = c(NA_real_, NA_real_, NA_real_)
    ))
  }

  moran_core <- function(z_vec) {
    num <- sum(vapply(
      seq_along(neighbors),
      function(i) {
        if (length(neighbors[[i]]) == 0) return(0)
        z_vec[[i]] * sum(z_vec[neighbors[[i]]])
      },
      numeric(1)
    ))
    den <- sum(z_vec^2)
    (n / s0) * (num / den)
  }

  i_obs <- moran_core(z)
  i_exp <- -1 / (n - 1)
  set.seed(seed)
  i_perm <- replicate(permutations, moran_core(sample(z, replace = FALSE)))
  p_val <- (sum(abs(i_perm) >= abs(i_obs)) + 1) / (permutations + 1)

  tibble::tibble(
    metric = c("moran_i", "moran_expected", "moran_p_value"),
    value = c(i_obs, i_exp, p_val)
  )
}

load_and_clean_groups <- function(cfg, apply_bbox_filter = TRUE) {
  raw <- jsonlite::read_json(cfg$paths$groups_json, simplifyVector = TRUE)
  groups <- tibble::as_tibble(raw)

  if ("location_coord" %in% names(groups)) {
    groups <- bind_cols(groups, tibble::as_tibble(groups$location_coord))
  } else if (all(c("location_coord.lng", "location_coord.lat") %in% names(groups))) {
    groups <- groups |>
      rename(
        lng = location_coord.lng,
        lat = location_coord.lat
      )
  }

  groups <- groups |>
    mutate(
      lat = suppressWarnings(as.numeric(lat)),
      lng = suppressWarnings(as.numeric(lng)),
      duplicate_id = duplicated(id),
      missing_coord = !is.finite(lat) | !is.finite(lng),
      outside_bbox = !missing_coord & (
        lat < cfg$bbox$lat_min |
          lat > cfg$bbox$lat_max |
          lng < cfg$bbox$lon_min |
          lng > cfg$bbox$lon_max
      ),
      excluded_reason = case_when(
        duplicate_id ~ "duplicate_id",
        missing_coord ~ "missing_coord",
        apply_bbox_filter & outside_bbox ~ "outside_bbox",
        TRUE ~ NA_character_
      )
    )

  excluded <- groups |>
    filter(!is.na(excluded_reason)) |>
    transmute(
      id, name, location_name, lat, lng, excluded_reason,
      bbox_filter_applied = apply_bbox_filter
    )

  cleaned <- groups |>
    filter(is.na(excluded_reason)) |>
    select(-duplicate_id, -missing_coord, -outside_bbox, -excluded_reason)

  qc_summary <- tibble::tribble(
    ~section, ~metric, ~value, ~bbox_filter_applied,
    "groups_cleaning", "raw_rows", nrow(groups), apply_bbox_filter,
    "groups_cleaning", "retained_rows", nrow(cleaned), apply_bbox_filter,
    "groups_cleaning", "excluded_duplicate_id", sum(groups$duplicate_id), apply_bbox_filter,
    "groups_cleaning", "excluded_missing_coord", sum(groups$missing_coord), apply_bbox_filter,
    "groups_cleaning", "excluded_outside_bbox", sum(apply_bbox_filter & groups$outside_bbox), apply_bbox_filter
  )

  list(cleaned = cleaned, excluded = excluded, qc_summary = qc_summary)
}

assign_groups_to_msoa <- function(clean_groups, boundaries) {
  groups_sf <- sf::st_as_sf(clean_groups, coords = c("lng", "lat"), crs = 4326, remove = FALSE)
  boundaries_proj <- sf::st_transform(boundaries, sf::st_crs(groups_sf))

  joined <- sf::st_join(
    groups_sf,
    boundaries_proj |> select(msoa11_code),
    join = sf::st_intersects,
    left = TRUE
  )

  joined_tbl <- joined |>
    sf::st_drop_geometry() |>
    mutate(msoa_matched = !is.na(msoa11_code))

  msoa_counts <- joined_tbl |>
    filter(msoa_matched) |>
    count(msoa11_code, name = "n_groups")

  diagnostics <- tibble::tribble(
    ~section, ~metric, ~value,
    "spatial_join", "total_groups", nrow(joined_tbl),
    "spatial_join", "matched_groups", sum(joined_tbl$msoa_matched),
    "spatial_join", "unmatched_groups", sum(!joined_tbl$msoa_matched),
    "spatial_join", "match_rate", mean(joined_tbl$msoa_matched)
  )

  list(joined = joined_tbl, msoa_counts = msoa_counts, diagnostics = diagnostics)
}

build_covariates <- function(cfg) {
  base <- boundaries_msoa11 |>
    sf::st_drop_geometry() |>
    select(msoa11_code)

  region_lookup <- lookup_msoa11_ltla21 |>
    select(msoa11_code, ltla21_code) |>
    left_join(
      lookup_ltla21_region21 |>
        select(ltla21_code, region21_name),
      by = "ltla21_code"
    ) |>
    mutate(
      region_fe = case_when(
        !is.na(region21_name) ~ region21_name,
        substr(msoa11_code, 1, 1) == "W" ~ "Wales",
        substr(msoa11_code, 1, 1) == "S" ~ "Scotland",
        substr(msoa11_code, 1, 1) == "N" ~ "Northern Ireland",
        TRUE ~ "Unknown"
      )
    ) |>
    select(msoa11_code, region_fe)

  ruc_lookup <- ruc11_msoa11 |>
    select(msoa11_code, ruc, classification) |>
    rename(urban_rural = ruc)

  covariates <- base |>
    left_join(region_lookup, by = "msoa11_code") |>
    left_join(ruc_lookup, by = "msoa11_code")

  availability <- tibble::tribble(
    ~category, ~status, ~column_used, ~source,
    "deprivation", "missing", NA_character_, "optional external covariates",
    "population_density", "missing", NA_character_, "optional external covariates",
    "age_65_plus", "missing", NA_character_, "optional external covariates",
    "socioeconomic", "missing", NA_character_, "optional external covariates",
    "ethnic_diversity", "missing", NA_character_, "optional external covariates",
    "urban_rural", "available", "urban_rural", "geographr::ruc11_msoa11"
  )

  extra_numeric <- character()
  if (file.exists(cfg$paths$optional_covariates_csv)) {
    ext <- readr::read_csv(cfg$paths$optional_covariates_csv, show_col_types = FALSE)
    ext_code <- choose_first_existing(
      names(ext),
      cfg$external_covariate_columns$msoa_code
    )
    if (!is.na(ext_code)) {
      ext <- ext |>
        rename(msoa11_code = all_of(ext_code))
      for (category in names(cfg$external_covariate_columns)[-1]) {
        col <- choose_first_existing(names(ext), cfg$external_covariate_columns[[category]])
        if (!is.na(col)) {
          availability$status[availability$category == category] <- "available"
          availability$column_used[availability$category == category] <- col
          extra_numeric <- c(extra_numeric, col)
        }
      }

      keep_cols <- c("msoa11_code", unique(extra_numeric))
      covariates <- covariates |>
        left_join(ext |> select(any_of(keep_cols)), by = "msoa11_code")
      availability$source[availability$status == "available" &
                            availability$source == "optional external covariates"] <-
        cfg$paths$optional_covariates_csv
    }
  }

  list(
    covariates = covariates,
    availability = availability,
    numeric_covariates = unique(extra_numeric),
    factor_covariates = c("urban_rural")
  )
}

build_analysis_dataset <- function(cfg, apply_bbox_filter = TRUE) {
  boundaries <- boundaries_msoa11 |>
    sf::st_make_valid()

  groups <- load_and_clean_groups(cfg, apply_bbox_filter = apply_bbox_filter)
  group_assignment <- assign_groups_to_msoa(groups$cleaned, boundaries)
  covariate_data <- build_covariates(cfg)

  msoa_base <- boundaries |>
    sf::st_drop_geometry() |>
    select(msoa11_code, msoa11_name) |>
    left_join(group_assignment$msoa_counts, by = "msoa11_code") |>
    mutate(
      n_groups = dplyr::coalesce(as.integer(n_groups), 0L),
      any_group = n_groups > 0L,
      log_groups = log1p(n_groups)
    ) |>
    left_join(covariate_data$covariates, by = "msoa11_code")

  trust <- rio::import(cfg$paths$trust_xlsx) |>
    mutate(MSOA_code = as.character(MSOA_code))

  full_joined <- msoa_base |>
    left_join(trust, by = join_by(msoa11_code == MSOA_code)) |>
    mutate(trust_matched = !is.na(Net_trust))

  trust_join_summary <- full_joined |>
    group_by(trust_matched) |>
    summarise(
      msoa_n = n(),
      mean_net_trust = mean(Net_trust, na.rm = TRUE),
      mean_n_groups = mean(n_groups, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(section = "trust_join")

  analysis_df <- full_joined |>
    filter(!is.na(Net_trust)) |>
    mutate(
      region_fe = as.factor(region_fe),
      urban_rural = as.factor(urban_rural),
      classification = as.factor(classification)
    )

  numeric_covars <- covariate_data$numeric_covariates
  for (col in numeric_covars) {
    z_col <- paste0("z_", col)
    analysis_df[[z_col]] <- safe_scale(analysis_df[[col]])
  }

  analysis_sf <- boundaries |>
    select(msoa11_code, geometry) |>
    left_join(analysis_df |> select(msoa11_code, Net_trust, any_group, n_groups, log_groups), by = "msoa11_code")

  data_qc <- bind_rows(
    groups$qc_summary |>
      mutate(section = as.character(section), metric = as.character(metric), value = as.numeric(value)),
    group_assignment$diagnostics |>
      mutate(bbox_filter_applied = apply_bbox_filter),
    trust_join_summary |>
      tidyr::pivot_longer(
        cols = c(msoa_n, mean_net_trust, mean_n_groups),
        names_to = "metric",
        values_to = "value"
      ) |>
      mutate(
        section = paste0(section, "_", ifelse(trust_matched, "matched", "unmatched")),
        bbox_filter_applied = apply_bbox_filter
      ) |>
      select(section, metric, value, bbox_filter_applied)
  )

  list(
    analysis_df = analysis_df,
    analysis_sf = analysis_sf,
    excluded_groups = groups$excluded,
    data_qc = data_qc,
    covariate_availability = covariate_data$availability,
    numeric_covariates_z = paste0("z_", numeric_covars),
    factor_covariates = covariate_data$factor_covariates,
    trust_nonmissing_n = sum(!is.na(trust$Net_trust)),
    spatial_joined_groups = group_assignment$joined
  )
}

run_models <- function(analysis_df, analysis_sf, numeric_covariates_z, factor_covariates, scenario, cfg) {
  rhs_covars <- c(numeric_covariates_z, factor_covariates)
  rhs_covars <- rhs_covars[rhs_covars %in% names(analysis_df)]

  adjusted_terms <- c(rhs_covars, "factor(region_fe)")
  adjusted_rhs <- if (length(adjusted_terms) > 0) paste(adjusted_terms, collapse = " + ") else "1"

  formulas <- list(
    M0 = stats::as.formula("Net_trust ~ any_group"),
    M1 = stats::as.formula("Net_trust ~ log_groups"),
    M2 = stats::as.formula(paste("Net_trust ~ any_group +", adjusted_rhs)),
    M3 = stats::as.formula(paste("Net_trust ~ log_groups +", adjusted_rhs))
  )

  fits <- lapply(formulas, function(fm) stats::lm(fm, data = analysis_df))

  model_results <- bind_rows(
    tidy_lm_hc3(fits$M0, "M0", scenario),
    tidy_lm_hc3(fits$M1, "M1", scenario),
    tidy_lm_hc3(fits$M2, "M2", scenario),
    tidy_lm_hc3(fits$M3, "M3", scenario)
  )

  diagnostics <- tibble::tibble(
    scenario = scenario,
    model_id = names(fits),
    n_obs = vapply(fits, stats::nobs, numeric(1)),
    r_squared = vapply(fits, function(m) summary(m)$r.squared, numeric(1)),
    adj_r_squared = vapply(fits, function(m) summary(m)$adj.r.squared, numeric(1)),
    aic = vapply(fits, stats::AIC, numeric(1))
  )

  vif_tbl <- compute_vif(fits$M2) |>
    mutate(scenario = scenario, model_id = "M2") |>
    select(scenario, model_id, term, vif)

  moran_tbl <- compute_moran_i(
    residuals = stats::residuals(fits$M2),
    polygons_sf = analysis_sf |> filter(!is.na(Net_trust)),
    permutations = cfg$moran_permutations,
    seed = cfg$random_seed
  ) |>
    mutate(scenario = scenario, model_id = "M2") |>
    select(scenario, model_id, metric, value)

  list(
    fits = fits,
    model_results = model_results,
    diagnostics = diagnostics,
    vif = vif_tbl,
    moran = moran_tbl
  )
}

export_outputs <- function(cfg, baseline, model_results, diagnostics, vif_results, moran_results, acceptance_checks) {
  ensure_output_dirs(cfg)

  readr::write_csv(baseline$analysis_df, cfg$paths$analysis_csv)
  readr::write_csv(model_results, cfg$paths$model_results_csv)
  readr::write_csv(baseline$data_qc, cfg$paths$data_qc_csv)
  readr::write_csv(diagnostics, cfg$paths$diagnostics_csv)
  readr::write_csv(vif_results, cfg$paths$vif_csv)
  readr::write_csv(baseline$covariate_availability, cfg$paths$covariate_availability_csv)
  readr::write_csv(acceptance_checks, cfg$paths$acceptance_checks_csv)
  readr::write_csv(baseline$excluded_groups, cfg$paths$excluded_points_csv)
  readr::write_csv(
    baseline$spatial_joined_groups |>
      count(msoa_matched, name = "n"),
    cfg$paths$join_diagnostics_csv
  )

  plot_box <- ggplot(baseline$analysis_df, aes(x = any_group, y = Net_trust)) +
    geom_boxplot(fill = "#6bb6ff", alpha = 0.8, outlier.alpha = 0.2) +
    labs(
      title = "Net trust by mutual aid group presence",
      x = "Any mutual aid group in MSOA",
      y = "Net trust"
    ) +
    theme_minimal(base_size = 11)
  ggsave(
    filename = file.path(cfg$paths$figures_dir, "trust_by_group_boxplot.png"),
    plot = plot_box,
    width = 8,
    height = 5,
    dpi = 300
  )

  plot_scatter <- ggplot(baseline$analysis_df, aes(x = log_groups, y = Net_trust)) +
    geom_jitter(width = 0.05, height = 0, alpha = 0.25, color = "#1f3b4d") +
    geom_smooth(method = "lm", se = TRUE, color = "#c0392b", linewidth = 0.9) +
    labs(
      title = "Net trust vs log(1 + number of groups)",
      x = "log(1 + n_groups)",
      y = "Net trust"
    ) +
    theme_minimal(base_size = 11)
  ggsave(
    filename = file.path(cfg$paths$figures_dir, "trust_vs_log_groups_scatter.png"),
    plot = plot_scatter,
    width = 8,
    height = 5,
    dpi = 300
  )

  map_plot <- ggplot() +
    geom_sf(
      data = baseline$analysis_sf,
      aes(fill = Net_trust),
      color = NA
    ) +
    geom_sf(
      data = baseline$analysis_sf |> filter(any_group),
      fill = NA,
      color = "black",
      linewidth = 0.03,
      alpha = 0.45
    ) +
    scale_fill_gradient2(
      low = "#b2182b",
      mid = "#f7f7f7",
      high = "#2166ac",
      midpoint = 0
    ) +
    labs(
      title = "MSOA net trust (fill) with mutual aid presence outlines",
      fill = "Net trust"
    ) +
    theme_void(base_size = 10)
  ggsave(
    filename = file.path(cfg$paths$figures_dir, "msoa_map_groups_trust.png"),
    plot = map_plot,
    width = 9,
    height = 10,
    dpi = 300
  )

  main_fit <- stats::lm(
    Net_trust ~ any_group + urban_rural + factor(region_fe),
    data = baseline$analysis_df
  )
  residual_plot <- tibble::tibble(
    fitted = fitted(main_fit),
    residuals = residuals(main_fit)
  ) |>
    ggplot(aes(x = fitted, y = residuals)) +
    geom_point(alpha = 0.25, color = "#34495e") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#c0392b") +
    labs(
      title = "Residuals vs fitted (M2-style model)",
      x = "Fitted values",
      y = "Residuals"
    ) +
    theme_minimal(base_size = 11)
  ggsave(
    filename = file.path(cfg$paths$figures_dir, "m2_residuals_vs_fitted.png"),
    plot = residual_plot,
    width = 8,
    height = 5,
    dpi = 300
  )

  cooks <- stats::cooks.distance(main_fit)
  cook_plot <- tibble::tibble(index = seq_along(cooks), cooks_distance = cooks) |>
    ggplot(aes(x = index, y = cooks_distance)) +
    geom_col(fill = "#6c757d", width = 0.9) +
    labs(
      title = "Cook's distance (M2-style model)",
      x = "Observation index",
      y = "Cook's distance"
    ) +
    theme_minimal(base_size = 11)
  ggsave(
    filename = file.path(cfg$paths$figures_dir, "m2_cooks_distance.png"),
    plot = cook_plot,
    width = 8,
    height = 5,
    dpi = 300
  )

  writeLines(
    con = cfg$paths$methods_writeup_md,
    text = c(
      "# Methods: Mutual Aid Presence and Net Trust at MSOA Level",
      "",
      "## 1) Data sources and linkage",
      "- Mutual aid groups: `data/groups.json`",
      "- Trust outcome: `data/good_neighbours_full_data_by_msoa.xlsx`",
      "- Geography and lookup data: `geographr` (`boundaries_msoa11`, region lookups, rural/urban classification)",
      "- Linkage steps: (a) geocode groups to MSOAs via spatial join, (b) aggregate to MSOA-level exposure, (c) join trust by `msoa11_code == MSOA_code`.",
      "",
      "## 2) Exposure and outcome definitions",
      "- Binary exposure: `any_group = n_groups > 0`.",
      "- Intensity exposure: `log_groups = log1p(n_groups)`.",
      "- Outcome: `Net_trust` from the trust spreadsheet.",
      "",
      "## 3) Cleaning and exclusions",
      "- Duplicated group IDs are excluded (keep first instance only).",
      "- Missing/invalid coordinates are excluded.",
      "- Primary specification excludes obvious non-UK points via bounding box: lat 49-61, lon -9 to 2.5.",
      "- All exclusions are logged in `outputs/tables/excluded_groups.csv`.",
      "",
      "## 4) Model specifications",
      "- M0: `Net_trust ~ any_group`.",
      "- M1: `Net_trust ~ log_groups`.",
      "- M2: `Net_trust ~ any_group + available covariates + region fixed effects`.",
      "- M3: `Net_trust ~ log_groups + available covariates + region fixed effects`.",
      "- Inference: heteroskedasticity-robust HC3 standard errors and 95% confidence intervals.",
      "",
      "## 5) Sensitivity checks",
      "- Excluding top 1% of `n_groups` MSOAs.",
      "- Re-estimating with and without bounding-box filtering.",
      "- Comparing binary and intensity exposures.",
      "",
      "## 6) Interpretation boundaries",
      "- This is an association analysis, not a causal identification design.",
      "- Region FE and available controls reduce but do not eliminate omitted-variable bias.",
      "- Some requested covariates (deprivation, population density, age, socioeconomic mix, ethnic diversity) are only used when available from `data/msoa_covariates.csv`."
    )
  )
}

baseline <- build_analysis_dataset(config, apply_bbox_filter = TRUE)
no_bbox <- build_analysis_dataset(config, apply_bbox_filter = FALSE)

threshold_top1 <- stats::quantile(
  baseline$analysis_df$n_groups,
  probs = config$sensitivity_top_quantile,
  na.rm = TRUE
)
top1_trimmed_df <- baseline$analysis_df |>
  filter(n_groups <= threshold_top1)
top1_trimmed_sf <- baseline$analysis_sf |>
  semi_join(top1_trimmed_df |> select(msoa11_code), by = "msoa11_code")

models_baseline <- run_models(
  analysis_df = baseline$analysis_df,
  analysis_sf = baseline$analysis_sf,
  numeric_covariates_z = baseline$numeric_covariates_z,
  factor_covariates = baseline$factor_covariates,
  scenario = "bbox_on_full",
  cfg = config
)
models_no_bbox <- run_models(
  analysis_df = no_bbox$analysis_df,
  analysis_sf = no_bbox$analysis_sf,
  numeric_covariates_z = no_bbox$numeric_covariates_z,
  factor_covariates = no_bbox$factor_covariates,
  scenario = "bbox_off_full",
  cfg = config
)
models_top1 <- run_models(
  analysis_df = top1_trimmed_df,
  analysis_sf = top1_trimmed_sf,
  numeric_covariates_z = baseline$numeric_covariates_z,
  factor_covariates = baseline$factor_covariates,
  scenario = "bbox_on_trim_top1pct",
  cfg = config
)

model_results_all <- bind_rows(
  models_baseline$model_results,
  models_no_bbox$model_results,
  models_top1$model_results
)

diagnostics_all <- bind_rows(
  models_baseline$diagnostics,
  models_no_bbox$diagnostics,
  models_top1$diagnostics,
  models_baseline$moran,
  models_no_bbox$moran,
  models_top1$moran
)

vif_all <- bind_rows(
  models_baseline$vif,
  models_no_bbox$vif,
  models_top1$vif
)

models_baseline_repeat <- run_models(
  analysis_df = baseline$analysis_df,
  analysis_sf = baseline$analysis_sf,
  numeric_covariates_z = baseline$numeric_covariates_z,
  factor_covariates = baseline$factor_covariates,
  scenario = "bbox_on_full_repeat",
  cfg = config
)

coef_compare <- models_baseline$model_results |>
  select(model_id, term, estimate) |>
  inner_join(
    models_baseline_repeat$model_results |>
      select(model_id, term, estimate_repeat = estimate),
    by = c("model_id", "term")
  ) |>
  mutate(abs_diff = abs(estimate - estimate_repeat))

acceptance_checks <- tibble::tribble(
  ~check_name, ~status, ~details,
  "join_integrity",
  ifelse(
    nrow(baseline$analysis_df) == baseline$trust_nonmissing_n,
    "PASS",
    "FAIL"
  ),
  paste0("analysis_rows=", nrow(baseline$analysis_df), "; trust_nonmissing=", baseline$trust_nonmissing_n),
  "exposure_construction",
  ifelse(all(baseline$analysis_df$any_group == (baseline$analysis_df$n_groups > 0)), "PASS", "FAIL"),
  "all(any_group == n_groups > 0)",
  "cleaning_correctness",
  ifelse(
    all(!duplicated(baseline$spatial_joined_groups$id)) &&
      all(baseline$spatial_joined_groups$lat >= config$bbox$lat_min &
            baseline$spatial_joined_groups$lat <= config$bbox$lat_max &
            baseline$spatial_joined_groups$lng >= config$bbox$lon_min &
            baseline$spatial_joined_groups$lng <= config$bbox$lon_max),
    "PASS",
    "FAIL"
  ),
  "retained points are unique by id and inside UK bbox",
  "model_reproducibility",
  ifelse(max(coef_compare$abs_diff, na.rm = TRUE) < 1e-8, "PASS", "FAIL"),
  paste0("max_abs_diff=", signif(max(coef_compare$abs_diff, na.rm = TRUE), 4))
)

export_outputs(
  cfg = config,
  baseline = baseline,
  model_results = model_results_all,
  diagnostics = diagnostics_all,
  vif_results = vif_all,
  moran_results = bind_rows(models_baseline$moran, models_no_bbox$moran, models_top1$moran),
  acceptance_checks = acceptance_checks
)

required_files <- c(
  config$paths$model_results_csv,
  config$paths$data_qc_csv,
  file.path(config$paths$figures_dir, "trust_by_group_boxplot.png"),
  file.path(config$paths$figures_dir, "trust_vs_log_groups_scatter.png"),
  file.path(config$paths$figures_dir, "msoa_map_groups_trust.png")
)

file_check <- tibble::tibble(
  check_name = "reporting_completeness",
  status = ifelse(all(file.exists(required_files) & file.info(required_files)$size > 0), "PASS", "FAIL"),
  details = paste(required_files, collapse = "; ")
)

acceptance_checks <- bind_rows(acceptance_checks, file_check)
readr::write_csv(acceptance_checks, config$paths$acceptance_checks_csv)

message("Analysis complete. Outputs written to: ", config$paths$outputs_dir)
