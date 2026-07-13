make_survey_design <- function(data) {
  options(survey.lonely.psu = "adjust")
  survey::svydesign(
    ids = ~SDMVPSU,
    strata = ~SDMVSTRA,
    weights = ~MEC4YR,
    nest = TRUE,
    data = data
  )
}

model_formula <- function(outcome, type = c("core", "extended"), gam = FALSE) {
  type <- match.arg(type)
  if (gam) {
    rhs <- c("s(age, k = 8, bs = 'cr')", "s(bmi, k = 6, bs = 'cr')",
             "s(height_cm, k = 6, bs = 'cr')")
    if (type == "extended") {
      rhs <- c(rhs, "s(arm_circumference_cm, k = 6, bs = 'cr')",
               "s(arm_length_cm, k = 5, bs = 'cr')", "s(log_activity, k = 6, bs = 'cr')",
               "handedness", "any_hand_pain", "prior_surgery", "posture")
    }
  } else {
    rhs <- c("splines::ns(age, df = 6)", "splines::ns(bmi, df = 4)",
             "splines::ns(height_cm, df = 4)")
    if (type == "extended") {
      rhs <- c(rhs, "splines::ns(arm_circumference_cm, df = 4)",
               "splines::ns(arm_length_cm, df = 3)", "log_activity",
               "handedness", "any_hand_pain", "prior_surgery", "posture")
    }
  }
  stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
}

model_data <- function(data, sex_value, outcome, type) {
  base <- c(outcome, "age", "bmi", "height_cm", "MEC4YR", "SDMVPSU", "SDMVSTRA",
            "SEQN", "cycle", "cluster_id", "sex", "age_band")
  if (type == "extended") {
    base <- c(base, "arm_circumference_cm", "arm_length_cm", "log_activity",
              "handedness", "any_hand_pain", "prior_surgery", "posture")
  }
  data |>
    dplyr::filter(as.character(sex) == sex_value) |>
    dplyr::select(dplyr::all_of(unique(base))) |>
    dplyr::filter(stats::complete.cases(dplyr::across(dplyr::everything()))) |>
    droplevels()
}

fit_one_survey_model <- function(data, outcome, type) {
  design <- make_survey_design(data)
  survey::svyglm(model_formula(outcome, type, gam = FALSE), design = design,
                 family = gaussian())
}

fit_one_gam <- function(data, outcome, type = "core") {
  data$.model_weight <- data$MEC4YR / mean(data$MEC4YR)
  mgcv::gam(model_formula(outcome, type, gam = TRUE), data = data, weights = .model_weight,
            method = "REML", family = gaussian())
}

assign_cluster_folds <- function(data, k = 5L, seed = 20260713L) {
  clusters <- data |>
    dplyr::distinct(cluster_id, cycle) |>
    dplyr::arrange(cycle, as.character(cluster_id))
  set.seed(seed)
  clusters <- clusters |>
    dplyr::group_by(cycle) |>
    dplyr::mutate(.random_order = sample.int(dplyr::n()),
                  fold = rep(seq_len(k), length.out = dplyr::n())[order(.random_order)]) |>
    dplyr::ungroup() |>
    dplyr::select(-.random_order)
  data |>
    dplyr::left_join(clusters, by = c("cluster_id", "cycle"), relationship = "many-to-one")
}

calibration_stats <- function(actual, predicted, w) {
  keep <- is.finite(actual) & is.finite(predicted) & is.finite(w) & w > 0
  if (sum(keep) < 3L) return(data.frame(calibration_intercept = NA_real_, calibration_slope = NA_real_))
  fit <- stats::lm(actual[keep] ~ predicted[keep], weights = w[keep])
  data.frame(calibration_intercept = unname(stats::coef(fit)[1]),
             calibration_slope = unname(stats::coef(fit)[2]))
}

cross_validate_model <- function(data, outcome, type = "core", engine = c("survey", "gam"), k = 5L) {
  engine <- match.arg(engine)
  d <- assign_cluster_folds(data, k = k)
  predicted <- rep(NA_real_, nrow(d))
  for (fold_i in seq_len(k)) {
    train <- droplevels(d[d$fold != fold_i, , drop = FALSE])
    test <- d[d$fold == fold_i, , drop = FALSE]
    fit <- if (engine == "survey") fit_one_survey_model(train, outcome, type) else fit_one_gam(train, outcome, type)
    predicted[d$fold == fold_i] <- as.numeric(stats::predict(fit, newdata = test, type = "response"))
  }
  dplyr::transmute(
    d, SEQN, cycle, cluster_id, fold, sex = as.character(sex), age, age_band,
    outcome = outcome, model_type = type, engine = engine,
    actual = .data[[outcome]], predicted = predicted,
    residual = actual - predicted, weight = MEC4YR
  )
}

cycle_holdout_predictions <- function(data, outcome, type = "core") {
  cycles <- unique(as.character(data$cycle))
  dplyr::bind_rows(lapply(cycles, function(holdout) {
    train <- droplevels(data[as.character(data$cycle) != holdout, , drop = FALSE])
    test <- data[as.character(data$cycle) == holdout, , drop = FALSE]
    fit <- fit_one_survey_model(train, outcome, type)
    actual_values <- test[[outcome]]
    tibble::tibble(
      SEQN = test$SEQN, sex = as.character(test$sex), outcome = outcome,
      model_type = type, training_cycle = setdiff(cycles, holdout)[1], holdout_cycle = holdout,
      actual = actual_values, predicted = as.numeric(stats::predict(fit, newdata = test)),
      weight = test$MEC4YR
    )
  }))
}

summarise_predictions <- function(predictions) {
  predictions |>
    dplyr::group_by(sex, outcome, model_type, engine) |>
    dplyr::group_modify(~ dplyr::bind_cols(
      weighted_metrics(.x$actual, .x$predicted, .x$weight),
      calibration_stats(.x$actual, .x$predicted, .x$weight)
    )) |>
    dplyr::ungroup()
}

summarise_cycle_predictions <- function(predictions) {
  predictions |>
    dplyr::group_by(sex, outcome, model_type, holdout_cycle) |>
    dplyr::group_modify(~ dplyr::bind_cols(
      weighted_metrics(.x$actual, .x$predicted, .x$weight),
      calibration_stats(.x$actual, .x$predicted, .x$weight)
    )) |>
    dplyr::ungroup()
}

add_interval_coverage <- function(predictions) {
  predictions |>
    dplyr::group_by(sex, outcome, model_type, engine, age_band) |>
    dplyr::mutate(
      residual_lo = weighted_quantile(residual, weight, c(0.025))[1],
      residual_hi = weighted_quantile(residual, weight, c(0.975))[1],
      covered = actual >= predicted + residual_lo & actual <= predicted + residual_hi
    ) |>
    dplyr::ungroup()
}

fit_grip_models <- function(data,
                            output_path = "models/grip_models.rds",
                            metrics_path = "artifacts/model_metrics.csv",
                            seed = 20260713L) {
  set.seed(seed)
  outcomes <- c("best_single_grip", "bilateral_grip")
  sexes <- c("Female", "Male")
  final_models <- list()
  gam_models <- list()
  sensitivity_cohort_models <- list()
  oof <- list()
  cycle_oof <- list()
  for (sex_value in sexes) {
    for (outcome in outcomes) {
      for (type in c("core", "extended")) {
        d <- model_data(data, sex_value, outcome, type)
        key <- paste(outcome, sex_value, type, sep = "__")
        final_models[[key]] <- fit_one_survey_model(d, outcome, type)
        oof[[paste(key, "survey", sep = "__")]] <- cross_validate_model(d, outcome, type, "survey")
        if (type == "core") {
          gam_models[[key]] <- fit_one_gam(d, outcome, type)
          oof[[paste(key, "gam", sep = "__")]] <- cross_validate_model(d, outcome, type, "gam")
          cycle_oof[[key]] <- cycle_holdout_predictions(d, outcome, type)
        }
      }
      sensitivity_data <- model_data(data[data$sensitivity_eligible, , drop = FALSE],
                                     sex_value, outcome, "core")
      sensitivity_key <- paste(outcome, sex_value, "standing_no_pain", sep = "__")
      sensitivity_cohort_models[[sensitivity_key]] <- fit_one_survey_model(sensitivity_data, outcome, "core")
      sensitivity_oof <- cross_validate_model(sensitivity_data, outcome, "core", "survey")
      sensitivity_oof$model_type <- "standing_no_pain"
      oof[[paste(sensitivity_key, "survey", sep = "__")]] <- sensitivity_oof
    }
  }
  oof <- add_interval_coverage(dplyr::bind_rows(oof))
  cycle_oof <- dplyr::bind_rows(cycle_oof)
  metrics <- summarise_predictions(oof) |>
    dplyr::left_join(
      oof |>
        dplyr::group_by(sex, outcome, model_type, engine) |>
        dplyr::summarise(interval_coverage = stats::weighted.mean(covered, weight, na.rm = TRUE), .groups = "drop"),
      by = c("sex", "outcome", "model_type", "engine")
    )
  cycle_metrics <- summarise_cycle_predictions(cycle_oof)
  subgroup_calibration <- oof |>
    dplyr::filter(engine == "survey", model_type == "core") |>
    dplyr::left_join(data |>
                       dplyr::select(SEQN, race_ethnicity),
                     by = "SEQN", relationship = "many-to-one") |>
    dplyr::filter(!is.na(race_ethnicity)) |>
    dplyr::group_by(sex, outcome, race_ethnicity) |>
    dplyr::group_modify(~ dplyr::bind_cols(
      tibble::tibble(n = nrow(.x), effective_n = effective_sample_size(.x$weight),
                     mean_error = stats::weighted.mean(.x$actual - .x$predicted, .x$weight)),
      calibration_stats(.x$actual, .x$predicted, .x$weight)
    )) |>
    dplyr::ungroup()
  residual_reference <- oof |>
    dplyr::filter(engine == "survey") |>
    dplyr::select(SEQN, sex, outcome, model_type, age, age_band, residual, weight, fold, cluster_id)
  ranges <- data |>
    dplyr::filter(core_eligible) |>
    dplyr::group_by(sex) |>
    dplyr::summarise(dplyr::across(c(age, bmi, height_cm, arm_circumference_cm, arm_length_cm,
                                     activity_met_min_week),
                                   list(min = ~ min(.x, na.rm = TRUE), max = ~ max(.x, na.rm = TRUE))),
                     .groups = "drop")
  bundle <- list(
    survey_models = final_models,
    gam_models = gam_models,
    sensitivity_cohort_models = sensitivity_cohort_models,
    oof_predictions = oof,
    cycle_predictions = cycle_oof,
    metrics = metrics,
    cycle_metrics = cycle_metrics,
    subgroup_calibration = subgroup_calibration,
    residual_reference = residual_reference,
    ranges = ranges,
    outcomes = outcomes,
    seed = seed,
    fitted_at = Sys.time(),
    software = list(R = R.version.string, survey = as.character(utils::packageVersion("survey")),
                    mgcv = as.character(utils::packageVersion("mgcv")))
  )
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(metrics_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(bundle, output_path, compress = "xz")
  utils::write.csv(metrics, metrics_path, row.names = FALSE)
  utils::write.csv(cycle_metrics, sub("\\.csv$", "_cycle_holdouts.csv", metrics_path), row.names = FALSE)
  bundle
}
