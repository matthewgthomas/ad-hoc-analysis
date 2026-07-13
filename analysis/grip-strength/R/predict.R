select_residual_reference <- function(person, models, model_type) {
  ref <- models$residual_reference |>
    dplyr::filter(sex == person$sex, outcome == person$outcome, .data$model_type == .env$model_type)
  target_band <- as.character(age_band_value(person$age))
  local <- ref[as.character(ref$age_band) == target_band, , drop = FALSE]
  if (effective_sample_size(local$weight) >= 100) {
    local$reference_scope <- paste0(person$sex, ", age ", target_band)
    return(local)
  }
  levels_band <- levels(age_band_value(18:80))
  idx <- match(target_band, levels_band)
  for (radius in seq_along(levels_band)) {
    bands <- levels_band[pmax(1, idx - radius):pmin(length(levels_band), idx + radius)]
    expanded <- ref[as.character(ref$age_band) %in% bands, , drop = FALSE]
    if (effective_sample_size(expanded$weight) >= 100) {
      expanded$reference_scope <- paste0(person$sex, ", age bands ", paste(bands, collapse = ", "))
      return(expanded)
    }
  }
  ref$reference_scope <- paste0(person$sex, ", all adult ages")
  ref
}

predict_grip <- function(person, models) {
  person <- normalise_person(person)
  type <- if (advanced_person_complete(person)) "extended" else "core"
  key <- paste(person$outcome, person$sex, type, sep = "__")
  fit <- models$survey_models[[key]]
  if (is.null(fit)) stop("No fitted model is available for that combination.", call. = FALSE)
  newdata <- person_as_newdata(person, extended = identical(type, "extended"))
  expected <- as.numeric(stats::predict(fit, newdata = newdata, type = "response"))
  ref <- select_residual_reference(person, models, type)
  rq <- weighted_quantile(ref$residual, ref$weight, c(0.025, 0.975))
  gam_key <- paste(person$outcome, person$sex, "core", sep = "__")
  gam_pred <- NA_real_
  if (!is.null(models$gam_models[[gam_key]])) {
    gam_pred <- as.numeric(stats::predict(models$gam_models[[gam_key]],
                                          newdata = person_as_newdata(person, extended = FALSE),
                                          type = "response"))
  }
  list(
    person = person,
    outcome = person$outcome,
    sex = person$sex,
    model_type = type,
    predicted = expected,
    lower = expected + rq[[1]],
    upper = expected + rq[[2]],
    gam_sensitivity = gam_pred,
    residual_reference = ref,
    residual_effective_n = effective_sample_size(ref$weight),
    reference_scope = unique(ref$reference_scope)[1]
  )
}

compare_grip <- function(person, prediction, residual_reference = prediction$residual_reference,
                         analytic_data = NULL) {
  person <- normalise_person(person)
  observed <- person$observed_grip_kg
  if (!is.finite(observed)) {
    return(list(performed = FALSE, message = "No observed grip was supplied; no percentile or unusualness test was run."))
  }
  residual <- observed - prediction$predicted
  f <- weighted_ecdf_value(residual_reference$residual, residual_reference$weight, residual)
  p <- min(1, 2 * min(f, 1 - f))
  adjusted_percentile <- 100 * f
  raw_percentile <- NA_real_
  if (!is.null(analytic_data)) {
    band <- as.character(age_band_value(person$age))
    d <- analytic_data |>
      dplyr::filter(as.character(sex) == person$sex, as.character(age_band) == band,
                    valid_weight, is.finite(.data[[person$outcome]]))
    raw_percentile <- 100 * weighted_ecdf_value(d[[person$outcome]], d$MEC4YR, observed)
  }
  outside_interval <- observed < prediction$lower || observed > prediction$upper
  unusual <- isTRUE(p < 0.05) || outside_interval
  list(
    performed = TRUE,
    observed = observed,
    predicted = prediction$predicted,
    residual = residual,
    raw_percentile = raw_percentile,
    adjusted_percentile = adjusted_percentile,
    empirical_p = p,
    outside_interval = outside_interval,
    statistically_unusual = unusual,
    label = if (unusual) "Statistically unusual" else "Within the reference range",
    note = "This is a population reference, not a diagnosis."
  )
}
