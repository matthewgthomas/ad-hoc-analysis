grip_palette <- c(Female = "#C55A11", Male = "#2166AC")

theme_grip <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", colour = "#17324D"),
      plot.subtitle = ggplot2::element_text(colour = "#4B5D6B"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(colour = "#D8E1E8", linewidth = 0.35),
      panel.grid.major.y = ggplot2::element_line(colour = "#E8EEF2", linewidth = 0.35),
      strip.text = ggplot2::element_text(face = "bold", colour = "#17324D"),
      legend.position = "bottom"
    )
}

outcome_label <- function(outcome) {
  if (identical(outcome, "bilateral_grip")) "Bilateral maximal-effort grip (kg)" else "Best single-hand maximal-effort grip (kg)"
}

plot_weighted_distribution <- function(data, outcome = "best_single_grip", person = NULL) {
  d <- data |>
    dplyr::filter(distribution_eligible, is.finite(.data[[outcome]]))
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[outcome]], weight = MEC4YR, colour = sex, fill = sex)) +
    ggplot2::geom_density(alpha = 0.17, linewidth = 1.05, adjust = 1.05) +
    ggplot2::facet_wrap(~sex, ncol = 1, scales = "free_y") +
    ggplot2::scale_colour_manual(values = grip_palette, guide = "none") +
    ggplot2::scale_fill_manual(values = grip_palette, guide = "none") +
    ggplot2::labs(
      title = "Grip strength distributions differ substantially by released sex category",
      subtitle = "Survey-weighted NHANES 2011–2014 adults aged 18–80; maximal-effort trials only",
      x = outcome_label(outcome), y = "Weighted density"
    ) + theme_grip()
  if (!is.null(person)) {
    value <- if (is.finite(person$observed_grip_kg %||% NA_real_)) person$observed_grip_kg else person$predicted %||% NA_real_
    if (is.finite(value)) {
    mark <- data.frame(sex = factor(person$sex, levels = levels(data$sex)), value = value)
    p <- p + ggplot2::geom_vline(data = mark, ggplot2::aes(xintercept = value),
                                 colour = "#111111", linetype = "longdash", linewidth = 0.9,
                                 inherit.aes = FALSE)
    }
  }
  p
}

age_percentile_data <- function(data, outcome = "best_single_grip") {
  data |>
    dplyr::filter(distribution_eligible, is.finite(.data[[outcome]])) |>
    dplyr::mutate(age_group = cut(age, breaks = seq(18, 83, by = 5), right = FALSE,
                                  include.lowest = TRUE),
                  age_mid = floor((age - 18) / 5) * 5 + 20.5) |>
    dplyr::group_by(sex, age_group, age_mid) |>
    dplyr::summarise(
      q10 = weighted_quantile(.data[[outcome]], MEC4YR, 0.10),
      q25 = weighted_quantile(.data[[outcome]], MEC4YR, 0.25),
      q50 = weighted_quantile(.data[[outcome]], MEC4YR, 0.50),
      q75 = weighted_quantile(.data[[outcome]], MEC4YR, 0.75),
      q90 = weighted_quantile(.data[[outcome]], MEC4YR, 0.90),
      effective_n = effective_sample_size(MEC4YR), .groups = "drop"
    )
}

plot_age_percentiles <- function(data, outcome = "best_single_grip", person = NULL) {
  q <- age_percentile_data(data, outcome)
  p <- ggplot2::ggplot(q, ggplot2::aes(x = age_mid)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q10, ymax = q90, fill = sex), alpha = 0.12) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q25, ymax = q75, fill = sex), alpha = 0.23) +
    ggplot2::geom_line(ggplot2::aes(y = q50, colour = sex), linewidth = 1.05) +
    ggplot2::facet_wrap(~sex, ncol = 1) +
    ggplot2::scale_colour_manual(values = grip_palette, guide = "none") +
    ggplot2::scale_fill_manual(values = grip_palette, guide = "none") +
    ggplot2::scale_x_continuous(breaks = seq(20, 80, 10), limits = c(18, 81)) +
    ggplot2::labs(
      title = "Median grip is highest in early adulthood and declines with age",
      subtitle = "Lines show weighted medians; dark and light bands show the 25th–75th and 10th–90th percentiles",
      x = "Age (years)", y = outcome_label(outcome)
    ) + theme_grip()
  if (!is.null(person)) {
    value <- if (is.finite(person$observed_grip_kg %||% NA_real_)) person$observed_grip_kg else person$predicted %||% NA_real_
    if (is.finite(value)) {
      mark <- data.frame(sex = factor(person$sex, levels = levels(data$sex)), age = person$age, value = value)
      p <- p + ggplot2::geom_point(data = mark, ggplot2::aes(age, value), inherit.aes = FALSE,
                                   shape = 21, size = 3.5, stroke = 1, fill = "white", colour = "#111111")
    }
  }
  p
}

binned_factor_summary <- function(data, outcome, variable, bins = 8L) {
  d <- data |>
    dplyr::filter(distribution_eligible, is.finite(.data[[outcome]]), is.finite(.data[[variable]])) |>
    dplyr::mutate(bin = dplyr::ntile(.data[[variable]], bins)) |>
    dplyr::group_by(sex, bin) |>
    dplyr::summarise(
      x = stats::weighted.mean(.data[[variable]], MEC4YR),
      q25 = weighted_quantile(.data[[outcome]], MEC4YR, 0.25),
      median = weighted_quantile(.data[[outcome]], MEC4YR, 0.50),
      q75 = weighted_quantile(.data[[outcome]], MEC4YR, 0.75), .groups = "drop"
    )
  d$variable <- variable
  d
}

salient_factor_data <- function(data, outcome = "best_single_grip") {
  vars <- c("bmi", "height_cm", "arm_circumference_cm", "activity_met_min_week")
  dplyr::bind_rows(lapply(vars, function(v) binned_factor_summary(data, outcome, v))) |>
    dplyr::mutate(
      factor_label = dplyr::recode(variable,
        bmi = "BMI (kg/m²)", height_cm = "Height (cm)",
        arm_circumference_cm = "Arm circumference (cm)",
        activity_met_min_week = "Activity (MET-min/week)"
      )
    )
}

plot_salient_factors <- function(data, outcome = "best_single_grip") {
  s <- salient_factor_data(data, outcome)
  ggplot2::ggplot(s, ggplot2::aes(x = x, y = median, colour = sex, group = sex)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = q25, ymax = q75), alpha = 0.5, linewidth = 0.75) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(ggplot2::aes(shape = sex), size = 2) +
    ggplot2::facet_wrap(~factor_label, scales = "free_x", ncol = 2) +
    ggplot2::scale_colour_manual(values = grip_palette) +
    ggplot2::scale_shape_manual(values = c(Female = 16, Male = 17)) +
    ggplot2::labs(
      title = "Body size and activity provide useful context for grip strength",
      subtitle = "Survey-weighted medians and interquartile ranges within equally sized bins",
      x = NULL, y = outcome_label(outcome), colour = "Sex", shape = "Sex"
    ) + theme_grip()
}

plot_calibration <- function(models, outcome = "best_single_grip", model_type = "core") {
  d <- models$oof_predictions |>
    dplyr::filter(.data$outcome == .env$outcome, .data$model_type == .env$model_type, engine == "survey") |>
    dplyr::mutate(bin = dplyr::ntile(predicted, 10)) |>
    dplyr::group_by(sex, bin) |>
    dplyr::summarise(predicted = stats::weighted.mean(predicted, weight),
                     observed = stats::weighted.mean(actual, weight), .groups = "drop")
  lim <- range(c(d$predicted, d$observed), finite = TRUE)
  ggplot2::ggplot(d, ggplot2::aes(predicted, observed, colour = sex, shape = sex)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#647582", linetype = "dashed") +
    ggplot2::geom_line(linewidth = 0.85) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::facet_wrap(~sex) +
    ggplot2::coord_equal(xlim = lim, ylim = lim) +
    ggplot2::scale_colour_manual(values = grip_palette) +
    ggplot2::scale_shape_manual(values = c(Female = 16, Male = 17)) +
    ggplot2::labs(
      title = "Out-of-fold predictions track observed grip across the range",
      subtitle = "Five cluster-held-out folds; each point is a weighted tenth of predicted values",
      x = "Mean predicted grip (kg)", y = "Mean observed grip (kg)", colour = "Sex", shape = "Sex"
    ) + theme_grip()
}

plot_residuals <- function(models, outcome = "best_single_grip", model_type = "core") {
  d <- models$oof_predictions |>
    dplyr::filter(.data$outcome == .env$outcome, .data$model_type == .env$model_type, engine == "survey")
  ggplot2::ggplot(d, ggplot2::aes(predicted, residual, colour = sex, weight = weight)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#647582", linetype = "dashed") +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE, linewidth = 0.9) +
    ggplot2::facet_wrap(~sex) +
    ggplot2::scale_colour_manual(values = grip_palette, guide = "none") +
    ggplot2::labs(
      title = "Residual checks reveal where prediction error changes across the range",
      subtitle = "Observed minus predicted grip; shaded lines are weighted local smooths",
      x = "Predicted grip (kg)", y = "Residual (kg)"
    ) + theme_grip()
}
