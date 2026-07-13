`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

row_max_na <- function(...) {
  x <- cbind(...)
  out <- apply(x, 1L, function(z) {
    z <- z[is.finite(z)]
    if (length(z)) max(z) else NA_real_
  })
  as.numeric(out)
}

weighted_quantile <- function(x, w, probs = c(0.025, 0.5, 0.975)) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  x <- x[keep]
  w <- w[keep]
  if (!length(x)) return(rep(NA_real_, length(probs)))
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cw <- cumsum(w) / sum(w)
  vapply(probs, function(p) x[which(cw >= p)[1L]], numeric(1))
}

weighted_ecdf_value <- function(x, w, value) {
  keep <- is.finite(x) & is.finite(w) & w > 0
  if (!any(keep) || !is.finite(value)) return(NA_real_)
  sum(w[keep & x <= value]) / sum(w[keep])
}

weighted_metrics <- function(actual, predicted, w) {
  keep <- is.finite(actual) & is.finite(predicted) & is.finite(w) & w > 0
  actual <- actual[keep]
  predicted <- predicted[keep]
  w <- w[keep]
  if (!length(actual)) {
    return(data.frame(n = 0L, rmse = NA_real_, mae = NA_real_, r_squared = NA_real_))
  }
  w <- w / sum(w)
  err <- actual - predicted
  mu <- sum(w * actual)
  data.frame(
    n = length(actual),
    rmse = sqrt(sum(w * err^2)),
    mae = sum(w * abs(err)),
    r_squared = 1 - sum(w * err^2) / sum(w * (actual - mu)^2)
  )
}

effective_sample_size <- function(w) {
  w <- w[is.finite(w) & w > 0]
  if (!length(w)) return(0)
  sum(w)^2 / sum(w^2)
}

age_band_value <- function(age) {
  cut(
    age,
    breaks = c(18, 30, 40, 50, 60, 70, 81),
    right = FALSE,
    include.lowest = TRUE,
    labels = c("18–29", "30–39", "40–49", "50–59", "60–69", "70–80")
  )
}

clean_yes_no <- function(x, yes = 1, no = 2) {
  factor(dplyr::case_when(x == yes ~ "Yes", x == no ~ "No", TRUE ~ NA_character_),
         levels = c("No", "Yes"))
}

normalise_person <- function(person) {
  required <- c("age", "sex", "height_cm")
  missing_required <- required[!vapply(required, function(nm) {
    !is.null(person[[nm]]) && length(person[[nm]]) == 1L && !is.na(person[[nm]]) && person[[nm]] != ""
  }, logical(1))]
  if (length(missing_required)) {
    stop("Missing required field(s): ", paste(missing_required, collapse = ", "), call. = FALSE)
  }
  person$age <- as.numeric(person$age)
  person$height_cm <- as.numeric(person$height_cm)
  person$sex <- as.character(person$sex)
  if (!person$sex %in% c("Male", "Female")) {
    stop("sex must be 'Male' or 'Female' to match the released NHANES category.", call. = FALSE)
  }
  if (!is.finite(person$age) || person$age < 18 || person$age > 80) {
    stop("age must be between 18 and 80 years.", call. = FALSE)
  }
  if (!is.finite(person$height_cm) || person$height_cm < 120 || person$height_cm > 230) {
    stop("height_cm must be between 120 and 230 cm.", call. = FALSE)
  }
  bmi <- suppressWarnings(as.numeric(person$bmi %||% NA_real_))
  weight <- suppressWarnings(as.numeric(person$weight_kg %||% NA_real_))
  if (!is.finite(bmi) && is.finite(weight)) bmi <- weight / (person$height_cm / 100)^2
  if (!is.finite(bmi)) stop("Supply either bmi or weight_kg.", call. = FALSE)
  if (bmi < 12 || bmi > 70) stop("BMI must be between 12 and 70 kg/m².", call. = FALSE)
  person$bmi <- bmi
  person$outcome <- person$outcome %||% "best_single_grip"
  if (!person$outcome %in% c("best_single_grip", "bilateral_grip")) {
    stop("outcome must be 'best_single_grip' or 'bilateral_grip'.", call. = FALSE)
  }
  observed <- suppressWarnings(as.numeric(person$observed_grip_kg %||% NA_real_))
  if (is.finite(observed) && observed <= 0) stop("observed_grip_kg must be positive.", call. = FALSE)
  person$observed_grip_kg <- observed
  person
}

advanced_person_complete <- function(person) {
  required <- c("arm_circumference_cm", "arm_length_cm", "activity_met_min_week",
                "handedness", "any_hand_pain", "prior_surgery", "posture")
  all(vapply(required, function(nm) {
    x <- person[[nm]]
    !is.null(x) && length(x) == 1L && !is.na(x) && x != ""
  }, logical(1)))
}

person_as_newdata <- function(person, extended = FALSE) {
  x <- data.frame(
    age = as.numeric(person$age),
    bmi = as.numeric(person$bmi),
    height_cm = as.numeric(person$height_cm)
  )
  if (extended) {
    x$arm_circumference_cm <- as.numeric(person$arm_circumference_cm)
    x$arm_length_cm <- as.numeric(person$arm_length_cm)
    x$log_activity <- log1p(as.numeric(person$activity_met_min_week))
    x$handedness <- factor(person$handedness, levels = c("Right", "Left", "Ambidextrous"))
    x$any_hand_pain <- factor(person$any_hand_pain, levels = c("No", "Yes"))
    x$prior_surgery <- factor(person$prior_surgery, levels = c("No", "Yes", "Not asked"))
    x$posture <- factor(person$posture, levels = c("Standing", "Seated"))
  }
  x
}
