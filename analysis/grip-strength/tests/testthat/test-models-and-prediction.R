test_that("cluster folds are isolated and validation results are populated", {
  models <- readRDS(project_file("models", "grip_models.rds"))
  isolation <- models$oof_predictions |>
    dplyr::group_by(sex, outcome, model_type, engine, cluster_id) |>
    dplyr::summarise(folds = dplyr::n_distinct(fold), .groups = "drop")
  expect_equal(max(isolation$folds), 1)
  expect_true(all(models$metrics$n > 0))
  expect_true(all(models$metrics$interval_coverage > 0.90))
  expect_true(all(models$metrics$interval_coverage < 0.99))
  expect_true(all(models$cycle_metrics$n > 2000))
  expect_gt(nrow(models$subgroup_calibration), 10)
  expect_true(all(is.finite(models$subgroup_calibration$mean_error)))
})

test_that("personal predictions are deterministic and require observed grip for comparison", {
  data <- readRDS(project_file("data", "nhanes_grip_adults.rds"))
  models <- readRDS(project_file("models", "grip_models.rds"))
  person <- list(age = 40, sex = "Female", height_cm = 165, bmi = 24,
                 outcome = "best_single_grip")
  p1 <- predict_grip(person, models)
  p2 <- predict_grip(person, models)
  expect_equal(p1$predicted, p2$predicted, tolerance = 1e-12)
  expect_equal(unique(p1$residual_reference$model_type), "core")
  expect_lt(p1$lower, p1$predicted)
  expect_gt(p1$upper, p1$predicted)
  expect_false(compare_grip(person, p1, analytic_data = data)$performed)
  person$observed_grip_kg <- p1$predicted
  comparison <- compare_grip(person, p1, analytic_data = data)
  expect_true(comparison$performed)
  expect_gte(comparison$adjusted_percentile, 0)
  expect_lte(comparison$adjusted_percentile, 100)
  expect_gte(comparison$empirical_p, 0)
  expect_lte(comparison$empirical_p, 1)
})

test_that("extended inputs select the extended model", {
  models <- readRDS(project_file("models", "grip_models.rds"))
  person <- list(
    age = 45, sex = "Male", height_cm = 178, bmi = 26,
    outcome = "best_single_grip", arm_circumference_cm = 33,
    arm_length_cm = 38, activity_met_min_week = 1200,
    handedness = "Right", any_hand_pain = "No", prior_surgery = "No", posture = "Standing"
  )
  expect_equal(predict_grip(person, models)$model_type, "extended")
})

test_that("invalid personal inputs fail clearly", {
  models <- readRDS(project_file("models", "grip_models.rds"))
  expect_error(predict_grip(list(age = 17, sex = "Male", height_cm = 180, bmi = 24), models),
               "between 18 and 80")
  expect_error(predict_grip(list(age = 40, sex = "Other", height_cm = 180, bmi = 24), models),
               "Male.*Female")
  expect_error(predict_grip(list(age = 40, sex = "Male", height_cm = 180), models),
               "either bmi or weight_kg")
})
