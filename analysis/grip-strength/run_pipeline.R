#!/usr/bin/env Rscript

source(file.path("R", "load_all.R"))

message("Preparing NHANES 2011–2014 grip-strength data …")
prepared <- prepare_nhanes(
  cache_dir = file.path("data", "raw"),
  output_path = file.path("data", "nhanes_grip_adults.rds"),
  qa_path = file.path("artifacts", "data_quality.rds"),
  download = TRUE
)

message("Fitting survey-weighted models and cluster-held-out validation …")
models <- fit_grip_models(
  prepared$data,
  output_path = file.path("models", "grip_models.rds"),
  metrics_path = file.path("artifacts", "model_metrics.csv")
)

utils::write.csv(prepared$qa$source_counts, file.path("artifacts", "source_row_counts.csv"), row.names = FALSE)
utils::write.csv(prepared$qa$missingness, file.path("artifacts", "missingness.csv"), row.names = FALSE)
utils::write.csv(prepared$qa$invalid_codes, file.path("artifacts", "invalid_codes.csv"), row.names = FALSE)
utils::write.csv(models$subgroup_calibration, file.path("artifacts", "race_ethnicity_calibration.csv"), row.names = FALSE)
source_manifest <- nhanes_manifest()
source_manifest$path <- file.path("data", "raw", source_manifest$file)
source_manifest$bytes <- file.info(source_manifest$path)$size
source_manifest$md5 <- unname(tools::md5sum(source_manifest$path))
utils::write.csv(source_manifest, file.path("artifacts", "source_manifest.csv"), row.names = FALSE)

message("Done. Adult rows: ", nrow(prepared$data),
        "; core eligible: ", sum(prepared$data$core_eligible),
        "; fitted model objects: ", length(models$survey_models) + length(models$gam_models) +
          length(models$sensitivity_cohort_models), ".")
