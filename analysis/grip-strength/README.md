# NHANES grip-strength analysis in R

This project prepares NHANES 2011–2014 grip, demographic, body-measure, and physical-activity data; fits survey-aware prediction models; renders a self-contained Quarto report; and runs a local Shiny app for personal comparison.

The included prepared data and model objects cover US adults aged 18–80. Personal inputs are blank by default. Results are population references, not clinical diagnoses.

## Included

- `grip_strength_analysis.qmd` and `grip_strength_analysis.html`: reproducible analysis and rendered report.
- `app.R`: local Shiny interface.
- `R/`: preparation, modelling, prediction/comparison, and plotting functions.
- `data/nhanes_grip_adults.rds`: prepared adult analysis data.
- `models/grip_models.rds`: fitted survey models, GAM sensitivity models, held-out predictions, and residual references.
- `artifacts/`: quality checks, source manifest, validation metrics, and subgroup diagnostics.
- `tests/`: preparation, model, prediction, and Shiny server tests.
- `renv.lock`: pinned R dependencies.

Raw NHANES XPT files are intentionally omitted from the release bundle because `prepare_nhanes()` downloads them from the CDC and verifies all joins. The source manifest records URLs, sizes, and checksums used for the delivered analysis.

## Reproduce

From the project directory in R 4.6.0:

```r
install.packages("renv")
renv::restore()
source("run_pipeline.R")
```

Render the report:

```sh
quarto render grip_strength_analysis.qmd
```

Run the app:

```r
shiny::runApp(".")
```

Run tests:

```r
source("tests/testthat.R")
```

The pipeline uses `MEC4YR = WTMEC2YR / 2` and a nested NHANES design with MEC weights, strata, and PSUs. Five validation folds hold out complete PSU groups. Reciprocal survey-cycle holdouts provide a separate stability check.

## Personal measurement choice

Choose “Best single hand” only for one maximal hand value. Choose “Sum of best right + left” only when the observed value is the sum of best measurements from both hands. A mismatch will make the comparison meaningless.

Required inputs are age, the released NHANES male/female comparison category, height, and either BMI or weight. Observed grip is optional; without it the app reports only expected grip and its individual interval.
