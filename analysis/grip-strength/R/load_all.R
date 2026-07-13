required_packages <- c(
  "haven", "dplyr", "tidyr", "tibble", "purrr", "ggplot2", "survey",
  "splines", "mgcv", "shiny", "bslib", "plotly", "testthat"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  stop("Install missing R package(s): ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

for (source_file in c("utils.R", "data_prep.R", "model.R", "predict.R", "plots.R")) {
  source(file.path("R", source_file), local = FALSE)
}
