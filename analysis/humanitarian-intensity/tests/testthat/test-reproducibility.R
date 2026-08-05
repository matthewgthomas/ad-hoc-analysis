test_that("two runs from the locked raw cache are byte-identical", {
  before <- readr::read_csv(project_path("artifacts", "processed_file_checksums.csv"), show_col_types = FALSE)
  previous_directory <- setwd(PROJECT_ROOT)
  on.exit(setwd(previous_directory), add = TRUE)
  suppressMessages(run_humanitarian_indices_pipeline(refresh = FALSE))
  after <- readr::read_csv(project_path("artifacts", "processed_file_checksums.csv"), show_col_types = FALSE)
  expect_equal(after, before)
})
