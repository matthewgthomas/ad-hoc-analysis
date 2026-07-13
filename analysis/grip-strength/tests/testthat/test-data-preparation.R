test_that("prepared IDs, weights, and reconciliation pass release gates", {
  data <- readRDS(project_file("data", "nhanes_grip_adults.rds"))
  qa <- readRDS(project_file("artifacts", "data_quality.rds"))
  expect_false(anyNA(data$SEQN))
  expect_equal(anyDuplicated(data$SEQN), 0L)
  expect_equal(data$MEC4YR, data$WTMEC2YR / 2)
  expect_true(all(data$MEC4YR > 0))
  expect_equal(qa$reconciliation$discordant, 0)
  expect_equal(qa$source_counts$rows, qa$source_counts$unique_seqn)
  expect_true(all(qa$invalid_codes$invalid_n == 0))
  expect_gt(qa$eligibility$core, 10000)
})

test_that("trial order maps to right and left hands", {
  x <- tibble::tibble(
    MGATHAND = c(1, 2),
    MGXH1T1 = c(30, 20), MGXH1T1E = 1,
    MGXH1T2 = c(31, 21), MGXH1T2E = 1,
    MGXH1T3 = c(32, 22), MGXH1T3E = 1,
    MGXH2T1 = c(20, 30), MGXH2T1E = 1,
    MGXH2T2 = c(21, 31), MGXH2T2E = 1,
    MGXH2T3 = c(22, 32), MGXH2T3E = 1
  )
  y <- derive_grip(x)
  expect_equal(y$right_max_effort, c(32, 32))
  expect_equal(y$left_max_effort, c(22, 22))
  expect_equal(y$bilateral_grip, c(54, 54))
})

test_that("non-maximal effort trials do not enter derived outcomes", {
  x <- tibble::tibble(
    MGATHAND = 1,
    MGXH1T1 = 50, MGXH1T1E = 2,
    MGXH1T2 = 30, MGXH1T2E = 1,
    MGXH1T3 = 31, MGXH1T3E = 1,
    MGXH2T1 = 40, MGXH2T1E = 1,
    MGXH2T2 = 41, MGXH2T2E = 1,
    MGXH2T3 = 60, MGXH2T3E = 2
  )
  y <- derive_grip(x)
  expect_equal(y$hand1_raw, 50)
  expect_equal(y$hand1_max_effort, 31)
  expect_equal(y$hand2_max_effort, 41)
  expect_equal(y$bilateral_grip, 72)
})
