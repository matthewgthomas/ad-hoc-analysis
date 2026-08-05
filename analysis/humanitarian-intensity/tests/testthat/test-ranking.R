test_that("ranking is vulnerability-oriented and preserves ties", {
  higher <- tibble::tibble(
    index_id = "synthetic",
    score = c(10, 10, 9, 8, 7, 6, 5, 4, 3, 2),
    score_direction = "higher_worse",
    rankable = TRUE,
    eligible_for_counts = TRUE
  ) |>
    rank_index()

  expect_equal(higher$rank[1:2], c(1L, 1L))
  expect_equal(higher$decile[1:2], c(1L, 1L))
  expect_true(all(higher$top_10[1:2]))
  expect_equal(higher$rank[3], 3L)
  expect_equal(higher$decile[3], 3L)

  lower <- higher |>
    dplyr::select(index_id, score) |>
    dplyr::mutate(
      score_direction = "lower_worse",
      rankable = TRUE,
      eligible_for_counts = TRUE
    ) |>
    rank_index()
  expect_equal(lower$rank[which.min(lower$score)], 1L)
  expect_equal(lower$decile[which.min(lower$score)], 1L)
})

test_that("missing values and non-rankable classifications never receive ranks", {
  numeric_data <- tibble::tibble(
    index_id = "synthetic",
    score = c(1, 2, NA_real_),
    score_direction = "lower_worse",
    rankable = TRUE,
    eligible_for_counts = TRUE
  ) |>
    rank_index()
  expect_true(is.na(numeric_data$rank[3]))
  expect_true(is.na(numeric_data$decile[3]))
  expect_true(is.na(numeric_data$top_10[3]))

  debt <- numeric_data |>
    dplyr::mutate(rankable = FALSE, eligible_for_counts = FALSE) |>
    dplyr::select(index_id, score, score_direction, rankable, eligible_for_counts) |>
    rank_index()
  expect_true(all(is.na(debt$rank)))
  expect_true(all(is.na(debt$decile)))
  expect_true(all(is.na(debt$top_10)))
})
