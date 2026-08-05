test_that("processed country and long datasets satisfy the master contract", {
  expect_true(file.exists(project_path("data", "processed", "humanitarian_indices_country.csv")))
  wide <- readr::read_csv(project_path("data", "processed", "humanitarian_indices_country.csv"), show_col_types = FALSE)
  long <- readr::read_csv(project_path("data", "processed", "humanitarian_indices_long.csv"), show_col_types = FALSE)

  expect_equal(nrow(wide), 195L)
  expect_equal(dplyr::n_distinct(wide$iso3), 195L)
  expect_false(anyNA(wide$iso3))
  expect_false(anyNA(wide$region))
  expect_false(anyNA(wide$subregion))
  expect_equal(nrow(long), 195L * length(INDEX_ORDER))
  expect_equal(dplyr::n_distinct(paste(long$iso3, long$index_id)), nrow(long))

  triples <- unlist(lapply(INDEX_ORDER, function(id) paste0(id, c("_score", "_rank", "_decile"))))
  expect_true(all(triples %in% names(wide)))
})

test_that("scores, deciles, flags and counts retain their intended semantics", {
  wide <- readr::read_csv(project_path("data", "processed", "humanitarian_indices_country.csv"), show_col_types = FALSE)
  long <- readr::read_csv(project_path("data", "processed", "humanitarian_indices_long.csv"), show_col_types = FALSE)

  ranked <- long |> dplyr::filter(!is.na(.data$rank))
  expect_true(all(ranked$decile %in% 1:10))
  expect_equal(ranked$top_10, ranked$decile == 1L)
  expect_equal(ranked$top_20, ranked$decile <= 2L)
  expect_true(all(is.na(long$rank[is.na(long$score)])))

  debt <- long |> dplyr::filter(.data$index_id == "debt_distress")
  expect_true(all(is.na(debt$rank)))
  expect_true(all(is.na(debt$decile)))
  expect_true(all(is.na(debt$top_10)))
  expect_true(all(is.na(debt$top_20)))

  expected_counts <- long |>
    dplyr::filter(.data$eligible_for_counts) |>
    dplyr::group_by(.data$iso3) |>
    dplyr::summarise(
      indices_ranked_count_expected = sum(!is.na(.data$rank)),
      top_10_count_expected = sum(.data$top_10 %in% TRUE),
      top_20_count_expected = sum(.data$top_20 %in% TRUE),
      .groups = "drop"
    )
  checked <- wide |> dplyr::left_join(expected_counts, by = "iso3")
  expect_equal(checked$indices_ranked_count, checked$indices_ranked_count_expected)
  expect_equal(checked$top_10_count, checked$top_10_count_expected)
  expect_equal(checked$top_20_count, checked$top_20_count_expected)
})

test_that("GHI censored values retain labels but not numeric rankings", {
  long <- readr::read_csv(project_path("data", "processed", "humanitarian_indices_long.csv"), show_col_types = FALSE)
  censored <- long |>
    dplyr::filter(
      .data$index_id == "ghi",
      stringr::str_detect(.data$score_label, "<|–|-")
    )
  expect_gt(nrow(censored), 0L)
  expect_true(all(is.na(censored$score)))
  expect_true(all(is.na(censored$rank)))
  expect_setequal(c("BDI", "YEM"), censored$iso3[censored$score_label == "35–49.9*"])
})
