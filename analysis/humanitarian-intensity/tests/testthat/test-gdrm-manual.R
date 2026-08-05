test_that("manual GDRM input enforces required filters and values", {
  registry <- source_registry()
  master <- readr::read_csv(project_path("data", "processed", "master_geography.csv"), show_col_types = FALSE)
  qa <- new_qa_state()
  base <- tibble::tibble(
    iso3 = c("AFG", "ALB"),
    country = c("Afghanistan", "Albania"),
    aad_current_multihazard = c("100", "20"),
    scenario = c("Current", "Current"),
    metric = c("AAD", "AAD"),
    hazard_scope = c("Multi-hazard", "All"),
    source_snapshot_date = c("2026-08-04", "2026-08-04"),
    aggregation_level = c("country total", "country total"),
    notes = c("", "")
  )
  path <- tempfile(fileext = ".csv")
  readr::write_csv(base, path)
  valid <- adapt_disaster_displacement(path, registry, master, qa)
  expect_equal(nrow(valid), 2L)
  expect_equal(valid$score, c(100, 20))

  invalid_scenario <- base
  invalid_scenario$scenario[1] <- "Future"
  readr::write_csv(invalid_scenario, path)
  expect_error(adapt_disaster_displacement(path, registry, master, qa), "scenario must be Current")

  duplicate <- dplyr::bind_rows(base[1, ], base[1, ])
  readr::write_csv(duplicate, path)
  expect_error(adapt_disaster_displacement(path, registry, master, qa), "duplicate ISO3")

  negative <- base
  negative$aad_current_multihazard[1] <- "-1"
  readr::write_csv(negative, path)
  expect_error(adapt_disaster_displacement(path, registry, master, qa), "non-negative")
})
