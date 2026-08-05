score_ranges <- function() {
  tibble::tribble(
    ~index_id, ~valid_min, ~valid_max,
    "inform_risk", 0, 10,
    "inform_severity", 0, 10,
    "underfunded_crisis", 0, 100,
    "oecd_fragility", -10, 10,
    "worldrisk", 0, 100,
    "nd_gain", 0, 100,
    "hdi", 0, 1,
    "mpi", 0, 1,
    "ghi", 0, 100,
    "ghs", 0, 100,
    "wps", 0, 1,
    "un_mvi", 0, 100,
    "debt_distress", 1, 4,
    "searo", 0, 10,
    "disaster_displacement", 0, Inf,
    "internal_displacement", 0, 1
  )
}

complete_index_coverage <- function(adapter_data, index_id, registry, master) {
  spec <- registry[registry$index_id == index_id, , drop = FALSE]
  if (nrow(spec) != 1L) stop("Missing source specification for ", index_id, call. = FALSE)
  assert_unique_nonmissing(adapter_data, "iso3", paste(index_id, "adapter"))

  completed <- master |>
    dplyr::select(country, iso3, region, subregion) |>
    dplyr::left_join(adapter_data, by = "iso3")
  if (nrow(completed) != nrow(master)) stop("Join expansion while completing ", index_id, call. = FALSE)

  completed |>
    dplyr::mutate(
      index_id = .env$index_id,
      index_name = spec$index_name,
      edition = spec$edition,
      reference_year = dplyr::coalesce(.data$reference_year, spec$reference_year),
      score_direction = spec$score_direction,
      rankable = spec$rankable,
      eligible_for_counts = spec$eligible_for_counts
    ) |>
    rank_index() |>
    dplyr::select(
      country, iso3, region, subregion,
      index_id, index_name, source_country,
      score, score_label, reference_year, edition,
      score_direction, rankable, eligible_for_counts,
      n_scored, rank, decile, top_10, top_20
    )
}

check_index_quality <- function(data, registry) {
  ranges <- score_ranges()
  coverage <- data |>
    dplyr::group_by(.data$index_id, .data$index_name) |>
    dplyr::summarise(
      n_master_countries = dplyr::n(),
      n_numeric_scores = sum(!is.na(.data$score)),
      n_labelled_records = sum(!is.na(.data$score_label)),
      n_ranked = sum(!is.na(.data$rank)),
      n_top_10 = sum(.data$top_10 %in% TRUE),
      n_top_20 = sum(.data$top_20 %in% TRUE),
      .groups = "drop"
    ) |>
    dplyr::left_join(
      registry |>
        dplyr::select(index_id, expected_min, expected_max),
      by = "index_id"
    ) |>
    dplyr::mutate(
      coverage_ok = .data$n_numeric_scores >= .data$expected_min &
        .data$n_numeric_scores <= .data$expected_max
    )

  checks <- data |>
    dplyr::left_join(ranges, by = "index_id") |>
    dplyr::group_by(.data$index_id, .data$index_name) |>
    dplyr::group_modify(function(rows, key) {
      scored <- rows |> dplyr::filter(!is.na(.data$score))
      ranked <- rows |> dplyr::filter(!is.na(.data$rank))
      direction <- unique(rows$score_direction)
      ordered <- if (!nrow(ranked)) {
        ranked
      } else if (identical(direction, "higher_worse")) {
        ranked |> dplyr::arrange(dplyr::desc(.data$score), .data$rank)
      } else {
        ranked |> dplyr::arrange(.data$score, .data$rank)
      }
      tibble::tibble(
        scores_in_range = !nrow(scored) || all(scored$score >= scored$valid_min & scored$score <= scored$valid_max),
        rank_monotonic = !nrow(ordered) || all(diff(ordered$rank) >= 0),
        ranks_start_at_one = !nrow(ranked) || min(ranked$rank) == 1L,
        deciles_valid = !nrow(ranked) || all(ranked$decile %in% 1:10),
        top_10_matches_decile = all((rows$top_10 %in% TRUE) == (!is.na(rows$decile) & rows$eligible_for_counts & rows$decile == 1L)),
        top_20_matches_decile = all((rows$top_20 %in% TRUE) == (!is.na(rows$decile) & rows$eligible_for_counts & rows$decile <= 2L)),
        missing_scores_unranked = all(is.na(rows$rank[is.na(rows$score)])),
        duplicate_scored_iso3 = anyDuplicated(rows$iso3[!is.na(rows$score)]) > 0L
      )
    }) |>
    dplyr::ungroup()

  list(coverage = coverage, checks = checks)
}

validate_acceptance <- function(master, long, quality) {
  assert_unique_nonmissing(master, "iso3", "Master geography")
  if (nrow(master) != 195L) stop("Master geography acceptance check failed.", call. = FALSE)
  if (nrow(long) != 195L * length(INDEX_ORDER)) stop("Long dataset does not have one row per country-index pair.", call. = FALSE)
  if (anyDuplicated(paste(long$iso3, long$index_id, sep = "::"))) stop("Long dataset has duplicate country-index rows.", call. = FALSE)

  failed_coverage <- quality$coverage |>
    dplyr::filter(!.data$coverage_ok)
  if (nrow(failed_coverage)) {
    stop(
      "Pinned-source coverage check failed: ",
      paste0(failed_coverage$index_id, "=", failed_coverage$n_numeric_scores, collapse = ", "),
      call. = FALSE
    )
  }
  failed_checks <- quality$checks |>
    dplyr::filter(
      !.data$scores_in_range | !.data$rank_monotonic | !.data$ranks_start_at_one |
        !.data$deciles_valid | !.data$top_10_matches_decile |
        !.data$top_20_matches_decile | !.data$missing_scores_unranked |
        .data$duplicate_scored_iso3
    )
  if (nrow(failed_checks)) stop("Index quality checks failed for: ", paste(failed_checks$index_id, collapse = ", "), call. = FALSE)

  debt <- long |> dplyr::filter(.data$index_id == "debt_distress")
  if (any(!is.na(debt$rank)) || any(!is.na(debt$decile)) || any(!is.na(debt$top_10)) || any(!is.na(debt$top_20))) {
    stop("Debt distress must not receive ranks, deciles, or top-percentile flags.", call. = FALSE)
  }
  invisible(TRUE)
}

build_wide_dataset <- function(master, long) {
  metrics <- long |>
    dplyr::select(iso3, index_id, score, rank, decile) |>
    tidyr::pivot_wider(
      names_from = index_id,
      values_from = c(score, rank, decile),
      names_glue = "{index_id}_{.value}"
    )

  special <- long |>
    dplyr::group_by(.data$iso3) |>
    dplyr::summarise(
      ghi_score_label = .data$score_label[.data$index_id == "ghi"][1],
      mpi_reference_year = .data$reference_year[.data$index_id == "mpi"][1],
      debt_distress_class = .data$score_label[.data$index_id == "debt_distress"][1],
      debt_distress_ordinal = .data$score[.data$index_id == "debt_distress"][1],
      .groups = "drop"
    )

  counts <- long |>
    dplyr::filter(.data$eligible_for_counts) |>
    dplyr::group_by(.data$iso3) |>
    dplyr::summarise(
      indices_ranked_count = sum(!is.na(.data$rank)),
      top_10_count = sum(.data$top_10 %in% TRUE),
      top_20_count = sum(.data$top_20 %in% TRUE),
      .groups = "drop"
    )

  result <- master |>
    dplyr::select(country, iso3, region, subregion) |>
    dplyr::left_join(metrics, by = "iso3") |>
    dplyr::left_join(special, by = "iso3") |>
    dplyr::left_join(counts, by = "iso3")

  triple_columns <- unlist(lapply(INDEX_ORDER, function(id) paste0(id, c("_score", "_rank", "_decile"))))
  result |>
    dplyr::select(
      country, iso3, region, subregion,
      dplyr::all_of(triple_columns),
      ghi_score_label, mpi_reference_year,
      debt_distress_class, debt_distress_ordinal,
      indices_ranked_count, top_10_count, top_20_count
    )
}

build_source_manifest <- function(registry, paths, quality) {
  coverage <- quality$coverage |>
    dplyr::select(
      index_id, n_numeric_scores, n_labelled_records,
      n_ranked, coverage_ok
    )
  index_manifest <- registry |>
    dplyr::mutate(
      local_file = vapply(.data$index_id, function(id) relative_path(paths[[id]]), character(1)),
      retrieval_date = as.character(SNAPSHOT_DATE),
      sha256 = vapply(.data$index_id, function(id) sha256_file(paths[[id]]), character(1)),
      file_bytes = vapply(.data$index_id, function(id) as.numeric(file.info(paths[[id]])$size), numeric(1)),
      source_status = dplyr::if_else(
        .data$file_type == "manual_csv" & .data$file_bytes <= 120,
        "manual input template; data unavailable",
        "available"
      )
    ) |>
    dplyr::left_join(coverage, by = "index_id") |>
    dplyr::transmute(
      source_id = .data$index_id,
      source_name = .data$index_name,
      edition = .data$edition,
      reference_year = .data$reference_year,
      source_url = .data$source_url,
      download_url = .data$download_url,
      local_file = .data$local_file,
      retrieval_date = .data$retrieval_date,
      sha256 = .data$sha256,
      file_bytes = .data$file_bytes,
      source_status = .data$source_status,
      n_numeric_scores = .data$n_numeric_scores,
      n_labelled_records = .data$n_labelled_records,
      n_ranked = .data$n_ranked,
      coverage_ok = .data$coverage_ok,
      license_notes = .data$license_notes,
      source_notes = .data$source_notes
    )

  geo <- geography_registry()
  geo_path <- paths[["un_m49"]]
  geo_manifest <- tibble::tibble(
    source_id = geo$source_id,
    source_name = geo$source_name,
    edition = geo$edition,
    reference_year = NA_character_,
    source_url = geo$source_url,
    download_url = geo$download_url,
    local_file = relative_path(geo_path),
    retrieval_date = as.character(SNAPSHOT_DATE),
    sha256 = sha256_file(geo_path),
    file_bytes = as.numeric(file.info(geo_path)$size),
    source_status = "available",
    n_numeric_scores = NA_integer_,
    n_labelled_records = NA_integer_,
    n_ranked = NA_integer_,
    coverage_ok = TRUE,
    license_notes = geo$license_notes,
    source_notes = geo$source_notes
  )
  dplyr::bind_rows(index_manifest, geo_manifest)
}

write_pipeline_outputs <- function(master, long, wide, manifest, quality, qa) {
  ensure_directories()
  readr::write_csv(wide, "data/processed/humanitarian_indices_country.csv", na = "")
  saveRDS(wide, "data/processed/humanitarian_indices_country.rds", version = 3, compress = "gzip")
  readr::write_csv(long, "data/processed/humanitarian_indices_long.csv", na = "")
  saveRDS(long, "data/processed/humanitarian_indices_long.rds", version = 3, compress = "gzip")
  readr::write_csv(master, "data/processed/master_geography.csv", na = "")
  readr::write_csv(manifest, "artifacts/source_manifest.csv", na = "")
  readr::write_csv(quality$coverage, "artifacts/coverage_report.csv", na = "")
  readr::write_csv(quality$checks, "artifacts/data_quality_checks.csv", na = "")
  readr::write_csv(dplyr::distinct(qa$excluded), "artifacts/exclusions_report.csv", na = "")
  readr::write_csv(dplyr::distinct(qa$crosswalk_used), "artifacts/country_crosswalk_report.csv", na = "")
  readr::write_csv(dplyr::distinct(qa$unmatched), "artifacts/unmatched_entities_report.csv", na = "")
  readr::write_csv(dplyr::distinct(qa$messages), "artifacts/pipeline_messages.csv", na = "")

  checksum_targets <- c(
    "data/processed/humanitarian_indices_country.csv",
    "data/processed/humanitarian_indices_country.rds",
    "data/processed/humanitarian_indices_long.csv",
    "data/processed/humanitarian_indices_long.rds",
    "data/processed/master_geography.csv",
    "artifacts/source_manifest.csv",
    "artifacts/coverage_report.csv",
    "artifacts/data_quality_checks.csv",
    "artifacts/exclusions_report.csv",
    "artifacts/country_crosswalk_report.csv",
    "artifacts/unmatched_entities_report.csv",
    "artifacts/pipeline_messages.csv"
  )
  checksums <- tibble::tibble(
    file = checksum_targets,
    sha256 = vapply(checksum_targets, sha256_file, character(1))
  )
  readr::write_csv(checksums, "artifacts/processed_file_checksums.csv")
  invisible(checksums)
}

run_humanitarian_indices_pipeline <- function(refresh = FALSE) {
  check_dependencies()
  ensure_directories()
  registry <- source_registry()
  qa <- new_qa_state()
  paths <- download_sources(registry, refresh = refresh)
  master <- build_master_geography(paths)

  adapters <- lapply(INDEX_ORDER, function(index_id) {
    message("Adapter: ", index_id)
    ADAPTERS[[index_id]](paths[[index_id]], registry, master, qa)
  })
  names(adapters) <- INDEX_ORDER

  long <- dplyr::bind_rows(lapply(INDEX_ORDER, function(index_id) {
    complete_index_coverage(adapters[[index_id]], index_id, registry, master)
  })) |>
    dplyr::mutate(index_id = factor(.data$index_id, levels = INDEX_ORDER)) |>
    dplyr::arrange(.data$country, .data$index_id) |>
    dplyr::mutate(index_id = as.character(.data$index_id))

  quality <- check_index_quality(long, registry)
  validate_acceptance(master, long, quality)
  wide <- build_wide_dataset(master, long)
  assert_unique_nonmissing(wide, "iso3", "Wide country dataset")
  manifest <- build_source_manifest(registry, paths, quality)
  checksums <- write_pipeline_outputs(master, long, wide, manifest, quality, qa)

  message("Complete: ", nrow(wide), " countries, ", length(INDEX_ORDER), " indices.")
  invisible(list(master = master, long = long, wide = wide, manifest = manifest, quality = quality, checksums = checksums))
}
