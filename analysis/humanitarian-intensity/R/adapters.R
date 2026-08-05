read_raw_excel <- function(path, sheet, skip = 0L) {
  as.data.frame(
    readxl::read_excel(path, sheet = sheet, skip = skip, col_names = FALSE, .name_repair = "minimal"),
    stringsAsFactors = FALSE
  )
}

adapt_inform_risk <- function(path, registry, master, qa) {
  raw <- read_raw_excel(path, "INFORM Risk 2026 (a-z)", skip = 2)
  data <- tibble::tibble(
    source_country = raw[[1]],
    iso3 = raw[[2]],
    score = raw[[3]],
    score_label = NA_character_
  ) |>
    dplyr::filter(!is.na(.data$iso3), !is.na(.data$score))
  standard_adapter(data, "inform_risk", registry, master, qa)
}

adapt_inform_severity <- function(path, registry, master, qa) {
  raw <- read_raw_excel(path, "INFORM Severity - country", skip = 4)
  data <- tibble::tibble(
    source_country = raw[[3]],
    iso3 = raw[[4]],
    score = raw[[6]],
    score_label = NA_character_
  ) |>
    dplyr::filter(!is.na(.data$iso3), !is.na(.data$score))
  standard_adapter(data, "inform_severity", registry, master, qa)
}

adapt_underfunded_crisis <- function(path, registry, master, qa) {
  document <- rvest::read_html(path)
  scripts <- document |>
    rvest::html_elements('script[type="application/json"]') |>
    rvest::html_text2()
  selected <- scripts[stringr::str_detect(scripts, '"cum_percent_met"')]
  if (length(selected) != 1L) stop("Could not identify Underfunded Crisis data payload.", call. = FALSE)
  payload <- jsonlite::fromJSON(selected, simplifyVector = TRUE)
  raw <- tibble::as_tibble(payload$x$tag$attribs$data)

  excluded <- raw |>
    dplyr::filter(stringr::str_detect(.data$context, "^Regional:")) |>
    dplyr::transmute(
      index_id = "underfunded_crisis",
      source_country = .data$context,
      reason = "regional or multi-country response plan"
    )
  qa_append(qa, "excluded", excluded)

  data <- raw |>
    dplyr::filter(!stringr::str_detect(.data$context, "^Regional:")) |>
    dplyr::transmute(
      source_country = .data$context,
      iso3 = map_country_names(.data$context, "underfunded_crisis", master, qa),
      score = 100 * as.numeric(.data$cum_percent_met),
      score_label = paste0(format(round(100 * as.numeric(.data$cum_percent_met), 1), nsmall = 1), "%")
    )
  standard_adapter(data, "underfunded_crisis", registry, master, qa)
}

adapt_oecd_fragility <- function(path, registry, master, qa) {
  raw <- readr::read_tsv(path, locale = readr::locale(decimal_mark = ","), show_col_types = FALSE)
  data <- raw |>
    dplyr::transmute(
      source_country = .data$country,
      iso3 = .data$iso3,
      score = as.numeric(.data$overall),
      score_label = NA_character_
    )
  standard_adapter(data, "oecd_fragility", registry, master, qa)
}

adapt_worldrisk <- function(path, registry, master, qa) {
  raw <- readxl::read_excel(path, sheet = 1)
  data <- tibble::tibble(
    source_country = raw[[1]],
    iso3 = raw[[2]],
    score = raw[[3]],
    score_label = NA_character_
  )
  standard_adapter(data, "worldrisk", registry, master, qa)
}

adapt_nd_gain <- function(path, registry, master, qa) {
  listing <- utils::unzip(path, list = TRUE)$Name
  entry <- listing[stringr::str_detect(listing, "(?<!__MACOSX)/gain/gain\\.csv$")]
  if (length(entry) != 1L) stop("Could not identify ND-GAIN overall score file in ZIP.", call. = FALSE)
  connection <- unz(path, entry, open = "rb")
  on.exit(close(connection), add = TRUE)
  raw <- readr::read_csv(connection, show_col_types = FALSE)
  years <- names(raw)[stringr::str_detect(names(raw), "^[0-9]{4}$")]
  latest_year <- as.character(max(as.integer(years)))
  data <- raw |>
    dplyr::transmute(
      source_country = .data$Name,
      iso3 = .data$ISO3,
      score = as.numeric(.data[[latest_year]]),
      score_label = NA_character_,
      reference_year = latest_year
    ) |>
    dplyr::filter(!is.na(.data$score))
  standard_adapter(data, "nd_gain", registry, master, qa)
}

adapt_hdi <- function(path, registry, master, qa) {
  raw <- readr::read_csv(path, show_col_types = FALSE)
  aggregates <- raw |>
    dplyr::filter(!stringr::str_detect(.data$iso3, "^[A-Z]{3}$"), !is.na(.data$hdi_2023)) |>
    dplyr::transmute(
      index_id = "hdi",
      source_country = .data$country,
      reason = "publisher aggregate rather than country"
    )
  qa_append(qa, "excluded", aggregates)
  data <- raw |>
    dplyr::filter(stringr::str_detect(.data$iso3, "^[A-Z]{3}$")) |>
    dplyr::transmute(
      source_country = .data$country,
      iso3 = .data$iso3,
      score = as.numeric(.data$hdi_2023),
      score_label = NA_character_,
      reference_year = "2023"
    ) |>
    dplyr::filter(!is.na(.data$score))
  standard_adapter(data, "hdi", registry, master, qa)
}

adapt_mpi <- function(path, registry, master, qa) {
  raw <- read_raw_excel(path, "gMPI_Table1", skip = 7)
  score <- suppressWarnings(as.numeric(raw[[4]]))
  aggregate_names <- normalise_country_name(c(
    "Developing countries", "Small island developing states", "Arab States",
    "East Asia and the Pacific", "Europe and Central Asia",
    "Latin America and the Caribbean", "South Asia", "Sub-Saharan Africa"
  ))
  is_aggregate <- normalise_country_name(raw[[1]]) %in% aggregate_names
  qa_append(
    qa,
    "excluded",
    tibble::tibble(
      index_id = "mpi",
      source_country = as.character(raw[[1]][!is.na(score) & is_aggregate]),
      reason = "publisher aggregate rather than country"
    )
  )
  keep <- !is.na(score) & !is.na(raw[[1]]) & !is_aggregate
  countries <- as.character(raw[[1]][keep])
  data <- tibble::tibble(
    source_country = countries,
    iso3 = map_country_names(countries, "mpi", master, qa),
    score = score[keep],
    score_label = NA_character_,
    reference_year = as.character(raw[[2]][keep])
  )
  standard_adapter(data, "mpi", registry, master, qa)
}

adapt_ghi <- function(path, registry, master, qa) {
  rows <- rvest::read_html(path) |>
    rvest::html_elements("table tr")
  parsed <- lapply(rows, function(row) {
    cells <- rvest::html_elements(row, "td")
    if (length(cells) < 6L) return(NULL)
    countries <- cells[[2]] |>
      rvest::html_elements("a") |>
      rvest::html_text2()
    if (!length(countries)) countries <- rvest::html_text2(cells[[2]])
    score_text <- rvest::html_text2(cells[[6]]) |> stringr::str_squish()
    hidden <- cells[[6]] |>
      rvest::html_elements("span.stealth") |>
      rvest::html_text2()
    score_label <- if (length(hidden)) {
      stringr::str_remove(score_text, stringr::fixed(hidden[[1]])) |> stringr::str_squish()
    } else {
      score_text
    }
    tibble::tibble(source_country = countries, score_label = score_label)
  }) |>
    dplyr::bind_rows()
  exact <- stringr::str_detect(parsed$score_label, "^[0-9]+(?:\\.[0-9]+)?$")
  countries <- parsed$source_country
  data <- parsed |>
    dplyr::mutate(
      iso3 = map_country_names(countries, "ghi", master, qa),
      score = suppressWarnings(dplyr::if_else(exact, as.numeric(.data$score_label), NA_real_)),
      reference_year = "2025"
    ) |>
    dplyr::select(source_country, iso3, score, score_label, reference_year)
  standard_adapter(data, "ghi", registry, master, qa)
}

adapt_ghs <- function(path, registry, master, qa) {
  raw <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::filter(.data$Year == 2021)
  countries <- as.character(raw$Country)
  data <- tibble::tibble(
    source_country = countries,
    iso3 = map_country_names(countries, "ghs", master, qa),
    score = as.numeric(raw[["OVERALL SCORE"]]),
    score_label = NA_character_,
    reference_year = "2021"
  )
  standard_adapter(data, "ghs", registry, master, qa)
}

adapt_wps <- function(path, registry, master, qa) {
  raw <- read_raw_excel(path, "TABLE 1", skip = 6)
  data <- tibble::tibble(
    source_country = raw[[2]],
    iso3 = raw[[3]],
    score = suppressWarnings(as.numeric(raw[[5]])),
    score_label = NA_character_,
    reference_year = "2025"
  ) |>
    dplyr::filter(stringr::str_detect(as.character(.data$iso3), "^[A-Z]{3}$"), !is.na(.data$score))
  standard_adapter(data, "wps", registry, master, qa)
}

adapt_un_mvi <- function(path, registry, master, qa) {
  pages <- pdftools::pdf_text(path)
  lines <- unlist(stringr::str_split(pages[seq_len(min(3L, length(pages)))], "\\n"))
  pattern <- "^\\s*(.*?)\\s+([A-Z]{3})\\s+([0-9]+\\.[0-9])\\s+([0-9]+\\.[0-9])\\s+([0-9]+\\.[0-9])\\s*$"
  matched <- stringr::str_match(lines, pattern)
  matched <- matched[!is.na(matched[, 1]), , drop = FALSE]
  data <- tibble::tibble(
    source_country = matched[, 2],
    iso3 = matched[, 3],
    score = as.numeric(matched[, 4]),
    score_label = NA_character_,
    reference_year = "2023"
  )
  standard_adapter(data, "un_mvi", registry, master, qa)
}

adapt_debt_distress <- function(path, registry, master, qa) {
  lines <- unlist(stringr::str_split(pdftools::pdf_text(path), "\\n"))
  pattern <- "^\\s*([0-9]+)\\s+(.*?)\\s+([0-9]{1,2}/[0-9]{1,2}/[0-9]{4})\\s+(In debt distress|High|Moderate|Low)\\b"
  matched <- stringr::str_match(lines, pattern)
  matched <- matched[!is.na(matched[, 1]), , drop = FALSE]
  countries <- stringr::str_remove(stringr::str_squish(matched[, 3]), "\\s+4/$")
  classes <- matched[, 5]
  ordinal <- dplyr::recode(classes, "In debt distress" = 4, "High" = 3, "Moderate" = 2, "Low" = 1)
  data <- tibble::tibble(
    source_country = c(countries, "Eritrea"),
    iso3 = c(map_country_names(countries, "debt_distress", master, qa), "ERI"),
    score = c(as.numeric(ordinal), NA_real_),
    score_label = c(classes, "No current DSA"),
    reference_year = c(matched[, 4], NA_character_)
  )
  standard_adapter(data, "debt_distress", registry, master, qa)
}

adapt_searo <- function(path, registry, master, qa) {
  raw <- read_raw_excel(path, "SEARO", skip = 4)
  data <- tibble::tibble(
    source_country = raw[[1]],
    iso3 = raw[[3]],
    score = suppressWarnings(as.numeric(raw[[5]])),
    score_label = NA_character_,
    reference_year = "December 2025"
  ) |>
    dplyr::filter(stringr::str_detect(as.character(.data$iso3), "^[A-Z]{3}$"), !is.na(.data$score))
  standard_adapter(data, "searo", registry, master, qa)
}

adapt_disaster_displacement <- function(path, registry, master, qa) {
  if (!file.exists(path) || file.info(path)$size == 0L) {
    return(empty_adapter(
      "disaster_displacement", registry, master, qa,
      "Manual IDMC GDRM 2.0 country export is missing; all scores remain NA."
    ))
  }
  raw <- readr::read_csv(path, show_col_types = FALSE, col_types = readr::cols(.default = readr::col_character()))
  required <- c("iso3", "country", "aad_current_multihazard", "scenario", "metric", "hazard_scope", "source_snapshot_date", "aggregation_level", "notes")
  missing <- setdiff(required, names(raw))
  if (length(missing)) stop("Manual GDRM file is missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
  if (!nrow(raw)) {
    return(empty_adapter(
      "disaster_displacement", registry, master, qa,
      "Manual IDMC GDRM 2.0 template contains no data; all scores remain NA."
    ))
  }
  if (any(tolower(raw$scenario) != "current")) stop("GDRM scenario must be Current for every row.", call. = FALSE)
  if (any(toupper(raw$metric) != "AAD")) stop("GDRM metric must be AAD for every row.", call. = FALSE)
  hazard <- normalise_country_name(raw$hazard_scope)
  if (any(!hazard %in% c("multi hazard", "multihazard", "all"))) stop("GDRM hazard_scope must be Multi-hazard or All.", call. = FALSE)
  score <- suppressWarnings(as.numeric(raw$aad_current_multihazard))
  if (any(is.na(score) | score < 0)) stop("GDRM AAD values must be non-negative numbers.", call. = FALSE)
  if (anyDuplicated(clean_iso3(raw$iso3))) stop("GDRM manual file contains duplicate ISO3 rows.", call. = FALSE)
  data <- tibble::tibble(
    source_country = raw$country,
    iso3 = raw$iso3,
    score = score,
    score_label = NA_character_,
    reference_year = raw$source_snapshot_date
  )
  standard_adapter(data, "disaster_displacement", registry, master, qa)
}

adapt_internal_displacement <- function(path, registry, master, qa) {
  raw <- read_raw_excel(path, "IDI 2022 values ", skip = 3)
  data <- tibble::tibble(
    source_country = raw[[3]],
    iso3 = raw[[1]],
    score = suppressWarnings(as.numeric(raw[[46]])),
    score_label = NA_character_,
    reference_year = "2022"
  ) |>
    dplyr::filter(stringr::str_detect(as.character(.data$iso3), "^[A-Z]{3}$"), !is.na(.data$score))
  standard_adapter(data, "internal_displacement", registry, master, qa)
}

ADAPTERS <- list(
  inform_risk = adapt_inform_risk,
  inform_severity = adapt_inform_severity,
  underfunded_crisis = adapt_underfunded_crisis,
  oecd_fragility = adapt_oecd_fragility,
  worldrisk = adapt_worldrisk,
  nd_gain = adapt_nd_gain,
  hdi = adapt_hdi,
  mpi = adapt_mpi,
  ghi = adapt_ghi,
  ghs = adapt_ghs,
  wps = adapt_wps,
  un_mvi = adapt_un_mvi,
  debt_distress = adapt_debt_distress,
  searo = adapt_searo,
  disaster_displacement = adapt_disaster_displacement,
  internal_displacement = adapt_internal_displacement
)
