required_packages <- c(
  "countrycode", "digest", "dplyr", "httr2", "jsonlite", "pdftools",
  "purrr", "readr", "readxl", "rvest", "stringr", "tibble", "tidyr"
)

check_dependencies <- function() {
  missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop(
      "Missing R packages: ", paste(missing, collapse = ", "),
      ". Run `Rscript -e 'renv::restore()'` first.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

ensure_directories <- function() {
  dirs <- c("data/raw", "data/processed", "data/manual", "artifacts")
  invisible(vapply(dirs, dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))
}

normalise_country_name <- function(x) {
  x |>
    stringr::str_replace_all("&", " and ") |>
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", " ") |>
    stringr::str_squish()
}

new_qa_state <- function() {
  env <- new.env(parent = emptyenv())
  env$unmatched <- tibble::tibble(index_id = character(), source_country = character(), reason = character())
  env$excluded <- tibble::tibble(index_id = character(), source_country = character(), reason = character())
  env$crosswalk_used <- tibble::tibble(index_id = character(), source_country = character(), iso3 = character(), mapping = character())
  env$messages <- tibble::tibble(index_id = character(), severity = character(), message = character())
  env
}

qa_append <- function(qa, field, rows) {
  if (!is.null(rows) && nrow(rows)) {
    qa[[field]] <- dplyr::bind_rows(qa[[field]], rows)
  }
  invisible(qa)
}

qa_message <- function(qa, index_id, severity, message) {
  qa_append(
    qa,
    "messages",
    tibble::tibble(index_id = index_id, severity = severity, message = message)
  )
}

assert_unique_nonmissing <- function(data, key, label) {
  values <- data[[key]]
  if (any(is.na(values) | values == "")) {
    stop(label, " contains missing ", key, " values.", call. = FALSE)
  }
  duplicates <- unique(values[duplicated(values)])
  if (length(duplicates)) {
    stop(label, " contains duplicate ", key, " values: ", paste(duplicates, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

clean_iso3 <- function(x) {
  x <- stringr::str_to_upper(stringr::str_squish(as.character(x)))
  dplyr::if_else(stringr::str_detect(x, "^[A-Z]{3}$"), x, NA_character_)
}

build_name_lookup <- function(master) {
  master_lookup <- master |>
    dplyr::transmute(
      normalised_name = normalise_country_name(.data$country),
      iso3 = .data$iso3,
      mapping = "master country name"
    )

  alias_lookup <- COUNTRY_ALIASES |>
    dplyr::transmute(
      normalised_name = normalise_country_name(.data$alias),
      iso3 = .data$iso3,
      mapping = paste0("explicit alias: ", .data$note)
    )

  lookup <- dplyr::bind_rows(master_lookup, alias_lookup) |>
    dplyr::distinct(.data$normalised_name, .data$iso3, .keep_all = TRUE)

  conflicts <- lookup |>
    dplyr::count(.data$normalised_name) |>
    dplyr::filter(.data$n > 1)
  if (nrow(conflicts)) {
    stop("Country name lookup contains conflicting aliases: ", paste(conflicts$normalised_name, collapse = ", "), call. = FALSE)
  }
  lookup
}

map_country_names <- function(country, index_id, master, qa) {
  lookup <- build_name_lookup(master)
  input <- tibble::tibble(
    row_id = seq_along(country),
    source_country = stringr::str_squish(as.character(country)),
    normalised_name = normalise_country_name(country)
  )
  mapped <- input |>
    dplyr::left_join(lookup, by = "normalised_name") |>
    dplyr::arrange(.data$row_id)

  used <- mapped |>
    dplyr::filter(!is.na(.data$iso3)) |>
    dplyr::transmute(index_id = index_id, source_country = .data$source_country, iso3 = .data$iso3, mapping = .data$mapping)
  qa_append(qa, "crosswalk_used", used)

  mapped$iso3
}

validate_iso3 <- function(iso3, index_id) {
  iso3 <- clean_iso3(iso3)
  known <- unique(stats::na.omit(countrycode::codelist$iso3c))
  custom <- c("XKX")
  invalid <- unique(stats::na.omit(iso3[!iso3 %in% c(known, custom)]))
  if (length(invalid)) {
    stop(index_id, " contains invalid ISO3 codes: ", paste(invalid, collapse = ", "), call. = FALSE)
  }
  iso3
}

standard_adapter <- function(
  data,
  index_id,
  registry,
  master,
  qa,
  source_country = NULL,
  reference_year = NULL
) {
  spec <- registry[registry$index_id == index_id, , drop = FALSE]
  if (nrow(spec) != 1L) stop("Missing registry row for ", index_id, call. = FALSE)

  if (!"iso3" %in% names(data)) data$iso3 <- NA_character_
  if (!"score" %in% names(data)) data$score <- NA_real_
  if (!"score_label" %in% names(data)) data$score_label <- NA_character_
  if (!"source_country" %in% names(data)) {
    data$source_country <- if (is.null(source_country)) NA_character_ else source_country
  }
  if (!"reference_year" %in% names(data)) {
    data$reference_year <- if (is.null(reference_year)) spec$reference_year else reference_year
  }

  data <- data |>
    dplyr::mutate(
      iso3 = validate_iso3(.data$iso3, index_id),
      score = suppressWarnings(as.numeric(.data$score)),
      score_label = dplyr::na_if(stringr::str_squish(as.character(.data$score_label)), ""),
      source_country = dplyr::na_if(stringr::str_squish(as.character(.data$source_country)), ""),
      reference_year = as.character(.data$reference_year)
    )

  unresolved <- data |>
    dplyr::filter(is.na(.data$iso3), !is.na(.data$score) | !is.na(.data$score_label)) |>
    dplyr::transmute(index_id = index_id, source_country = .data$source_country, reason = "country name not mapped to ISO3")
  qa_append(qa, "unmatched", unresolved)
  if (nrow(unresolved)) {
    stop(index_id, " has scored entities without an explicit ISO3 mapping: ", paste(unique(unresolved$source_country), collapse = ", "), call. = FALSE)
  }

  outside <- data |>
    dplyr::filter(!is.na(.data$iso3), !.data$iso3 %in% master$iso3) |>
    dplyr::transmute(index_id = index_id, source_country = dplyr::coalesce(.data$source_country, .data$iso3), reason = "valid ISO3 outside 195-country master universe")
  qa_append(qa, "excluded", outside)

  data <- data |>
    dplyr::filter(.data$iso3 %in% master$iso3) |>
    dplyr::select(iso3, source_country, score, score_label, reference_year)

  duplicate_iso <- data |>
    dplyr::filter(!is.na(.data$score) | !is.na(.data$score_label)) |>
    dplyr::count(.data$iso3) |>
    dplyr::filter(.data$n > 1)
  if (nrow(duplicate_iso)) {
    stop(index_id, " contains duplicate scored ISO3 rows: ", paste(duplicate_iso$iso3, collapse = ", "), call. = FALSE)
  }

  data |>
    dplyr::mutate(
      index_id = index_id,
      index_name = spec$index_name,
      edition = spec$edition,
      score_direction = spec$score_direction,
      rankable = spec$rankable,
      eligible_for_counts = spec$eligible_for_counts
    ) |>
    dplyr::select(
      iso3, index_id, index_name, source_country,
      score, score_label, reference_year, edition,
      score_direction, rankable, eligible_for_counts
    )
}

empty_adapter <- function(index_id, registry, master, qa, message = NULL) {
  if (!is.null(message)) qa_message(qa, index_id, "warning", message)
  standard_adapter(
    tibble::tibble(iso3 = character(), source_country = character(), score = numeric(), score_label = character()),
    index_id, registry, master, qa
  )
}

rank_index <- function(data) {
  if (!nrow(data)) return(data)
  rankable <- isTRUE(unique(data$rankable))
  n_scored <- sum(!is.na(data$score))
  if (!rankable || n_scored == 0L) {
    return(data |>
      dplyr::mutate(
        n_scored = n_scored,
        rank = NA_integer_,
        decile = NA_integer_,
        top_10 = NA,
        top_20 = NA
      ))
  }

  direction <- unique(data$score_direction)
  if (length(direction) != 1L || !direction %in% c("higher_worse", "lower_worse")) {
    stop("Invalid score direction for ", unique(data$index_id), call. = FALSE)
  }

  vulnerability_order <- if (direction == "higher_worse") -data$score else data$score
  ranks <- dplyr::min_rank(vulnerability_order)
  deciles <- pmin(10L, floor(10 * (ranks - 1) / n_scored) + 1L)
  eligible <- isTRUE(unique(data$eligible_for_counts))

  data |>
    dplyr::mutate(
      n_scored = n_scored,
      rank = as.integer(ranks),
      decile = as.integer(deciles),
      top_10 = dplyr::if_else(eligible & !is.na(.data$decile), .data$decile == 1L, NA),
      top_20 = dplyr::if_else(eligible & !is.na(.data$decile), .data$decile <= 2L, NA)
    )
}

sha256_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  unname(digest::digest(file = path, algo = "sha256", serialize = FALSE))
}

relative_path <- function(path) {
  root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  absolute <- normalizePath(path, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (identical(absolute, root)) return(".")
  if (startsWith(absolute, prefix)) return(substring(absolute, nchar(prefix) + 1L))
  absolute
}
