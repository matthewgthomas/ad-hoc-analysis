validate_download <- function(path, file_type) {
  if (!file.exists(path) || file.info(path)$size == 0) {
    stop("Downloaded file is missing or empty: ", path, call. = FALSE)
  }
  prefix <- readBin(path, what = "raw", n = 8)
  prefix_text <- rawToChar(prefix, multiple = TRUE) |> paste(collapse = "")
  zip_signature <- length(prefix) >= 2L && identical(prefix[1:2], charToRaw("PK"))
  valid <- switch(
    file_type,
    xlsx = zip_signature,
    zip = zip_signature,
    pdf = startsWith(prefix_text, "%PDF"),
    html = grepl("<(!doctype|html)", tolower(paste(readLines(path, warn = FALSE, n = 5), collapse = " "))),
    csv = file.info(path)$size > 20,
    tsv = file.info(path)$size > 20,
    TRUE
  )
  if (!isTRUE(valid)) {
    stop("Downloaded file does not look like a valid ", file_type, ": ", path, call. = FALSE)
  }
  invisible(TRUE)
}

download_one <- function(id, url, path, file_type, refresh = FALSE) {
  if (file.exists(path) && !refresh) {
    validate_download(path, file_type)
    return(path)
  }
  if (is.na(url) || !nzchar(url)) stop("No download URL configured for ", id, call. = FALSE)

  headers <- source_download_headers(id)
  request <- httr2::request(url) |>
    httr2::req_user_agent(headers[["User-Agent"]]) |>
    httr2::req_timeout(180) |>
    httr2::req_retry(max_tries = 4)
  if (!is.null(headers$Referer)) {
    request <- request |> httr2::req_headers(Referer = headers$Referer)
  }
  response <- httr2::req_perform(request)
  if (httr2::resp_status(response) >= 400) {
    stop("Download failed for ", id, " with HTTP ", httr2::resp_status(response), call. = FALSE)
  }
  writeBin(httr2::resp_body_raw(response), path)
  validate_download(path, file_type)
  path
}

download_sources <- function(registry, refresh = FALSE) {
  ensure_directories()
  paths <- stats::setNames(rep(NA_character_, nrow(registry)), registry$index_id)
  for (i in seq_len(nrow(registry))) {
    spec <- registry[i, ]
    if (spec$file_type == "manual_csv") {
      paths[[spec$index_id]] <- file.path("data/manual", spec$file_name)
      next
    }
    path <- file.path("data/raw", spec$file_name)
    message("Source: ", spec$index_name)
    paths[[spec$index_id]] <- download_one(
      spec$index_id, spec$download_url, path, spec$file_type, refresh = refresh
    )
  }

  geo <- geography_registry()
  geo_path <- file.path("data/raw", geo$file_name)
  message("Source: ", geo$source_name)
  paths[["un_m49"]] <- download_one(
    geo$source_id, geo$download_url, geo_path, geo$file_type, refresh = refresh
  )
  paths
}
