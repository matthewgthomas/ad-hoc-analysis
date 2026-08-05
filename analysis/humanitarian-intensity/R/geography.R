read_un_m49 <- function(path) {
  tables <- rvest::read_html(path) |>
    rvest::html_elements("table") |>
    rvest::html_table(fill = TRUE)
  if (!length(tables)) stop("No table found in UN M49 source.", call. = FALSE)
  raw <- as.data.frame(tables[[1]], stringsAsFactors = FALSE)
  if (ncol(raw) < 12L) stop("UN M49 table structure changed.", call. = FALSE)

  tibble::tibble(
    country_m49 = as.character(raw[-1, 9]),
    m49_code = stringr::str_pad(as.character(raw[-1, 10]), 3, pad = "0"),
    iso3 = clean_iso3(raw[-1, 12]),
    region = stringr::str_squish(as.character(raw[-1, 4])),
    subregion = stringr::str_squish(as.character(raw[-1, 6])),
    intermediate_region = stringr::str_squish(as.character(raw[-1, 8]))
  ) |>
    dplyr::mutate(
      region = dplyr::na_if(.data$region, ""),
      subregion = dplyr::na_if(.data$subregion, ""),
      intermediate_region = dplyr::na_if(.data$intermediate_region, "")
    ) |>
    dplyr::filter(!is.na(.data$iso3)) |>
    dplyr::distinct(.data$iso3, .keep_all = TRUE)
}

build_master_geography <- function(paths) {
  worldrisk_raw <- readxl::read_excel(paths[["worldrisk"]], sheet = 1)
  worldrisk <- tibble::tibble(
      country = stringr::str_squish(as.character(worldrisk_raw[[1]])),
      iso3 = clean_iso3(worldrisk_raw[[2]])
    ) |>
    dplyr::filter(!is.na(.data$iso3)) |>
    dplyr::distinct(.data$iso3, .keep_all = TRUE)

  if (nrow(worldrisk) != 193L) {
    stop("WorldRiskIndex should define 193 UN member states; found ", nrow(worldrisk), ".", call. = FALSE)
  }

  m49 <- read_un_m49(paths[["un_m49"]])
  observers <- m49 |>
    dplyr::filter(.data$iso3 %in% c("PSE", "VAT")) |>
    dplyr::transmute(country = .data$country_m49, iso3 = .data$iso3)
  if (nrow(observers) != 2L) stop("Could not locate both UN observer states in M49.", call. = FALSE)

  master <- dplyr::bind_rows(worldrisk, observers) |>
    dplyr::left_join(
      m49 |> dplyr::select(iso3, m49_code, region, subregion, intermediate_region),
      by = "iso3"
    ) |>
    dplyr::arrange(.data$country) |>
    dplyr::select(country, iso3, m49_code, region, subregion, intermediate_region)

  assert_unique_nonmissing(master, "iso3", "Master geography")
  if (nrow(master) != 195L) stop("Master geography must contain 195 countries; found ", nrow(master), ".", call. = FALSE)
  if (any(is.na(master$region) | is.na(master$subregion))) {
    bad <- master$iso3[is.na(master$region) | is.na(master$subregion)]
    stop("Missing UN M49 region/subregion for: ", paste(bad, collapse = ", "), call. = FALSE)
  }
  master
}
