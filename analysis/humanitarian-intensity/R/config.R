SNAPSHOT_DATE <- as.Date("2026-08-04")

INDEX_ORDER <- c(
  "inform_risk", "inform_severity", "underfunded_crisis",
  "oecd_fragility", "worldrisk", "nd_gain", "hdi", "mpi", "ghi",
  "ghs", "wps", "un_mvi", "debt_distress", "searo", "disaster_displacement",
  "internal_displacement"
)

source_registry <- function() {
  tibble::tribble(
    ~index_id, ~index_name, ~edition, ~reference_year, ~source_url, ~download_url, ~file_name, ~file_type, ~score_direction, ~rankable, ~eligible_for_counts, ~expected_min, ~expected_max, ~license_notes, ~source_notes,
    "inform_risk", "INFORM Risk", "2026 v0.7.2", "2026", "https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Risk/Results-and-data", "https://drmkc.jrc.ec.europa.eu/inform-index/Portals/0/InfoRM/2026/INFORM_Risk_2026_v072.xlsx", "inform_risk_2026.xlsx", "xlsx", "higher_worse", TRUE, TRUE, 191L, 191L, "European Commission reuse terms; cite INFORM", "Release dated 31 March 2026" ,
    "inform_severity", "INFORM Severity", "June 2026", "2026-06", "https://drmkc.jrc.ec.europa.eu/inform-index/INFORM-Severity/Results-and-data", "https://drmkc.jrc.ec.europa.eu/inform-index/Portals/0/InfoRM/Severity/2026/202606_INFORM_Severity_-_June_2026__2_.xlsx", "inform_severity_2026_06.xlsx", "xlsx", "higher_worse", TRUE, TRUE, 60L, 75L, "European Commission reuse terms; cite INFORM", "Publisher-provided country sheet" ,
    "underfunded_crisis", "Underfunded Crisis Index", "2025", "2021-2025 cumulative", "https://humanitarianfundingforecast.org/index-underfunded-crisis/", "https://humanitarianfundingforecast.org/wp-content/uploads/2025/11/underfundingtable25.html", "underfunded_crisis_2025.html", "html", "lower_worse", TRUE, TRUE, 25L, 30L, "Publisher copyright; cite Humanitarian Funding Forecast", "Country contexts only; regional plans excluded" ,
    "oecd_fragility", "OECD Multidimensional Fragility", "States of Fragility 2025", "predominantly 2023", "https://www.oecd.org/en/data/dashboards/multidimensional-fragility.html", "https://gitvfd.github.io/states-of-fragility-snail/data-new.tsv", "oecd_fragility_2025.tsv", "tsv", "lower_worse", TRUE, TRUE, 61L, 61L, "OECD terms; cite States of Fragility 2025", "Public artifact exposes 61 high/extreme contexts" ,
    "worldrisk", "WorldRiskIndex", "2025", "2025", "https://weltrisikobericht.de/worldriskreport/", "https://weltrisikobericht.de/download/4568/?tmstv=1758617984", "worldriskindex_2025.xlsx", "xlsx", "higher_worse", TRUE, TRUE, 193L, 193L, "CC BY 4.0", "All 193 UN member states" ,
    "nd_gain", "ND-GAIN Country Index", "2026 release", "2024", "https://gain.nd.edu/our-work/country-index/download-data/", "https://gain.nd.edu/assets/647440/ndgain_countryindex_2026.zip", "ndgain_countryindex_2026.zip", "zip", "lower_worse", TRUE, TRUE, 175L, 190L, "Free/open-access data; cite ND-GAIN", "Download requires browser user-agent and referring page" ,
    "hdi", "Human Development Index", "Human Development Report 2025", "2023", "https://hdr.undp.org/data-center/documentation-and-downloads", "https://hdr.undp.org/sites/default/files/2025_HDR/HDR25_Composite_indices_complete_time_series.csv", "hdr25_composite_indices.csv", "csv", "lower_worse", TRUE, TRUE, 185L, 195L, "UNDP data terms; cite Human Development Report 2025", "Uses hdi_2023 from complete time series" ,
    "mpi", "Multidimensional Poverty Index", "Global MPI 2025", "country-specific survey year", "https://hdr.undp.org/content/2025-global-multidimensional-poverty-index-mpi", "https://hdr.undp.org/sites/default/files/publications/additional-files/2025-10/2025_gMPI_Table1and2.xlsx", "global_mpi_2025.xlsx", "xlsx", "higher_worse", TRUE, TRUE, 100L, 115L, "UNDP/OPHI terms; cite Global MPI 2025", "Country estimates use different survey years" ,
    "ghi", "Global Hunger Index", "2025", "2025", "https://www.globalhungerindex.org/ranking.html", "https://www.globalhungerindex.org/ranking.html", "global_hunger_index_2025.html", "html", "higher_worse", TRUE, TRUE, 90L, 105L, "Publisher terms; cite 2025 Global Hunger Index", "Only exact numeric scores are ranked; censored/range labels retained" ,
    "ghs", "Global Health Security Index", "2021", "2021", "https://ghsindex.org/global/report/", "https://ghsindex.org/wp-content/uploads/2026/07/2021-GHS-Index-April-2022.csv", "ghs_index_2021.csv", "csv", "lower_worse", TRUE, TRUE, 193L, 193L, "GHS Index publisher terms; cite 2021 edition", "Current worldwide edition; 195 published entities, of which 193 are in the selected master" ,
    "wps", "Women, Peace and Security Index", "2025/26", "2025", "https://giwps.georgetown.edu/the-index/", "https://giwps.georgetown.edu/wp-content/uploads/2025/10/WPS-Index-2025-Data.xlsx", "wps_index_2025_26.xlsx", "xlsx", "lower_worse", TRUE, TRUE, 177L, 177L, "GIWPS/PRIO terms; cite WPS Index 2025/26", "181 published entities; Taiwan, Hong Kong, Puerto Rico and Kosovo are outside the selected master" ,
    "un_mvi", "UN Multidimensional Vulnerability Index", "High-Level Panel results", "2023", "https://www.un.org/ohrlls/mvi/documents", "https://sdgs.un.org/sites/default/files/2023-09/MVI_Results.pdf", "un_mvi_results.pdf", "pdf", "higher_worse", TRUE, TRUE, 140L, 145L, "United Nations terms; cite MVI High-Level Panel", "Overall MVI score parsed from official results PDF" ,
    "debt_distress", "Debt-distress classification", "31 March 2026", "latest DSA as of 2026-03-31", "https://www.imf.org/external/pubs/ft/dsa/lic.htm", "https://www.imf.org/-/media/files/publications/ft/dsa/dsalist.pdf", "imf_lic_dsa_2026_03_31.pdf", "pdf", "higher_worse", FALSE, FALSE, 67L, 67L, "IMF terms; cite LIC DSA list", "Ordinal code is derived; no rank or decile" ,
    "searo", "Sexual Exploitation and Abuse Risk Overview", "2026 v1.2", "December 2025", "https://interagencystandingcommittee.org/psea-searo-index", "https://reliefweb.int/attachments/597fb34b-29fa-40b3-869a-e7114bd6d965/SEARO%20Global%202026%20v1.2%20%28Dec-25%29.xlsx", "searo_global_2026_v1_2.xlsx", "xlsx", "higher_worse", TRUE, TRUE, 35L, 50L, "IASC resource terms; cite SEARO", "Latest workbook linked by IASC at snapshot date" ,
    "disaster_displacement", "Disaster Displacement Risk Model", "GDRM 2.0", "current climate", "https://www.internal-displacement.org/monitoring-tools/displacement-risk/", NA_character_, "idmc_gdrm2_country.csv", "manual_csv", "higher_worse", TRUE, TRUE, 0L, 195L, "IDMC terms; cite GDRM 2.0", "Manual current-climate multi-hazard AAD export" ,
    "internal_displacement", "Internal Displacement Index", "IDI 2023 publication", "2022", "https://www.internal-displacement.org/25-years-of-progress-on-internal-displacement-1998-2023/", "https://api.internal-displacement.org/sites/default/files/2023-10/IDMC_IDI_values_2022.xlsx", "idmc_idi_values_2022.xlsx", "xlsx", "lower_worse", TRUE, TRUE, 44L, 44L, "CC BY-NC; cite IDMC IDI 2023", "Workbook contains 44 numeric 2022 IDI country values"
  ) |>
    dplyr::mutate(index_id = factor(.data$index_id, levels = INDEX_ORDER)) |>
    dplyr::arrange(.data$index_id) |>
    dplyr::mutate(index_id = as.character(.data$index_id))
}

geography_registry <- function() {
  tibble::tibble(
    source_id = "un_m49",
    source_name = "UN M49 Standard Country or Area Codes",
    edition = "snapshot 2026-08-04",
    source_url = "https://unstats.un.org/unsd/methodology/m49/overview/",
    download_url = "https://unstats.un.org/unsd/methodology/m49/overview/",
    file_name = "un_m49_2026_08_04.html",
    file_type = "html",
    license_notes = "United Nations Statistics Division terms",
    source_notes = "English table used for region and subregion"
  )
}

source_download_headers <- function(index_id) {
  headers <- list(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 Chrome/138 Safari/537.36 humanitarian-intensity-r-pipeline/0.1")
  if (identical(index_id, "nd_gain")) {
    headers$Referer <- "https://gain.nd.edu/our-work/country-index/download-data/"
    headers$`User-Agent` <- "Mozilla/5.0 humanitarian-intensity-r-pipeline/0.1"
  }
  headers
}

COUNTRY_ALIASES <- tibble::tribble(
  ~alias, ~iso3, ~note,
  "Bosnia & Herzegovina", "BIH", "publisher abbreviation",
  "Bosnia and Hercegovina", "BIH", "GHS spelling",
  "Bolivia", "BOL", "short country name",
  "Bolivia (Plurinational State of)", "BOL", "UN long name",
  "Bolivia (Plurinat. State of)", "BOL", "GHI abbreviation",
  "Cabo Verde", "CPV", "current short name",
  "Cape Verde", "CPV", "former English name",
  "Brunei", "BRN", "short country name",
  "Congo", "COG", "MPI short name for Republic of the Congo",
  "Congo (Democratic Republic of the)", "COD", "MPI long name",
  "Congo, Democratic Republic of", "COD", "IMF name",
  "Congo, Republic of", "COG", "IMF name",
  "Congo Republic", "COG", "publisher short name",
  "Congo (Brazzaville)", "COG", "GHS name",
  "Congo (Democratic Republic)", "COD", "GHS name",
  "Cook Islands", "COK", "valid ISO3 outside selected master",
  "DR Congo", "COD", "publisher short name",
  "Democratic Republic of Congo", "COD", "publisher short name",
  "Democratic Republic of the Congo", "COD", "UN long name",
  "Dem. Rep. of the Congo", "COD", "GHI abbreviation",
  "Cote d Ivoire", "CIV", "ASCII spelling",
  "Côte D'Ivoire", "CIV", "publisher capitalization",
  "Côte d’Ivoire", "CIV", "curly apostrophe spelling",
  "Czech Republic", "CZE", "former short name",
  "Eswatini (Kingdom of)", "SWZ", "UNDP long name",
  "Gambia, The", "GMB", "IMF name",
  "Hong Kong, China (SAR)", "HKG", "UNDP territory name",
  "Iran (Islamic Republic of)", "IRN", "UN long name",
  "Iran", "IRN", "short country name",
  "Korea, Democratic People's Republic of", "PRK", "publisher long name",
  "Korea (DPR)", "PRK", "GHI abbreviation",
  "Korea, Republic of", "KOR", "publisher long name",
  "Kyrgyz Republic", "KGZ", "IMF name",
  "Lao P.D.R.", "LAO", "IMF name",
  "Lao PDR", "LAO", "publisher abbreviation",
  "Laos", "LAO", "common short name",
  "Micronesia", "FSM", "IMF short name",
  "Micronesia (Federated States of)", "FSM", "UN long name",
  "Moldova", "MDA", "short country name",
  "Moldova (Rep. of)", "MDA", "GHI abbreviation",
  "North Korea", "PRK", "common short name",
  "Niue", "NIU", "valid ISO3 outside selected master",
  "Palestine", "PSE", "publisher short name",
  "Palestine, State of", "PSE", "UNDP country name",
  "Republic of Korea", "KOR", "publisher long name",
  "Republic of Moldova", "MDA", "UN long name",
  "Rohingya JRP", "BGD", "Bangladesh country response",
  "Russia", "RUS", "short country name",
  "Russian Federation", "RUS", "UN long name",
  "Sao Tome and Principe", "STP", "ASCII spelling",
  "São Tomé and Príncipe", "STP", "accented spelling",
  "Slovak Republic", "SVK", "former short name",
  "South Korea", "KOR", "common short name",
  "State of Palestine", "PSE", "UN name",
  "Libya", "LBY", "current short name",
  "St. Kitts and Nevis", "KNA", "IMF abbreviation",
  "St. Lucia", "LCA", "IMF abbreviation",
  "St. Vincent and the Grenadines", "VCT", "IMF abbreviation",
  "Swaziland", "SWZ", "former country name",
  "Syria", "SYR", "short country name",
  "Syrian Arab Republic", "SYR", "UN long name",
  "Tanzania", "TZA", "short country name",
  "Tanzania (United Republic of)", "TZA", "publisher long name",
  "Tanzania (United Rep. of)", "TZA", "GHI abbreviation",
  "Timor Leste", "TLS", "publisher spacing",
  "Turkey", "TUR", "former English short name",
  "Türkiye", "TUR", "current short name",
  "United Republic of Tanzania", "TZA", "UN long name",
  "United Kingdom", "GBR", "short country name",
  "United States", "USA", "short country name",
  "United States of America", "USA", "UN long name",
  "Venezuela", "VEN", "short country name",
  "Venezuela (Bolivarian Republic of)", "VEN", "UN long name",
  "Venezuela (Boliv. Rep. of)", "VEN", "GHI abbreviation",
  "Vietnam", "VNM", "common spelling",
  "Viet Nam", "VNM", "UN spelling",
  "Yemen, Republic of", "YEM", "IMF name"
)
