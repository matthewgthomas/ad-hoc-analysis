#!/usr/bin/env Rscript

# Reconstruct the English Indices of Deprivation 2025 at 2021 LSOA level
# using COINr. See README-COINr.md for scope and unavoidable substitutions.

required_packages <- c("COINr", "dplyr", "readxl")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Install required packages before running: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(COINr)
  library(dplyr)
  library(readxl)
})

download_dir <- file.path("tmp", "downloads")
output_dir <- file.path("output", "coinr")
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

source_files <- list(
  indicators = list(
    path = file.path(download_dir, "File_8_IoD2025_Underlying_Indicators.xlsx"),
    url = paste0(
      "https://assets.publishing.service.gov.uk/media/691dec012c6b98ecdbc500d4/",
      "File_8_IoD2025_Underlying_Indicators.xlsx"
    )
  ),
  all_results = list(
    path = file.path(
      download_dir,
      "File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv"
    ),
    url = paste0(
      "https://assets.publishing.service.gov.uk/media/691ded56d140bbbaa59a2a7d/",
      "File_7_IoD2025_All_Ranks_Scores_Deciles_Population_Denominators.csv"
    )
  ),
  transformed_domains = list(
    path = file.path(
      download_dir,
      "File_9_IoD2025_Transformed_Domain_Scores.xlsx"
    ),
    url = paste0(
      "https://assets.publishing.service.gov.uk/media/691ded670dcbf6343e9a2a6c/",
      "File_9_IoD2025_Transformed_Domain_Scores.xlsx"
    )
  )
)

ensure_download <- function(source) {
  if (!file.exists(source$path)) {
    message("Downloading ", basename(source$path))
    download.file(source$url, source$path, mode = "wb", quiet = TRUE)
  }
  source$path
}

paths <- lapply(source_files, ensure_download)

# These functions implement Technical Report sections 3.5, 3.6 and Appendix E.
# COINr reverses negative-direction indicators before calling the normaliser.
imd_rank_normal <- function(x) {
  out <- rep(NA_real_, length(x))
  observed <- !is.na(x)
  n <- sum(observed)
  out[observed] <- qnorm(
    (rank(x[observed], ties.method = "average") - 0.5) / n
  )
  out
}

imd_rank_exponential <- function(x, constant = 23) {
  out <- rep(NA_real_, length(x))
  observed <- !is.na(x)
  n <- sum(observed)
  rank_fraction <- rank(x[observed], ties.method = "average") / n
  out[observed] <- -constant * log(
    1 - rank_fraction * (1 - exp(-100 / constant))
  )
  out
}

# Unlike COINr::a_amean(), this deliberately does not rescale weights to sum to
# one. The published domain weights sum to 0.999 after rounding, and MHCLG used
# those displayed weights directly in the released IMD score.
imd_weighted_sum <- function(x, w) {
  if (anyNA(x)) return(NA_real_)
  sum(x * w)
}

make_coin <- function(
    iData,
    indicator_meta,
    aggregate_code,
    aggregate_name,
    normaliser = NULL) {
  stopifnot(
    identical(names(iData)[1], "uCode"),
    identical(names(iData)[-1], indicator_meta$iCode)
  )

  iMeta <- bind_rows(
    indicator_meta |>
      transmute(
        Level = 1L,
        iCode,
        iName,
        Parent = aggregate_code,
        Direction,
        Weight,
        Type = "Indicator"
      ),
    tibble(
      Level = 2L,
      iCode = aggregate_code,
      iName = aggregate_name,
      Parent = NA_character_,
      Direction = 1,
      Weight = 1,
      Type = "Aggregate"
    )
  )

  coin <- new_coin(
    iData = iData,
    iMeta = iMeta,
    level_names = c("Indicator", "Aggregate"),
    quietly = TRUE
  )

  dset <- "Raw"
  if (!is.null(normaliser)) {
    coin <- suppressMessages(Normalise(
      coin,
      dset = "Raw",
      global_specs = list(f_n = normaliser)
    ))
    dset <- "Normalised"
  }

  coin <- suppressMessages(Aggregate(
    coin,
    dset = dset,
    f_ag = "imd_weighted_sum"
  ))

  list(
    coin = coin,
    score = get_dset(coin, "Aggregated")[[aggregate_code]]
  )
}

meta <- function(iCode, iName, Direction = 1, Weight = 1) {
  tibble(iCode, iName, Direction, Weight)
}

read_sheet <- function(sheet) {
  read_excel(paths$indicators, sheet = sheet) |>
    arrange(`LSOA code (2021)`)
}

official <- read.csv(paths$all_results, check.names = FALSE) |>
  arrange(`LSOA code (2021)`)

uCode <- official$`LSOA code (2021)`
n_lsoa <- length(uCode)

assert_codes <- function(data, label) {
  if (!identical(data$`LSOA code (2021)`, uCode)) {
    stop(label, " LSOA codes do not align with File 7.", call. = FALSE)
  }
  invisible(data)
}

education <- assert_codes(read_sheet("IoD25 Education Domain"), "Education")
health <- assert_codes(read_sheet("IoD25 Health Domain"), "Health")
crime <- assert_codes(read_sheet("IoD25 Crime Domain"), "Crime")
barriers <- assert_codes(read_sheet("IoD25 Barriers Domain"), "Barriers")
living <- assert_codes(read_sheet("IoD25 Living Env Domain"), "Living Environment")

# Education: four pupil indicators are not published. File 7's published
# Children and Young People sub-domain score is therefore the narrowest valid
# substitution. The Adult Skills indicator is reconstructed from File 8.
education_coin <- make_coin(
  data.frame(
    uCode,
    children_young_people = official$`Children and Young People Sub-domain Score`,
    adult_skills = education$`Adult skills and English language proficiency indicator`
  ),
  meta(
    c("children_young_people", "adult_skills"),
    c("Children and Young People sub-domain", "Adult Skills sub-domain"),
    Weight = c(0.5, 0.5)
  ),
  "education",
  "Education, Skills and Training",
  "imd_rank_exponential"
)

health_coin <- make_coin(
  data.frame(
    uCode,
    acute_morbidity = health$`Acute morbidity indicator`,
    illness_disability = health$`Comparative illness and disability ratio indicator`,
    mental_health = health$`Mental health indicator`,
    years_life_lost = health$`Years of potential life lost indicator`
  ),
  meta(
    c("acute_morbidity", "illness_disability", "mental_health", "years_life_lost"),
    c(
      "Acute morbidity",
      "Comparative illness and disability ratio",
      "Mental health",
      "Years of potential life lost"
    ),
    Weight = c(0.222, 0.294, 0.244, 0.240)
  ),
  "health",
  "Health Deprivation and Disability",
  "imd_rank_normal"
)

crime_coin <- make_coin(
  data.frame(
    uCode,
    violence_injury = crime$`Violence with injury, rate per 1,000 'at-risk population'`,
    violence_no_injury = crime$`Violence without injury, rate per 1,000 'at-risk population'`,
    stalking_harassment = crime$`Stalking and harassment, rate per 1,000 'at-risk population'`,
    burglary = crime$`Burglary, rate per 1,000 'at-risk properties'`,
    theft = crime$`Theft, rate per 1,000 'at-risk population'`,
    criminal_damage = crime$`Criminal damage, rate per 1,000 'at-risk population'`,
    public_order_weapons = crime$`Public order and Possession of weapons, rate per 1,000 'at-risk population'`,
    antisocial_behaviour = crime$`Anti-Social Behaviour, rate per 1,000 'at-risk population'`
  ),
  meta(
    c(
      "violence_injury", "violence_no_injury", "stalking_harassment",
      "burglary", "theft", "criminal_damage", "public_order_weapons",
      "antisocial_behaviour"
    ),
    c(
      "Violence with injury", "Violence without injury", "Stalking and harassment",
      "Burglary", "Theft", "Criminal damage", "Public order and weapons",
      "Anti-social behaviour"
    ),
    Weight = c(0.151, 0.154, 0.132, 0.074, 0.097, 0.144, 0.145, 0.103)
  ),
  "crime",
  "Crime",
  "imd_rank_normal"
)

# Barriers: form the two composite indicators first, then the Wider Barriers
# sub-domain, then combine exponentially transformed sub-domains.
overcrowding_coin <- make_coin(
  data.frame(
    uCode,
    rooms = barriers$`Household overcrowding indicator (rooms)`,
    bedrooms = barriers$`Household overcrowding indicator (bedrooms)`
  ),
  meta(
    c("rooms", "bedrooms"),
    c("Household overcrowding: rooms", "Household overcrowding: bedrooms"),
    Weight = c(0.5, 0.5)
  ),
  "overcrowding",
  "Household overcrowding",
  "imd_rank_normal"
)

homelessness_coin <- make_coin(
  data.frame(
    uCode,
    core_homelessness = barriers$`Core homelessness rate (% of households)`,
    statutory_homelessness = barriers$`Homelessness indicator (rate per 1,000 households)`
  ),
  meta(
    c("core_homelessness", "statutory_homelessness"),
    c("Core homelessness", "Statutory homelessness"),
    Weight = c(0.5, 0.5)
  ),
  "homelessness",
  "Homelessness",
  "imd_rank_normal"
)

wider_barriers_coin <- make_coin(
  data.frame(
    uCode,
    housing_affordability = barriers$`Housing affordability indicator`,
    digital_connectivity = barriers$`Digital Connectivity (Broadband download and upload speeds)`,
    patient_gp_ratio = barriers$`Patient-to-GP Ratio`,
    overcrowding = overcrowding_coin$score,
    homelessness = homelessness_coin$score
  ),
  meta(
    c(
      "housing_affordability", "digital_connectivity", "patient_gp_ratio",
      "overcrowding", "homelessness"
    ),
    c(
      "Housing affordability", "Digital connectivity deprivation",
      "Patient-to-GP ratio", "Household overcrowding", "Homelessness"
    ),
    Weight = rep(0.2, 5)
  ),
  "wider_barriers",
  "Wider Barriers",
  "imd_rank_normal"
)

barriers_coin <- make_coin(
  data.frame(
    uCode,
    geographical_barriers = barriers$`Connectivity Score`,
    wider_barriers = wider_barriers_coin$score
  ),
  meta(
    c("geographical_barriers", "wider_barriers"),
    c("Geographical Barriers", "Wider Barriers"),
    Direction = c(-1, 1),
    Weight = c(0.5, 0.5)
  ),
  "barriers",
  "Barriers to Housing and Services",
  "imd_rank_exponential"
)

indoors_coin <- make_coin(
  data.frame(
    uCode,
    housing_condition = living$`Housing in poor condition indicator`,
    energy_performance = living$`Housing energy performance deprivation Score`,
    private_outdoor_space = living$`Housing lacking private outdoor space deprivation score`
  ),
  meta(
    c("housing_condition", "energy_performance", "private_outdoor_space"),
    c(
      "Housing in poor condition", "Housing energy performance deprivation",
      "Housing lacking private outdoor space"
    ),
    Weight = rep(1 / 3, 3)
  ),
  "indoors",
  "Indoors Living Environment",
  "imd_rank_normal"
)

outdoors_coin <- make_coin(
  data.frame(
    uCode,
    noise = living$`Noise pollution`,
    road_casualties = living$`Road traffic casualties involving injury to pedestrians and cyclists`,
    air_quality = living$`Air quality indicator`
  ),
  meta(
    c("noise", "road_casualties", "air_quality"),
    c("Noise pollution", "Road casualties", "Air quality"),
    Weight = rep(1 / 3, 3)
  ),
  "outdoors",
  "Outdoors Living Environment",
  "imd_rank_normal"
)

living_coin <- make_coin(
  data.frame(
    uCode,
    indoors = indoors_coin$score,
    outdoors = outdoors_coin$score
  ),
  meta(
    c("indoors", "outdoors"),
    c("Indoors Living Environment", "Outdoors Living Environment"),
    Weight = c(0.7, 0.3)
  ),
  "living_environment",
  "Living Environment",
  "imd_rank_exponential"
)

domain_weights <- c(0.225, 0.225, 0.135, 0.135, 0.093, 0.093, 0.093)
domain_codes <- c(
  "income", "employment", "education", "health", "crime", "barriers",
  "living_environment"
)
domain_names <- c(
  "Income", "Employment", "Education, Skills and Training",
  "Health Deprivation and Disability", "Crime",
  "Barriers to Housing and Services", "Living Environment"
)

official_domain_data <- data.frame(
  uCode,
  income = official$`Income Score (rate)`,
  employment = official$`Employment Score (rate)`,
  education = official$`Education, Skills and Training Score`,
  health = official$`Health Deprivation and Disability Score`,
  crime = official$`Crime Score`,
  barriers = official$`Barriers to Housing and Services Score`,
  living_environment = official$`Living Environment Score`
)

reconstructed_domain_data <- official_domain_data
reconstructed_domain_data$education <- education_coin$score
reconstructed_domain_data$health <- health_coin$score
reconstructed_domain_data$crime <- crime_coin$score
reconstructed_domain_data$barriers <- barriers_coin$score
reconstructed_domain_data$living_environment <- living_coin$score

final_meta <- meta(domain_codes, domain_names, Weight = domain_weights)

reconstructed_imd_coin <- make_coin(
  reconstructed_domain_data,
  final_meta,
  "imd",
  "Index of Multiple Deprivation",
  "imd_rank_exponential"
)

raw_domain_benchmark_coin <- make_coin(
  official_domain_data,
  final_meta,
  "imd",
  "Index of Multiple Deprivation",
  "imd_rank_exponential"
)

transformed <- read_excel(
  paths$transformed_domains,
  sheet = "IoD25 Transformed Domain Scores"
) |>
  transmute(
    uCode = `LSOA code (2021)`,
    income = `Income Score - exponentially transformed`,
    employment = `Employment Score - exponentially transformed`,
    education = `Education Score - exponentially transformed`,
    health = `Health Score - exponentially transformed`,
    crime = `Crime Score - exponentially transformed`,
    barriers = `Barriers Score - exponentially transformed`,
    living_environment = `Living Environment Score - exponentially transformed`
  ) |>
  arrange(uCode)

if (!identical(transformed$uCode, uCode)) {
  stop("File 9 LSOA codes do not align with File 7.", call. = FALSE)
}

transformed_benchmark_coin <- make_coin(
  transformed,
  final_meta,
  "imd",
  "Index of Multiple Deprivation"
)

rank_desc <- function(x) as.integer(rank(-x, ties.method = "first"))
rank_to_decile <- function(x) pmin(10L, as.integer(ceiling(x * 10 / n_lsoa)))

score_metrics <- function(model, score) {
  model_rank <- rank_desc(score)
  official_score <- official$`Index of Multiple Deprivation (IMD) Score`
  official_rank <- official$`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`
  official_decile <- official$`Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`

  tibble(
    model,
    score_pearson = cor(score, official_score),
    score_spearman = cor(score, official_score, method = "spearman"),
    mean_absolute_score_difference = mean(abs(score - official_score)),
    maximum_absolute_score_difference = max(abs(score - official_score)),
    rank_spearman = cor(model_rank, official_rank, method = "spearman"),
    exact_rank_match = mean(model_rank == official_rank),
    mean_absolute_rank_difference = mean(abs(model_rank - official_rank)),
    maximum_absolute_rank_difference = max(abs(model_rank - official_rank)),
    decile_match = mean(rank_to_decile(model_rank) == official_decile)
  )
}

imd_validation <- bind_rows(
  score_metrics("Published-indicator reconstruction", reconstructed_imd_coin$score),
  score_metrics("Published raw-domain benchmark", raw_domain_benchmark_coin$score),
  score_metrics("Published transformed-domain benchmark", transformed_benchmark_coin$score)
)

official_domain_matrix <- official_domain_data[-1]
reconstructed_domain_matrix <- reconstructed_domain_data[-1]

domain_validation <- bind_rows(lapply(seq_along(domain_codes), function(j) {
  reconstructed_score <- reconstructed_domain_matrix[[j]]
  official_score <- official_domain_matrix[[j]]
  substitution <- domain_codes[j] %in% c("income", "employment")
  tibble(
    domain = domain_names[j],
    treatment = if (substitution) {
      "Published domain score substituted (unreleased shrinkage inputs)"
    } else if (domain_codes[j] == "education") {
      "File 8 plus published Children and Young People sub-domain score"
    } else {
      "Reconstructed from File 8 indicators"
    },
    score_pearson = cor(reconstructed_score, official_score),
    rank_spearman = cor(reconstructed_score, official_score, method = "spearman"),
    mean_absolute_score_difference = mean(abs(reconstructed_score - official_score)),
    maximum_absolute_score_difference = max(abs(reconstructed_score - official_score))
  )
}))

model_results <- function(prefix, score) {
  model_rank <- rank_desc(score)
  setNames(
    data.frame(score, rank = model_rank, decile = rank_to_decile(model_rank)),
    paste0(prefix, c("_score", "_rank", "_decile"))
  )
}

results <- bind_cols(
  official |>
    transmute(
      lsoa21_code = `LSOA code (2021)`,
      lsoa21_name = `LSOA name (2021)`,
      lad24_code = `Local Authority District code (2024)`,
      lad24_name = `Local Authority District name (2024)`,
      official_imd_score = `Index of Multiple Deprivation (IMD) Score`,
      official_imd_rank = `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived)`,
      official_imd_decile = `Index of Multiple Deprivation (IMD) Decile (where 1 is most deprived 10% of LSOAs)`
    ),
  model_results("reconstructed_imd", reconstructed_imd_coin$score),
  model_results("raw_domain_benchmark", raw_domain_benchmark_coin$score),
  model_results("transformed_domain_benchmark", transformed_benchmark_coin$score),
  reconstructed_domain_matrix |>
    rename_with(~ paste0("reconstructed_", .x)),
  official_domain_matrix |>
    rename_with(~ paste0("official_", .x))
)

substitutions <- tibble(
  component = c(
    "Income Deprivation Domain",
    "Employment Deprivation Domain",
    "Children and Young People education sub-domain"
  ),
  reason = c(
    "The released numerator is insufficient to reproduce shrinkage; some numerator values are also suppressed.",
    "The released numerator is insufficient to reproduce shrinkage; some numerator values are also suppressed.",
    "Four pupil-derived indicators are not published because they derive from pupil microdata."
  ),
  substitution = c(
    "Published Income Score (rate) from File 7",
    "Published Employment Score (rate) from File 7",
    "Published Children and Young People sub-domain score from File 7"
  )
)

# Fail loudly if an upstream workbook changes shape or the reconstruction drifts
# materially from the published results.
stopifnot(
  nrow(results) == 33755L,
  !anyDuplicated(results$lsoa21_code),
  !anyNA(results$reconstructed_imd_score),
  all(results$reconstructed_imd_decile %in% 1:10),
  imd_validation$rank_spearman[
    imd_validation$model == "Published-indicator reconstruction"
  ] > 0.9999,
  imd_validation$decile_match[
    imd_validation$model == "Published-indicator reconstruction"
  ] > 0.99,
  imd_validation$maximum_absolute_rank_difference[
    imd_validation$model == "Published transformed-domain benchmark"
  ] <= 3
)

write.csv(
  results,
  file.path(output_dir, "imd2025_coinr_results.csv"),
  row.names = FALSE
)
write.csv(
  domain_validation,
  file.path(output_dir, "domain_validation.csv"),
  row.names = FALSE
)
write.csv(
  imd_validation,
  file.path(output_dir, "imd_validation.csv"),
  row.names = FALSE
)
write.csv(
  substitutions,
  file.path(output_dir, "method_substitutions.csv"),
  row.names = FALSE
)

if (identical(Sys.getenv("IMD_SAVE_COINS"), "1")) {
  saveRDS(
    list(
      reconstructed_imd = reconstructed_imd_coin$coin,
      raw_domain_benchmark = raw_domain_benchmark_coin$coin,
      transformed_domain_benchmark = transformed_benchmark_coin$coin
    ),
    file.path(output_dir, "imd2025_coinr_objects.rds")
  )
}

message("COINr reconstruction complete: ", normalizePath(output_dir))
print(imd_validation)
