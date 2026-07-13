nhanes_manifest <- function() {
  tidyr::expand_grid(
    cycle = c("2011–2012", "2013–2014"),
    component = c("MGX", "DEMO", "BMX", "PAQ")
  ) |>
    dplyr::mutate(
      suffix = dplyr::if_else(cycle == "2011–2012", "G", "H"),
      year_path = dplyr::if_else(cycle == "2011–2012", "2011", "2013"),
      file = paste0(component, "_", suffix, ".xpt"),
      url = paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/", year_path,
                   "/DataFiles/", file)
    )
}

download_nhanes <- function(cache_dir = "data/raw", overwrite = FALSE) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- nhanes_manifest()
  for (i in seq_len(nrow(manifest))) {
    dest <- file.path(cache_dir, manifest$file[[i]])
    if (overwrite || !file.exists(dest)) {
      utils::download.file(manifest$url[[i]], destfile = dest, mode = "wb", quiet = FALSE)
    }
  }
  manifest$path <- file.path(cache_dir, manifest$file)
  manifest
}

assert_unique_id <- function(x, label) {
  if (anyNA(x$SEQN)) stop(label, " contains missing SEQN.", call. = FALSE)
  if (anyDuplicated(x$SEQN)) stop(label, " contains duplicated SEQN.", call. = FALSE)
  invisible(TRUE)
}

activity_component <- function(yes_no, days, minutes, met) {
  dplyr::case_when(
    yes_no == 2 ~ 0,
    yes_no == 1 & dplyr::between(days, 1, 7) & dplyr::between(minutes, 1, 1440) ~ days * minutes * met,
    TRUE ~ NA_real_
  )
}

derive_activity <- function(x) {
  components <- data.frame(
    vigorous_work = activity_component(x$PAQ605, x$PAQ610, x$PAD615, 8),
    moderate_work = activity_component(x$PAQ620, x$PAQ625, x$PAD630, 4),
    transport = activity_component(x$PAQ635, x$PAQ640, x$PAD645, 4),
    vigorous_recreation = activity_component(x$PAQ650, x$PAQ655, x$PAD660, 8),
    moderate_recreation = activity_component(x$PAQ665, x$PAQ670, x$PAD675, 4)
  )
  total <- rowSums(components)
  total[rowSums(is.na(components)) > 0] <- NA_real_
  total
}

max_effort_hand <- function(t1, e1, t2, e2, t3, e3) {
  row_max_na(ifelse(e1 == 1, t1, NA_real_), ifelse(e2 == 1, t2, NA_real_),
             ifelse(e3 == 1, t3, NA_real_))
}

derive_grip <- function(x) {
  hand1_raw <- row_max_na(x$MGXH1T1, x$MGXH1T2, x$MGXH1T3)
  hand2_raw <- row_max_na(x$MGXH2T1, x$MGXH2T2, x$MGXH2T3)
  hand1_effort <- max_effort_hand(x$MGXH1T1, x$MGXH1T1E, x$MGXH1T2, x$MGXH1T2E,
                                  x$MGXH1T3, x$MGXH1T3E)
  hand2_effort <- max_effort_hand(x$MGXH2T1, x$MGXH2T1E, x$MGXH2T2, x$MGXH2T2E,
                                  x$MGXH2T3, x$MGXH2T3E)
  right_raw <- ifelse(x$MGATHAND == 1, hand1_raw, hand2_raw)
  left_raw <- ifelse(x$MGATHAND == 1, hand2_raw, hand1_raw)
  right_effort <- ifelse(x$MGATHAND == 1, hand1_effort, hand2_effort)
  left_effort <- ifelse(x$MGATHAND == 1, hand2_effort, hand1_effort)
  tibble::tibble(
    hand1_raw = hand1_raw,
    hand2_raw = hand2_raw,
    hand1_max_effort = hand1_effort,
    hand2_max_effort = hand2_effort,
    right_raw = right_raw,
    left_raw = left_raw,
    right_max_effort = right_effort,
    left_max_effort = left_effort,
    best_single_grip = row_max_na(right_effort, left_effort),
    bilateral_grip = ifelse(is.finite(right_effort) & is.finite(left_effort),
                            right_effort + left_effort, NA_real_),
    raw_bilateral_grip = ifelse(is.finite(right_raw) & is.finite(left_raw),
                                right_raw + left_raw, NA_real_)
  )
}

read_cycle <- function(cycle, suffix, raw_dir) {
  paths <- setNames(file.path(raw_dir, paste0(c("MGX", "DEMO", "BMX", "PAQ"), "_", suffix, ".xpt")),
                    c("MGX", "DEMO", "BMX", "PAQ"))
  missing_files <- paths[!file.exists(paths)]
  if (length(missing_files)) stop("Missing source file(s): ", paste(basename(missing_files), collapse = ", "), call. = FALSE)
  parts <- lapply(paths, haven::read_xpt)
  for (nm in names(parts)) assert_unique_id(parts[[nm]], paste(cycle, nm))
  start_n <- nrow(parts$MGX)
  joined <- parts$MGX |>
    dplyr::left_join(parts$DEMO, by = "SEQN", relationship = "one-to-one") |>
    dplyr::left_join(parts$BMX, by = "SEQN", relationship = "one-to-one") |>
    dplyr::left_join(parts$PAQ, by = "SEQN", relationship = "one-to-one")
  if (nrow(joined) != start_n) stop("Join changed MGX row count for ", cycle, ".", call. = FALSE)
  grip <- derive_grip(joined)
  joined <- dplyr::bind_cols(joined, grip)
  joined$cycle <- cycle
  attr(joined, "source_counts") <- tibble::tibble(
    cycle = cycle, component = names(parts), rows = vapply(parts, nrow, integer(1)),
    unique_seqn = vapply(parts, function(z) dplyr::n_distinct(z$SEQN), integer(1))
  )
  joined
}

prepare_nhanes <- function(cache_dir = "data/raw",
                           output_path = "data/nhanes_grip_adults.rds",
                           qa_path = "artifacts/data_quality.rds",
                           download = TRUE) {
  if (download) download_nhanes(cache_dir)
  g <- read_cycle("2011–2012", "G", cache_dir)
  h <- read_cycle("2013–2014", "H", cache_dir)
  source_counts <- dplyr::bind_rows(attr(g, "source_counts"), attr(h, "source_counts"))
  x <- dplyr::bind_rows(g, h)
  effort_values <- unlist(x[c("MGXH1T1E", "MGXH1T2E", "MGXH1T3E", "MGXH2T1E", "MGXH2T2E", "MGXH2T3E")], use.names = FALSE)
  trial_values <- unlist(x[c("MGXH1T1", "MGXH1T2", "MGXH1T3", "MGXH2T1", "MGXH2T2", "MGXH2T3")], use.names = FALSE)
  invalid_codes <- tibble::tibble(
    variable = c("RIAGENDR", "MGATHAND", "MGD130", "MGDSEAT", "effort flags", "trial values"),
    invalid_n = c(
      sum(!is.na(x$RIAGENDR) & !x$RIAGENDR %in% c(1, 2)),
      sum(!is.na(x$MGATHAND) & !x$MGATHAND %in% c(1, 2)),
      sum(!is.na(x$MGD130) & !x$MGD130 %in% c(1, 2, 3)),
      sum(!is.na(x$MGDSEAT) & !x$MGDSEAT %in% c(1, 2)),
      sum(!is.na(effort_values) & !effort_values %in% c(1, 2)),
      sum(!is.na(trial_values) & (trial_values < 0 | trial_values > 200))
    )
  )
  x$activity_met_min_week <- derive_activity(x)
  x <- x |>
    dplyr::transmute(
      SEQN, cycle,
      age = as.numeric(RIDAGEYR),
      age_band = age_band_value(age),
      sex = factor(dplyr::recode(RIAGENDR, `1` = "Male", `2` = "Female"), levels = c("Female", "Male")),
      race_ethnicity = factor(dplyr::recode(RIDRETH3,
        `1` = "Mexican American", `2` = "Other Hispanic", `3` = "Non-Hispanic White",
        `4` = "Non-Hispanic Black", `6` = "Non-Hispanic Asian", `7` = "Other / multiracial"
      )),
      height_cm = as.numeric(BMXHT), bmi = as.numeric(BMXBMI),
      arm_length_cm = as.numeric(BMXARML), arm_circumference_cm = as.numeric(BMXARMC),
      activity_met_min_week = as.numeric(activity_met_min_week),
      log_activity = log1p(activity_met_min_week),
      handedness = factor(dplyr::recode(MGD130, `1` = "Right", `2` = "Left", `3` = "Ambidextrous"),
                          levels = c("Right", "Left", "Ambidextrous")),
      any_hand_pain = factor(dplyr::case_when(MGQ070 == 1 | MGQ100 == 1 ~ "Yes",
                                              MGQ070 == 2 & MGQ100 == 2 ~ "No",
                                              TRUE ~ NA_character_), levels = c("No", "Yes")),
      prior_surgery = factor(dplyr::case_when(MGD050 == 1 ~ "Yes", MGD050 == 2 ~ "No",
                                              MGD050 %in% c(7, 9) ~ NA_character_,
                                              is.na(MGD050) & RIDAGEYR < 20 ~ "Not asked",
                                              TRUE ~ NA_character_),
                             levels = c("No", "Yes", "Not asked")),
      posture = factor(dplyr::recode(MGDSEAT, `1` = "Standing", `2` = "Seated"),
                       levels = c("Standing", "Seated")),
      test_status = MGDEXSTS, start_hand = MGATHAND,
      hand1_raw, hand2_raw, hand1_max_effort, hand2_max_effort,
      right_raw, left_raw, right_max_effort, left_max_effort,
      best_single_grip, bilateral_grip, raw_bilateral_grip,
      official_bilateral_grip = as.numeric(MGDCGSZ),
      WTMEC2YR = as.numeric(WTMEC2YR), MEC4YR = as.numeric(WTMEC2YR) / 2,
      SDMVPSU = as.numeric(SDMVPSU), SDMVSTRA = as.numeric(SDMVSTRA),
      cluster_id = interaction(cycle, SDMVSTRA, SDMVPSU, drop = TRUE)
    ) |>
    dplyr::filter(dplyr::between(age, 18, 80)) |>
    dplyr::mutate(
      valid_weight = is.finite(MEC4YR) & MEC4YR > 0 & !is.na(SDMVPSU) & !is.na(SDMVSTRA),
      distribution_eligible = valid_weight & test_status == 1 & is.finite(best_single_grip),
      core_eligible = distribution_eligible & is.finite(age) & !is.na(sex) &
        is.finite(height_cm) & is.finite(bmi),
      extended_eligible = core_eligible & is.finite(arm_length_cm) &
        is.finite(arm_circumference_cm) & is.finite(log_activity) &
        !is.na(handedness) & !is.na(any_hand_pain) & !is.na(prior_surgery) & !is.na(posture),
      sensitivity_eligible = extended_eligible & posture == "Standing" &
        any_hand_pain == "No" & prior_surgery %in% c("No", "Not asked"),
      raw_reconciliation_difference = raw_bilateral_grip - official_bilateral_grip
    )
  assert_unique_id(x, "Prepared adult cohort")
  qa <- list(
    source_counts = source_counts,
    invalid_codes = invalid_codes,
    adult_rows = nrow(x),
    adult_unique_seqn = dplyr::n_distinct(x$SEQN),
    eligibility = x |>
      dplyr::summarise(
        adults = dplyr::n(), valid_weight = sum(valid_weight),
        distribution = sum(distribution_eligible), core = sum(core_eligible),
        extended = sum(extended_eligible), sensitivity = sum(sensitivity_eligible)
      ),
    missingness = x |>
      dplyr::summarise(dplyr::across(c(best_single_grip, bilateral_grip, age, sex, height_cm, bmi,
                                              arm_length_cm, arm_circumference_cm, activity_met_min_week,
                                              handedness, any_hand_pain, prior_surgery, posture),
                                      ~ mean(is.na(.x)))) |>
      tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "missing_fraction"),
    reconciliation = x |>
      dplyr::filter(is.finite(raw_bilateral_grip), is.finite(official_bilateral_grip)) |>
      dplyr::summarise(n = dplyr::n(), exact = sum(abs(raw_reconciliation_difference) < 1e-8),
                       discordant = sum(abs(raw_reconciliation_difference) >= 1e-8),
                       max_abs_difference = max(abs(raw_reconciliation_difference)))
  )
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(qa_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, output_path)
  saveRDS(qa, qa_path)
  list(data = x, qa = qa, sources = nhanes_manifest())
}
