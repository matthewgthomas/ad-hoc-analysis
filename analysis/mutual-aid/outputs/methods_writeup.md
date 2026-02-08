# Methods: Mutual Aid Presence and Net Trust at MSOA Level

## 1) Data sources and linkage
- Mutual aid groups: `data/groups.json`
- Trust outcome: `data/good_neighbours_full_data_by_msoa.xlsx`
- Geography and lookup data: `geographr` (`boundaries_msoa11`, region lookups, rural/urban classification)
- Linkage steps: (a) geocode groups to MSOAs via spatial join, (b) aggregate to MSOA-level exposure, (c) join trust by `msoa11_code == MSOA_code`.

## 2) Exposure and outcome definitions
- Binary exposure: `any_group = n_groups > 0`.
- Intensity exposure: `log_groups = log1p(n_groups)`.
- Outcome: `Net_trust` from the trust spreadsheet.

## 3) Cleaning and exclusions
- Duplicated group IDs are excluded (keep first instance only).
- Missing/invalid coordinates are excluded.
- Primary specification excludes obvious non-UK points via bounding box: lat 49-61, lon -9 to 2.5.
- All exclusions are logged in `outputs/tables/excluded_groups.csv`.

## 4) Model specifications
- M0: `Net_trust ~ any_group`.
- M1: `Net_trust ~ log_groups`.
- M2: `Net_trust ~ any_group + available covariates + region fixed effects`.
- M3: `Net_trust ~ log_groups + available covariates + region fixed effects`.
- Inference: heteroskedasticity-robust HC3 standard errors and 95% confidence intervals.

## 5) Sensitivity checks
- Excluding top 1% of `n_groups` MSOAs.
- Re-estimating with and without bounding-box filtering.
- Comparing binary and intensity exposures.

## 6) Interpretation boundaries
- This is an association analysis, not a causal identification design.
- Region FE and available controls reduce but do not eliminate omitted-variable bias.
- Some requested covariates (deprivation, population density, age, socioeconomic mix, ethnic diversity) are only used when available from `data/msoa_covariates.csv`.
