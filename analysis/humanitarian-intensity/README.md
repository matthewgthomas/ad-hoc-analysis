# Humanitarian intensity composite-index dataset

This project builds a 195-country dataset from 16 published vulnerability,
fragility, development, hunger, health, gender, debt, safeguarding and
displacement indices. Publisher scores remain on their original scales. The
pipeline standardises only the rank and decile direction so that rank 1 and
decile 1 always mean **most vulnerable**.

## Run

```sh
Rscript -e 'renv::restore()'
Rscript run_pipeline.R
Rscript tests/testthat.R
```

Use `Rscript run_pipeline.R --refresh` to re-download the pinned source URLs.
Without `--refresh`, existing raw snapshots are reused. Downloads are saved in
`data/raw/`, and every file is recorded with a SHA-256 checksum in the source
manifest.

The main outputs are:

- `data/processed/humanitarian_indices_country.csv` and `.rds`: one row per UN
  member or observer state, with score/rank/decile columns for every index.
- `data/processed/humanitarian_indices_long.csv` and `.rds`: the complete
  country-index audit table.
- `artifacts/source_manifest.csv`: editions, URLs, hashes, coverage and source
  notes.
- `artifacts/coverage_report.csv`, `data_quality_checks.csv`,
  `country_crosswalk_report.csv`, `unmatched_entities_report.csv`, and
  `exclusions_report.csv`: quality-assurance evidence.

## Manual IDMC Global Displacement Risk Model input

IDMC's GDRM 2.0 results are published through an interactive Power BI
dashboard rather than a stable downloadable country file. Export the dashboard
with these filters:

- Scenario: `Current`
- Metric: `AAD`
- Hazard: `Multi-hazard` or `All`
- Geography: country total (preferred); otherwise sum administrative-level
  totals after selecting the publisher's multi-hazard result

Populate `data/manual/idmc_gdrm2_country.csv` using the existing columns. The
pipeline validates unique ISO3 codes, non-negative AAD values and the filter
metadata. When the file contains only its header, the output retains the GDRM
columns as `NA` and the source manifest reports
`manual input template; data unavailable`.

## Important interpretation notes

- Rankings and deciles are calculated within each index's published numeric
  coverage, not across all 195 master countries.
- Ties use minimum rank. Deciles are
  `min(10, floor(10 * (rank - 1) / n_scored) + 1)`, so a tied group can make a
  decile contain more than exactly 10% of scored countries.
- `top_10_count` counts decile 1 appearances and `top_20_count` counts deciles
  1-2. `indices_ranked_count` reports the available denominator by country.
- Debt distress is retained as a class and a derived ordinal value but is not
  ranked, placed into deciles, or included in the counts.
- GHI values such as `<5` and provisional ranges are preserved in
  `ghi_score_label`; they do not receive an invented numeric score.
- The Underfunded Crisis Index excludes regional response plans. The Rohingya
  Joint Response Plan is retained as a Bangladesh country response.
- The OECD artifact publicly exposes overall scores for 61 high/extreme
  fragility contexts; other countries remain missing.
- The source check found that the official IASC page now serves **SEARO 2026
  v1.2 (December 2025)** as its latest workbook. This newer official release is
  used instead of the earlier 2025 workbook named in the initial plan.
- No cross-index meta-score or imputation is produced.

## Reproducibility and licensing

The source manifest records publisher links, editions, retrieval snapshot,
file hashes and licensing/attribution notes. Raw files remain the property of
their publishers and should be redistributed only under their applicable
terms. Derived outputs preserve source attribution and do not alter publisher
scores.
