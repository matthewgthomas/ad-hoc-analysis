# English Indices of Deprivation 2025 reconstruction with COINr

This analysis reconstructs the English Index of Multiple Deprivation 2025 for all 33,755 2021 LSOAs using COINr. It implements the published rank-normal transformations, factor-derived weights, sub-domain hierarchy, exponential transformation, and final domain weights.

## Run

From this directory:

```sh
Rscript imd2025-coinr.R
```

Required R packages are `COINr`, `dplyr`, and `readxl`. Missing official files are downloaded from GOV.UK into `tmp/downloads/`.

Set `IMD_SAVE_COINS=1` to additionally save the three final COINr objects as an RDS file. This is off by default because the objects are large.

## What is reconstructed

The script builds separate COINr objects where the official method requires different transformations:

- Education combines the Children and Young People and Adult Skills sub-domains after exponential transformation, with equal weights.
- Health converts four indicators to rank-based normal scores and applies the published factor weights.
- Crime converts eight indicators to rank-based normal scores and applies the published factor weights.
- Barriers constructs overcrowding and homelessness composite indicators, the Wider Barriers sub-domain, and then combines exponentially transformed Geographical and Wider Barriers sub-domains equally.
- Living Environment constructs equal-weight Indoors and Outdoors sub-domains, exponentially transforms them, and combines them with 70% and 30% weights.
- The seven domains are exponentially transformed and combined using weights of 22.5%, 22.5%, 13.5%, 13.5%, 9.3%, 9.3%, and 9.3%.

The three final models serve different validation purposes:

1. `Published-indicator reconstruction` uses the released indicators wherever the published inputs permit reconstruction.
2. `Published raw-domain benchmark` starts from the seven released untransformed domain scores and checks the exponential transformation plus final aggregation.
3. `Published transformed-domain benchmark` starts from File 9 and checks the final weighted aggregation alone.

## Public-data boundary

A completely independent end-to-end replication is not possible from the released inputs:

- Income and Employment require shrinkage inputs that are not all published. Their released numerators also contain 5 and 21 suppressed LSOAs respectively. The reconstruction therefore uses their published domain rates from File 7.
- Four pupil-derived indicators in the Children and Young People education sub-domain are explicitly not published. The reconstruction therefore uses the published sub-domain score from File 7, while reconstructing its combination with the released Adult Skills indicator.

These substitutions are recorded component-by-component in `output/coinr/method_substitutions.csv`. They are not presented as independent replications.

Small residual differences in the reconstructible domains arise from rounding of released indicators and, for composite indicators such as overcrowding, from the fact that intermediate pre-shrinkage values are not released.

## Validation

Results from the current official v2 files are:

| Model | Rank Spearman | Mean absolute rank difference | Maximum rank difference | Exact rank match | Decile match |
|---|---:|---:|---:|---:|---:|
| Published-indicator reconstruction | 0.9999984 | 12.46 | 392 | 4.30% | 99.66% |
| Published raw-domain benchmark | 0.9999990 | 10.49 | 62 | 5.13% | 99.68% |
| Published transformed-domain benchmark | >0.999999999 | 0.10 | 3 | 90.39% | 99.99% |

The indicator reconstruction's overall score correlation is 0.9999991. Exact-rank agreement is deliberately reported alongside rank and decile agreement because exact positions are highly sensitive to rounding and unavailable intermediate values among near-tied LSOAs.

## Outputs

- `output/coinr/imd2025_coinr_results.csv`: official and reconstructed LSOA scores, ranks, deciles, and domain scores.
- `output/coinr/domain_validation.csv`: domain-level score and rank agreement.
- `output/coinr/imd_validation.csv`: final IMD score, rank, and decile agreement for all three models.
- `output/coinr/method_substitutions.csv`: exact public-data limitations and substitutions.

## Sources

- [English indices of deprivation 2025 data files](https://www.gov.uk/government/statistics/english-indices-of-deprivation-2025)
- [English Indices of Deprivation 2025 Technical Report](https://assets.publishing.service.gov.uk/media/68ff59c80f801e57b5bef907/ID_2025_Technical_Report.pdf)
- [COINr documentation](https://bluefoxr.github.io/COINrDoc/)

The official page was updated on 17 November 2025 to correct the LAD lookup for LSOA E01027305. Files 1–9 were reissued without changes to deprivation values. The script uses the current v2 URLs.
