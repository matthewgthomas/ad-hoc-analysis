# Interpretation of MSOA Trust and Mutual Aid Association Results

## Executive takeaway
Across MSOAs in England, mutual aid group presence is **positively associated** with net trust, and this association is statistically precise in both unadjusted and adjusted models. The estimated association attenuates after adjustment (as expected when adding controls and region fixed effects) but remains clearly positive. Sensitivity checks show stable direction and similar magnitude, including when excluding top-count outliers and when toggling bounding-box filtering. These results should be interpreted as **associational evidence only**, not causal evidence.

## What was analyzed
- **Unit of analysis:** MSOA (England).
- **Outcome:** `Net_trust`.
- **Exposures:**
  - `any_group` (whether an MSOA has at least one mapped mutual aid group)
  - `log_groups` (`log(1 + n_groups)`) as an intensity measure
- **Coverage:** 6,791 MSOAs with non-missing trust; 1,700 of 6,791 MSOAs had at least one group (~25.0%).

## Data quality and linkage interpretation
Quality-control and linkage outputs indicate a mostly consistent pipeline:
- Raw groups: **2,791**
- Retained after cleaning: **2,775**
- Excluded outside UK bounding box: **16**
- Spatially matched groups: **2,570**
- Unmatched groups after spatial join: **205**

The 205 unmatched records are groups with valid coordinates that did not intersect an MSOA polygon in the spatial join (for example, borderline geometry issues or points outside covered boundaries). Importantly, the trust-join integrity check passed (`analysis_rows=6791; trust_nonmissing=6791`), and all acceptance checks passed (join integrity, exposure construction, cleaning correctness, model reproducibility, reporting completeness).

## Main findings
Using the primary scenario (`bbox_on_full`):

- **M0 (unadjusted binary exposure):** `any_groupTRUE = 0.0542` (95% CI: `0.0480` to `0.0605`)
- **M1 (unadjusted intensity):** `log_groups = 0.0580` (95% CI: `0.0508` to `0.0652`)
- **M2 (adjusted binary exposure):** `any_groupTRUE = 0.0319` (95% CI: `0.0259` to `0.0380`)
- **M3 (adjusted intensity):** `log_groups = 0.0344` (95% CI: `0.0275` to `0.0412`)

Interpretation: MSOAs with mapped mutual aid groups tend to have higher net trust than MSOAs without mapped groups, and areas with more groups also tend to show higher trust. The smaller adjusted estimates (M2/M3 vs M0/M1) are consistent with partial confounding being absorbed by included controls and region fixed effects, while leaving a positive residual association.

Model fit also improves materially with adjustment: **M0 R² = 0.0389 (~0.039)** versus **M2 R² = 0.1777 (~0.178)**.

## Robustness and diagnostics
- **Sensitivity stability:**
  - Bounding-box on/off produced identical key exposure estimates in this run.
  - Top-1% trimming preserved direction and similar magnitude (for example, M2 `any_groupTRUE`: 0.0319 baseline vs 0.0319 trimmed; M3 `log_groups`: 0.0344 baseline vs 0.0368 trimmed).
- **Collinearity diagnostic:** max VIF is about **2.46**, which does not indicate severe multicollinearity.
- **Spatial diagnostic:** Moran's I on adjusted-model residuals is about **0.437** with **p=0.005**, indicating residual spatial clustering.

Interpretation of diagnostics: the association is consistent across key checks, but residual spatial dependence suggests model structure is incomplete geographically; non-spatial uncertainty estimates may therefore be somewhat optimistic.

## What this does and does not imply
This analysis supports the statement that **MSOAs with mutual aid groups tend to have higher trust**. It does **not** establish that mutual aid groups are the causal driver of trust differences.

Key limitations:
- Potential omitted confounding remains (for example, pre-existing civic capacity or local institutional factors).
- Reverse causality is plausible (higher-trust places may be more likely to form/maintain groups).
- Spatial autocorrelation remains in residuals, indicating unmodeled geographic structure.

Control coverage in this run was limited: adjusted models include **urban/rural classification + region fixed effects**. Requested covariates not yet available in the run were:
- deprivation
- population density
- age structure
- socioeconomic composition
- ethnic diversity

## Concrete next steps to strengthen evidence
1. Add `data/msoa_covariates.csv` with MSOA-level controls (deprivation, population density, age structure, socioeconomic composition, ethnic diversity), then rerun M2/M3.
2. Re-estimate with spatially explicit methods (e.g., spatial error/lag models or spatially robust inference) to address residual clustering.
3. Add temporal information on group activity (if available) to test ordering between exposure and trust.
4. Pre-register a primary specification and robustness set to reduce analytical flexibility and improve transparency.

## Figure guide for presentations
- `outputs/figures/trust_by_group_boxplot.png`: use to show the distributional trust gap between MSOAs with vs without any mapped group.
- `outputs/figures/trust_vs_log_groups_scatter.png`: use to show the positive trust gradient with group intensity.
- `outputs/figures/msoa_map_groups_trust.png`: use to show geographic co-patterns and why spatial dependence diagnostics matter.
