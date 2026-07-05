# Interpretation of MSOA Trust and Mutual Aid Results with IMD Deprivation

## Executive takeaway
After adding IMD deprivation (`Score`, higher = more deprived), deprivation is a very strong negative correlate of trust, and the mutual-aid coefficients remain positive but are substantially smaller than in the prior adjusted models. This indicates that deprivation explains a large share of the raw trust gradient, while mutual aid still shows an independent positive association. Results remain associational, not causal.

## Analysis basis
- Unit of analysis: England MSOAs.
- Working dataset: `outputs/analysis_dataset.csv` joined to `IMD::imd2019_england_msoa11` by `msoa11_code`.
- Coverage: 6,791 / 6,791 rows matched to IMD (`Score` missing count = 0).
- Primary adjusted deprivation term: `z_Score` (1 SD = ~13.21 IMD score points).

## Main estimates (primary scenario: `bbox_on_full`)
- M0 (`Net_trust ~ any_group`): `any_groupTRUE = 0.0542` (95% CI `0.0480` to `0.0605`).
- M1 (`Net_trust ~ log_groups`): `log_groups = 0.0580` (95% CI `0.0508` to `0.0652`).
- M2 (`Net_trust ~ any_group + z_Score + urban_rural + region FE`):
  - `any_groupTRUE = 0.0162` (95% CI `0.0126` to `0.0198`).
  - `z_Score = -0.0961` (95% CI `-0.0981` to `-0.0942`).
- M3 (`Net_trust ~ log_groups + z_Score + urban_rural + region FE`):
  - `log_groups = 0.0199` (95% CI `0.0157` to `0.0241`).
  - `z_Score = -0.0962` (95% CI `-0.0981` to `-0.0942`).

## Attenuation and model-fit implications
Relative to the prior adjusted specification without IMD deprivation:
- `any_group` attenuates from ~`0.0319` to `0.0162` (about **49.4%** smaller).
- `log_groups` attenuates from ~`0.0344` to `0.0199` (about **42.1%** smaller).

Model fit increases substantially when deprivation is included:
- Adjusted binary model R²: from ~`0.178` to **`0.723`**.
- Adjusted intensity model R²: from ~`0.177` to **`0.724`**.

Interpretation: deprivation is a dominant structural correlate of local trust. Ignoring it overstates the apparent mutual-aid/trust association.

## Robustness and diagnostics
- Top-1% trimming keeps signs and similar magnitudes (`any_groupTRUE ~ 0.0154`, `log_groups ~ 0.0192`, `z_Score ~ -0.0960`).
- Bounding-box on/off remains unchanged in this workspace run.
- Multicollinearity remains modest (max VIF ~`2.49`).
- Residual spatial clustering persists (Moran's I ~`0.432`, `p = 0.005`).

## Substantive implications
1. Place-based deprivation must be treated as a central confounder in trust analyses.
2. Mutual aid still has a positive residual association with trust after controlling for deprivation and broad geography, but the magnitude is materially smaller than unadjusted or weakly adjusted estimates.
3. Policy interpretation should avoid claiming that expanding mutual aid alone will close trust gaps where deprivation is high; deprivation-sensitive strategies are likely required.
4. Because residual spatial dependence remains, uncertainty from non-spatial models may still be optimistic.

## Limits and next steps
- This is associational evidence and does not identify causal effects.
- Reverse causality remains plausible (higher-trust areas may sustain more groups).
- Next steps:
  1. Add remaining covariates (population density, age structure, socioeconomic composition, ethnic diversity).
  2. Use spatially explicit models or spatially robust inference.
  3. Add temporal ordering of group activity versus trust where possible.
