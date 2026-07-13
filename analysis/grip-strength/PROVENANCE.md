# Data provenance and analytical scope

## Official sources

The pipeline downloads public SAS transport files directly from the US Centers for Disease Control and Prevention, National Center for Health Statistics:

- NHANES 2011–2012: `MGX_G`, `DEMO_G`, `BMX_G`, `PAQ_G`
- NHANES 2013–2014: `MGX_H`, `DEMO_H`, `BMX_H`, `PAQ_H`

The executable URL list and checksums are in `artifacts/source_manifest.csv`. Variable definitions are documented in the CDC component pages, including [grip strength](https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/MGX_G.htm), [body measures](https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/BMX_G.htm), and [weighting guidance](https://wwwn.cdc.gov/nchs/nhanes/tutorials/weighting.aspx).

## Transformations

Files are required to have unique non-missing `SEQN` values and are joined one-to-one, starting from the grip examination file. Adults aged 18–80 are retained. Combined MEC weights divide each two-year examination weight by two.

`MGATHAND` maps Hand 1 and Hand 2 trial order to right and left. Outcomes use only trials marked maximal effort by the examiner. The raw sum of the two hand maxima is independently reconciled against `MGDCGSZ`.

Physical activity is weekly MET-minutes from work, transport, and recreation: vigorous minutes use 8 MET and moderate/transport minutes use 4 MET. Activity is not imputed. No grip outcome is imputed.

## Statistical scope

Primary models are fitted separately by released male/female category and outcome using `survey::svyglm`. Core models use natural splines for age, height, and BMI. Extended models add arm measurements, log activity, handedness, pain/surgery, and test posture. Weighted GAMs and standing/no-pain cohorts are sensitivity analyses.

Race/ethnicity is never a prediction input. It is retained only to audit out-of-fold calibration by released subgroup.

Personal intervals use weighted out-of-fold residuals for the same sex and age band, expanding to adjacent age bands when Kish effective sample size is below 100. Empirical tail probabilities require an observed grip measurement.

## Limitations

The reference population is non-institutionalized US adults in 2011–2014. Grip protocol, device, effort, posture, pain, and surgery affect comparability. Grip is not equivalent to total upper-body strength. Public NHANES data in these cycles provide a binary male/female category, which limits representation. No result from this project is a diagnosis.
