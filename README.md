# UK traffic accident data based on stats19 and census data

stats19 data for 2019-2022, summarised to lsoa with socio-demographic indices

The STATS19 database is a collection of all road traffic accidents in
England, Scotland and Wales that resulted in a personal injury and were
reported to the police within 30 days of the accident.  The data is available
through the convenient R package stats19 (see reference below).

Accident numbers (excluding those on motorways and A(M) roads) have been
summarised by year and quarter and by lsoa.  lsoa is a geographical division
of England and Wales into about 32,000 different areas.

The 2021 UK census datasets at lsoa are freely available and various of these
datasets have been merged on, to act as features for the predictive analysis
that this dataset is intended for.

## Data Sources

The data in this repository has been compiled from the following sources:

- **stats19**: The data is freely available from the ONS, but the very useful
  R package, stats19 was used

- **UK 2021 census data**: Downloads of the UK census data are available heree [census data](https://www.nomisweb.co.uk/sources/census_2021_bulk).

## Dataset Format

The package ships three data objects:

### `dt_stats19_lsoa` — original monograph dataset (unchanged)

- **Rows**: 276,730
- **Columns**: 168
- **Grain**: (lsoa, yyqq) — one row per LSOA × calendar quarter
- **Built by**: `data-raw/01a_create_data.R`
- **Used by**: the monograph reproduction path in `stats19_analysis/RCode/02a_ManipulateData.R` → `03a_EDA.R` → `04*` → `05*`

Each row represents an lsoa for a given year-quarter, plus various features from the census data, `nu_cl` (the number of accidents in that LSOA-quarter), and `pop` (a rough exposure measure: the population of the LSOA).

### `dt_stats19_freq_lsoa` — GLMStudio curated build

- **Rows**: 127,240
- **Columns**: 78
- **Grain**: (lsoa, covid_ind) — one row per LSOA × covid-period band (4 bands)
- **Built by**: `data-raw/01b_create_freq_lsoa.R`
- **Used by**: GLMStudio Lab 7 (Monograph Ch 7, "Highly Correlated Variables")

Cross-joined to include zero-accident exposure rows that the original silently dropped; exposure (`ex`) expressed in thousand person-years; LSOA-level fold (`set.seed(2024)`); composite `unique_id`. England-only (Welsh LSOAs dropped because IoD 2019 is England-only).

### `dt_stats19_freq_lsoa_pca` — GLMStudio PCA-augmented build

- **Rows**: 127,240
- **Columns**: 141 (78 source + 63 PCs)
- **Grain**: same as `dt_stats19_freq_lsoa`
- **Built by**: `data-raw/01c_build_pca.R`
- **Used by**: GLMStudio Lab 7's PCA-LASSO comparison

Same rows as `dt_stats19_freq_lsoa` plus `pc1..pc63`, where the PCA basis is fit on training-fold LSOAs (`fold ∈ 1..7`) only and projected to all LSOAs. Numerical rank turned out to be 63 (one residual linear dependence in the curated compositional families is handled gracefully by `prcomp(rank. = N)`). Two JSON sidecars in `inst/extdata/` carry the loadings matrix and explained-variance schedule with full provenance (training folds, source columns, standardization means/sds, sign convention, generator version).

### Build relationship

The three datasets nest:

```
01a_create_data.R ─► dt_stats19_lsoa
                    │
                    └─► 01b_create_freq_lsoa.R ─► dt_stats19_freq_lsoa
                                                 │
                                                 └─► 01c_build_pca.R ─► dt_stats19_freq_lsoa_pca
                                                                       + inst/extdata/pca_loadings.json
                                                                       + inst/extdata/pca_explained_variance.json
```

`dt_stats19_lsoa` remains the canonical monograph dataset and is preserved unchanged. The other two are derived GLMStudio products. See `data-raw/_constants.R` for the shared `vars_ind_toUse` and `MONOGRAPH_CH7_EXCLUDE` lists, and the GLMStudio monorepo's `docs/en-auto-stats19-dataset-build-plan.md` for the full build spec and the divergences from the monograph.

## Usage

This data has been prepared for use in case studies demonstrating building of predictive models using generalised linear models and other techniques.

## Contributing

Contributions to this project are welcome. The data preparation of the existing dataset can be improved and in addition further datasets could be prepared using the same data sources.

## License

This project is licensed under [License Name]. Please refer to the `LICENSE` file for more details.
