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

The main dataset, `dt_stats19_lsoa`, is structured as a data frame with the following characteristics:

- **Rows**: 276,730
- **Columns**: 168

Each row represents an lsoa for a given year-quarter various featrures from the census data as well as `nu_cl` - the number of accidents, and `ex` - a very rough estimation of the exposure which is just the population of the lsoa.

## Usage

This data has been prepared for use in case studies demonstrating building of predictive models using generalised linear models and other techniques.

## Contributing

Contributions to this project are welcome. The data preparation of the existing dataset can be improved and in addition further datasets could be prepared using the same data sources.

## License

This project is licensed under [License Name]. Please refer to the `LICENSE` file for more details.
