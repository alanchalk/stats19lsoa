#' stats19 data for 2019-2022, summarised to lsoa with socio-demographic indices
#'
#' The STATS19 database is a collection of all road traffic accidents in
#' England, Scotland and Wales that resulted in a personal injury and were
#' reported to the police within 30 days of the accident.  The data is available
#' through the convenient R package stats19 (see reference below).
#'
#' Accident numbers (excluding those on motorways and A(M) roads) have been
#' summarised by year and quarter and by lsoa.  lsoa is a geographical division
#' of England and Wales into about 32,000 different areas.
#'
#' The 2021 UK census datasets at lsoa are freely available and various of these
#' datasets have been merged on, to act as features for the preditive analysis
#' that this dataset is intended for.
#'
#'
#'
#' @usage data(dt_stats19_lsoa)
#'
#' @format A data frame with 276,730 rows and 168 columns:
#' \describe{
#'   \item{lsoa}{A geographical area code}
#'   \item{column names starting with c_ts}{about 165 features from the census data}
#'   \item{column names other}{some other features}
#'   \item{yyqq}{year and quarter of the accident}
#'   \item{nu_cl}{number of accidents}
#'   \item{number_of_vehicles}{total number of vehicles involved in the accidents}
#'   \item{number_of_casualties}{total number of injured persons}
#' }
#' @source <https://www.ons.gov.uk/datasets/TS003/editions/2021/versions/4>
#' @source <https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareapopulationdensity>
#' @source <https://assets.publishing.service.gov.uk/media/5d8b3ade40f0b60999a23330/File_2_-_IoD2019_Domains_of_Deprivation.xlsx>
#' @source <https://assets.publishing.service.gov.uk/media/5d8b3b24ed915d0373d35410/File_4_-_IoD2019_Sub-domains_of_Deprivation.xlsx>
#'
#' @references
#' Lovelace R, Morgan M, Hama L, Padgham M, Ranzolin D, Sparks A (2019).
#' “stats 19: A package for working with open road crash data.”
#' The Journal of Open Source Software, 4(33), 1181. doi:10.21105/joss.01181.#'
#'
"dt_stats19_lsoa"

