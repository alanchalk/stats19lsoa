# proposal - create lsoa dataset with various columns which are 
# hopefully correlated and also hopefully some are predictive on some aspect 
# of traffic accident data

# stats19 
# - exclude A(M) roads and motorway
# - make quarterly so that we have Q as a feature and also
# can use covid. Extend to 2018

# On traffic accident data either use number of injured per accident
# or have to use number of accidents at lsoa level and using some measure of 
# exposure - I used population which seems OK

# 

# traffic data
#https://storage.googleapis.com/dft-statistics/road-traffic/downloads/data-gov-uk/local_authority_traffic.csv

library(openxlsx)
library(data.table)
library(dtplyr)

library(dplyr)
library(tidyr)

dir_project <- getwd()
dir_rawdata <- file.path("." ,'rawdata')
#dir_rawdata <- file.path(dir_project, 'rawdata')
dir_rdata <- file.path(dir_project, 'data')


#-------------------------------------------------------------------------------
# household composition - this is TS003 from census
#https://www.ons.gov.uk/datasets/TS003/editions/2021/versions/4
# https://www.ons.gov.uk/datasets/TS003/editions/2021/versions/4#get-data
#TS003-2021-4.csv
# dt_hhcomp <- fread(file.path(dir_rawdata, 'TS003-2021-4.csv'))
# colnames(dt_hhcomp) <- c('ltla_code', 'ltla', 'hhcomp_code', 'hhcomp', 'count')
# dt_hhcomp[hhcomp_code == -8, sum(count)]
# dt_hhcomp <- dt_hhcomp[hhcomp_code != -8, ]
# dt_hhcomp_wide <- dcast(dt_hhcomp,
#                         ltla_code ~ hhcomp_code, 
#                         value.var = "count",
#                         sep = '_prefix_')

# Add the prefix to all column names except the first one
# setnames(dt_hhcomp_wide,
#          old = colnames(dt_hhcomp_wide)[-1], 
#          new = paste0('hhcomp_', colnames(dt_hhcomp_wide)[-1]))
# 
# cols <- paste0('hhcomp_', 1:14)
# dt_hhcomp_wide[, rowSum := rowSums(.SD), .SDcols = cols]
# dt_hhcomp_wide[, 
#                (cols) := lapply(.SD, function(x) round(100 * x / rowSum, 0)),
#                .SDcols = cols]
# dt_hhcomp_wide[, rowSum := NULL]


#-------------------------------------------------------------------------------
# lsoa population density - seems to be TS006
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareapopulationdensity
# sape23dt11mid2020lsoapopulationdensity.xlsx
df_popdens <- openxlsx::read.xlsx(file.path(dir_rawdata, 'sape23dt11mid2020lsoapopulationdensity.xlsx'),
                    sheet = 'Mid-2020 Population Density',
                    startRow = 5)

colnames(df_popdens) <- c('lsoa', 'lsoa_desc', 'pop', 'area', 'popdens')


#-------------------------------------------------------------------------------
#File_2_-_IoD2019_Domains_of_Deprivation.xlsx
#https://assets.publishing.service.gov.uk/media/5d8b3ade40f0b60999a23330/File_2_-_IoD2019_Domains_of_Deprivation.xlsx
df_dep_file2 <- openxlsx::read.xlsx(file.path(dir_rawdata, 'File_2_-_IoD2019_Domains_of_Deprivation.xlsx'),
                                  sheet = 'IoD2019 Domains',
                                  startRow = 1)
dt_dep_file2 <- setDT(df_dep_file2)
rm(df_dep_file2)

colnames(dt_dep_file2) <- c('lsoa', 'lsoa_desc', 'lad_code', 'lad',
                            'imd_rank', 'imd_dec',
                            'income_rank', 'income_dec',
                            'employment_rank', 'employment_dec',
                            'est_rank', 'est_dec',
                            'hd_rank', 'hd_dec',
                            'crime_rank', 'crime_dec',
                            'bhs_rank', 'bhs_dec',
                            'le_rank', 'le_dec')

cols_to_remove <- grep("_dec$", colnames(dt_dep_file2), value = TRUE)

dt_dep_file2[, (cols_to_remove) := NULL]
rm(cols_to_remove)

cols_to_ptile <- grep("_rank$", colnames(dt_dep_file2), value = TRUE)
dt_dep_file2[, (cols_to_ptile) := lapply(.SD, function(x) {
  ranks <- rank(x, ties.method = "average")
  round(100 * (ranks - 1) / (length(x) - 1), 0)}), 
  .SDcols = cols_to_ptile]
rm(cols_to_ptile)


#-------------------------------------------------------------------------------
#File_4_-_IoD2019_Sub-domains_of_Deprivation.xlsx
#https://assets.publishing.service.gov.uk/media/5d8b3b24ed915d0373d35410/File_4_-_IoD2019_Sub-domains_of_Deprivation.xlsx
df_dep_file4 <- openxlsx::read.xlsx(file.path(dir_rawdata, 'File_4_-_IoD2019_Sub-domains_of_Deprivation.xlsx'),
                                    sheet = 'IoD2019 Sub-domains',
                                    startRow = 1)

dt_dep_file4 <- setDT(df_dep_file4)
rm(df_dep_file4)

colnames(dt_dep_file4) <- 
  c('lsoa', 'lsoa_desc', 'lad_code', 'lad',
    'est_rank', "est_dec",
    "cayp_rank", "cayp_dec",
    "as_rank", "as_dec",
    "bhs_rank", "bhs_dec",
    "gb_rank", "gb_dec",
    "wb_rank", "wb_dec",
    "le_rank", "le_dec",
    "i_rank", "i_dec",
    "o_rank", "o_dec" 
    )

cols_to_remove <- grep("_dec$", colnames(dt_dep_file4), value = TRUE)

dt_dep_file4[, (cols_to_remove) := NULL]
rm(cols_to_remove)

cols_to_ptile <- grep("_rank$", colnames(dt_dep_file4), value = TRUE)
dt_dep_file4[, (cols_to_ptile) := lapply(.SD, function(x) {
  ranks <- rank(x, ties.method = "average")
  round(100 * (ranks - 1) / (length(x) - 1), 0)}), 
  .SDcols = cols_to_ptile]


#-------------------------------------------------------------------------------
# police
# https://data.police.uk/data/fetch/b4988fee-94bc-44d8-92d8-d22220e06629/


#-------------------------------------------------------------------------------
# census
# https://www.nomisweb.co.uk/sources/census_2021_bulk

dir_census <- '/Users/axc/Dropbox/CAS/stats19_data_prep/rawdata/census_nomisweb'

# TS001 	Number of usual residents in households and communal establishments
dt_ts001 <- fread(file.path(dir_census, 'census2021-ts001/census2021-ts001-lsoa.csv'))
dt_ts001[, date := NULL]
dt_ts001[, geography := NULL]

colnames(dt_ts001) <- c('lsoa', 
                        'c_ts001_restype_tot', 
                        'c_ts001_restype_hh',
                        'c_ts001_restype_ce')
# note that a sensible feature here is ce pcs hh
dt_ts001[, 
         c_ts001_restype_cepct := round(100 * c_ts001_restype_ce / c_ts001_restype_tot,
                                        1)
         ]


# TS002  TS002 	Legal partnership status
dt_ts002 <- fread(file.path(dir_census, 'census2021-ts002/census2021-ts002-lsoa..csv'))
colnames(dt_ts002)
dt_ts002[, date := NULL]
dt_ts002[, geography := NULL]
colnames(dt_ts002)

colnames(dt_ts002) <- c('lsoa', 
                        'c_ts002_mcp_tot', 
                        'c_ts002_mcp_never',
                        'c_ts002_mcp_mrcp',
                        'c_ts002_mcp_m',
                        'c_ts002_mcp_mos',
                        'c_ts002_mcp_mss',
                        'c_ts002_mcp_rcp',
                        'c_ts002_mcp_rcpos',
                        'c_ts002_mcp_rcpss',
                        'c_ts002_mcp_slmcp',
                        'c_ts002_mcp_slm',
                        'c_ts002_mcp_slcp',
                        'c_ts002_mcp_dcpd',
                        'c_ts002_mcp_d',
                        'c_ts002_mcp_cpd',
                        'c_ts002_mcp_wscp',
                        'c_ts002_mcp_w',
                        'c_ts002_mcp_scp')

vars_to_pct <- colnames(dt_ts002)[-1]
vars_pct <- paste0(vars_to_pct, '_pct')

dt_ts002[, rowSum := rowSums(.SD), .SDcols = vars_to_pct]
dt_ts002[,
               (vars_pct) := lapply(.SD, function(x) round(100 * x / rowSum, 0)),
               .SDcols = vars_to_pct]
dt_ts002[, rowSum := NULL]


#TS003 	Household composition
dt_ts003 <- fread(file.path(dir_census, 'census2021-ts003/census2021-ts003-lsoa.csv'))
dt_ts003[, date := NULL]
dt_ts003[, geography := NULL]
colnames(dt_ts003)
colnames(dt_ts003) <- c('lsoa', 
                        'c_ts003_hc_tot', 
                        'c_ts003_hc_oph',
                        'c_ts003_hc_oph66p',
                        'c_ts003_hc_opho',
                        'c_ts003_hc_sfh',
                        'c_ts003_hc_sfh66p',
                        'c_ts003_hc_sfhmcp',
                        'c_ts003_hc_sfhmcpnc',
                        'c_ts003_hc_sfhmcpdc',
                        'c_ts003_hc_sfhmcpacnd',
                        'c_ts003_hc_sfhccf',
                        'c_ts003_hc_sfhccfnc',
                        'c_ts003_hc_sfhccfdc',
                        'c_ts003_hc_sfhccfacnd',
                        'c_ts003_hc_sfhlpf',
                        'c_ts003_hc_sfhlpfdc',
                        'c_ts003_hc_sfhlpfacnd',
                        'c_ts003_hc_sfhosfh',
                        'c_ts003_hc_sfhosfho',
                        'c_ts003_hc_oht',
                        'c_ts003_hc_ohtdc',
                        'c_ts003_hc_ohto'
)
cnames <- colnames(dt_ts003)

var_type <- 'c_ts003_hc_oph'
vars_type <- 'c_ts003_hc_oph'
dt_ts003[,
         (paste0(var_type, '_pct')) := round(100 * rowSums(.SD) / c_ts003_hc_tot, 1),
         .SDcols = vars_type]

var_type <- 'c_ts003_hc_sfh'
vars_type <- 'c_ts003_hc_sfh'
dt_ts003[,
         (paste0(var_type, '_pct')) := round(100 * rowSums(.SD) / c_ts003_hc_tot, 1),
         .SDcols = vars_type]

var_type <- 'c_ts003_hc_oht'
vars_type <- 'c_ts003_hc_oht'
dt_ts003[,
         (paste0(var_type, '_pct')) := round(100 * rowSums(.SD) / c_ts003_hc_tot, 1),
         .SDcols = vars_type]

rowSums(dt_ts003[, colnames(dt_ts003)[grep("_pct", colnames(dt_ts003))], with = FALSE])


#TS004 	Country of birth
dt_ts004 <- fread(file.path(dir_census, 'census2021-ts004/census2021-ts004-lsoa.csv'))
dt_ts004[, date := NULL]
dt_ts004[, geography := NULL]
colnames(dt_ts004)
dt_ts004 <- dt_ts004[, 
                     c("geography code",
                       "Country of birth: Total; measures: Value",
                       "Country of birth: Europe: United Kingdom; measures: Value",
                       "Country of birth: Europe: EU countries; measures: Value"
                       ),
                     with = FALSE]

colnames(dt_ts004) <- c('lsoa',
                        'c_ts004_cob_tot', 
                        'c_ts004_cob_uk',
                        'c_ts004_cob_eu')
colnames(dt_ts004)

dt_ts004[,
         ('c_ts004_cob_uk_pct') := round(100 * c_ts004_cob_uk / c_ts004_cob_tot, 0)
         ]

dt_ts004[,
         ('c_ts004_cob_eu_pct') := round(100 * c_ts004_cob_eu / c_ts004_cob_tot, 0)
]

dt_ts004[, ('c_ts004_cob_oth_pct') := 100 - c_ts004_cob_uk_pct - c_ts004_cob_eu_pct]


#TS006 	Population density
# other pop density already merged area and population - keep it
# dt_ts006 <- fread(file.path(dir_census, 'census2021-ts006/census2021-ts006-lsoa.csv'))
# dt_ts006[, date := NULL]
# dt_ts006[, geography := NULL]
# colnames(dt_ts006)


#TS007a 	Age by five-year age bands
dt_ts007a <- fread(file.path(dir_census, 'census2021-ts007a/census2021-ts007a-lsoa.csv'))
dt_ts007a[, date := NULL]
dt_ts007a[, geography := NULL]
colnames(dt_ts007a)

colnames(dt_ts007a) <- c('lsoa',
'c_ts007a_age_tot',
'c_ts007a_age_00_04',
'c_ts007a_age_05_09',
'c_ts007a_age_10_14',
'c_ts007a_age_15_19',
'c_ts007a_age_20_24',
'c_ts007a_age_25_29',
'c_ts007a_age_30_34',
'c_ts007a_age_35_39',
'c_ts007a_age_40_44',
'c_ts007a_age_45_49',
'c_ts007a_age_50_54',
'c_ts007a_age_55_59',
'c_ts007a_age_60_64',
'c_ts007a_age_65_69',
'c_ts007a_age_70_74',
'c_ts007a_age_75_79',
'c_ts007a_age_80_84',
'c_ts007a_age_85_'
)
colnames(dt_ts007a)

dt_ts007a[,
         ('c_ts007a_age_00_14_pct') := 
           round(100 * rowSums(.SD) / c_ts007a_age_tot, 0),
         .SDcols = c('c_ts007a_age_00_04',
                     'c_ts007a_age_05_09',
                     'c_ts007a_age_10_14')
         ]

dt_ts007a[,
          ('c_ts007a_age_15_29_pct') := 
            round(100 * rowSums(.SD) / c_ts007a_age_tot, 0),
          .SDcols = c('c_ts007a_age_15_19',
                      'c_ts007a_age_20_24',
                      'c_ts007a_age_25_29'
          )
]

dt_ts007a[,
          ('c_ts007a_age_30_64_pct') := 
            round(100 * rowSums(.SD) / c_ts007a_age_tot, 0),
          .SDcols = c('c_ts007a_age_30_34',
                      'c_ts007a_age_35_39',
                      'c_ts007a_age_40_44',
                      'c_ts007a_age_45_49',
                      'c_ts007a_age_50_54',
                      'c_ts007a_age_55_59',
                      'c_ts007a_age_60_64'
          )
]

dt_ts007a[,
          ('c_ts007a_age_65_pct') := 
            round(100 * rowSums(.SD) / c_ts007a_age_tot, 0),
          .SDcols = c('c_ts007a_age_65_69',
                      'c_ts007a_age_70_74',
                      'c_ts007a_age_75_79',
                      'c_ts007a_age_80_84',
                      'c_ts007a_age_85_'
          )
]


#TS008 	Sex
dt_ts008 <- fread(file.path(dir_census, 'census2021-ts008/census2021-ts008-lsoa.csv'))
dt_ts008[, date := NULL]
dt_ts008[, geography := NULL]
colnames(dt_ts008)

colnames(dt_ts008) <- c('lsoa',
                         'c_ts008_sex_tot',
                         'c_ts008_sex_f',
                         'c_ts008_sex_m')
colnames(dt_ts008)

dt_ts008[,
          ('c_ts008_sex_f_pct') := round(100 * c_ts008_sex_f / c_ts008_sex_tot, 0),
          ]


#TS011 	Households by deprivation dimensions
dt_ts011 <- fread(file.path(dir_census, 'census2021-ts011/census2021-ts011-lsoa.csv'))
dt_ts011[, date := NULL]
dt_ts011[, geography := NULL]
colnames(dt_ts011)

colnames(dt_ts011) <- c('lsoa',
                        'c_ts011_hhd_tot',
                        'c_ts011_hhd_0',
                        'c_ts011_hhd_1',
                        'c_ts011_hhd_2',
                        'c_ts011_hhd_3',
                        'c_ts011_hhd_4')

colnames(dt_ts011)

dt_ts011[,
          ('c_ts011_hhd_0_pct') := round(100 * c_ts011_hhd_0 / c_ts011_hhd_tot, 0),
         ]
dt_ts011[,
         ('c_ts011_hhd_1_pct') := round(100 * c_ts011_hhd_1 / c_ts011_hhd_tot, 0),
]
dt_ts011[,
         ('c_ts011_hhd_2_pct') := round(100 * c_ts011_hhd_2 / c_ts011_hhd_tot, 0),
]
dt_ts011[,
         ('c_ts011_hhd_3_pct') := round(100 * c_ts011_hhd_3 / c_ts011_hhd_tot, 0),
]
dt_ts011[,
         ('c_ts011_hhd_4_pct') := round(100 * c_ts011_hhd_4 / c_ts011_hhd_tot, 0),
]


#TS058 	Distance travelled to work
dt_ts058 <- fread(file.path(dir_census, 'census2021-ts058/census2021-ts058-lsoa.csv'))
dt_ts058[, date := NULL]
dt_ts058[, geography := NULL]
colnames(dt_ts058)

dt_ts058 <- dt_ts058[, 
                     c("geography code",
                       "Distance travelled to work: Total: All usual residents aged 16 years and over in employment the week before the census",
                       "Distance travelled to work: Less than 2km",
                       "Distance travelled to work: 2km to less than 5km",
                       "Distance travelled to work: 5km to less than 10km",
                       "Distance travelled to work: 10km to less than 20km",
                       "Distance travelled to work: Works mainly from home",
                       "Distance travelled to work: Works mainly at an offshore installation, in no fixed place, or outside the UK")]

colnames(dt_ts058) <- c('lsoa',
                        'c_ts058_dtw_tot',
                        'c_ts058_dtw_00_01',
                        'c_ts058_dtw_02_04',
                        'c_ts058_dtw_05_09',
                        'c_ts058_dtw_10_19',
                        'c_ts058_dtw_home',
                        'c_ts058_dtw_offshore')

# List of columns to transform
cols <- c('c_ts058_dtw_00_01',
          'c_ts058_dtw_02_04',
          'c_ts058_dtw_05_09',
          'c_ts058_dtw_10_19', 
          'c_ts058_dtw_home', 
          'c_ts058_dtw_offshore')

dt_ts058[,
         ('c_ts058_dtw_20_') := c_ts058_dtw_tot - rowSums(.SD),
         .SDcols = cols]

cols <- c('c_ts058_dtw_00_01',
          'c_ts058_dtw_02_04',
          'c_ts058_dtw_05_09',
          'c_ts058_dtw_10_19',
          'c_ts058_dtw_20_',
          'c_ts058_dtw_home', 
          'c_ts058_dtw_offshore')

dt_ts058[,
         paste0(cols, "_pct") := 
           lapply(.SD, function(x) round(x / c_ts058_dtw_tot * 100, 1)), 
         .SDcols = cols]

rowSums(dt_ts058[, 
                 c(colnames(dt_ts058)[grep('_pct', colnames(dt_ts058))]),
                 with = FALSE]
        )


#   TS063 	Occupation
dt_ts063 <- fread(file.path(dir_census, 'census2021-ts063/census2021-ts063-lsoa.csv'))
dt_ts063[, date := NULL]
dt_ts063[, geography := NULL]
colnames(dt_ts063)

colnames(dt_ts063) <- c('lsoa',
                        'c_ts063_occ_tot',
                        'c_ts063_occ_1',
                        'c_ts063_occ_2',
                        'c_ts063_occ_3',
                        'c_ts063_occ_4',
                        'c_ts063_occ_5',
                        'c_ts063_occ_6',
                        'c_ts063_occ_7',
                        'c_ts063_occ_8',
                        'c_ts063_occ_9')

cols <- colnames(dt_ts063)[c(-1, -2)]
dt_ts063[,
         paste0(cols, "_pct") := 
           lapply(.SD, function(x) round(x / c_ts063_occ_tot * 100, 1)), 
         .SDcols = cols]


lst_dt_names <-ls(pattern = "^dt_ts")
lst_dt <- mget(lst_dt_names)
dt_all <- Reduce(function(x, y) merge(x, y, by = "lsoa", all = TRUE), lst_dt)
dim(dt_all)

# three others to merge from the begining of this script
dt_popdens <- setDT(df_popdens)
rm(df_popdens)
dt_dep_file2
dt_dep_file4

dt_dep_file2[, lsoa_desc := NULL]
dt_dep_file4[, lsoa_desc := NULL]

dt_add <- merge(dt_popdens,
                dt_dep_file2,
                all.x = TRUE,
                all.y = TRUE,
                by.x = 'lsoa',
                by.y = 'lsoa')

dt_add <- merge(dt_add,
                dt_dep_file4[, 
                             c('lsoa',
                               "cayp_rank",
                               "as_rank",
                               "gb_rank",
                               "wb_rank",
                               "i_rank",
                               "o_rank"),
                             with = FALSE],
                all.x = TRUE,
                all.y = TRUE,
                by.x = 'lsoa',
                by.y = 'lsoa')


dt_all <- merge(dt_all,
                dt_add,
                all.x = TRUE,
                all.y = FALSE,
                by.x = 'lsoa',
                by.y = 'lsoa')


library(stats19)
library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(leaflet)
library(geojsonsf)
library(htmltools)

#sum(crashes_2022$local_authority_ons_district == 'E06000001')

df_crashes_2018 <- get_stats19(2018, silent = TRUE, output_format = "data.frame")
df_crashes_2019 <- get_stats19(2019, silent = TRUE, output_format = "data.frame")
df_crashes_2020 <- get_stats19(2020, silent = TRUE, output_format = "data.frame")
df_crashes_2021 <- get_stats19(2021, silent = TRUE, output_format = "data.frame")
df_crashes_2022 <- get_stats19(2022, silent = TRUE, output_format = "data.frame")
dt_crashes <- setDT(rbind(df_crashes_2018,
                          df_crashes_2019,
                          df_crashes_2020,
                          df_crashes_2021,
                          df_crashes_2022))
rm(df_crashes_2018,
   df_crashes_2019,
   df_crashes_2020,
   df_crashes_2021,
   df_crashes_2022)

table(dt_crashes$first_road_class)
dt_crashes[, list(number_of_vehicles = sum(number_of_vehicles)), by = 'first_road_class']

dt_crashes[, yyqq := paste0(year(date), "-Q", quarter(date))]

dt_crashes_lsoa <- 
  dt_crashes[!(first_road_class %in% c('Motorway', 'A(M)')),
              list(nu_cl = .N,
                   number_of_vehicles = sum(number_of_vehicles),
                   number_of_casualties = sum(number_of_casualties)),
              by = c('lsoa_of_accident_location', 'yyqq')]



dt_all <- merge(dt_all,
                dt_crashes_lsoa,
                all.x = TRUE,
                all.y = FALSE,
                by.x = 'lsoa',
                by.y = 'lsoa_of_accident_location')

dt_all <- setDT(dt_all)

# lost 60k claims - massively up from previous - only change
# was to limit to only those lsoa in census
sum(dt_all$nu_cl, na.rm = TRUE) # 463k
sum(dt_crashes_lsoa$nu_cl) # 520k

dt_all[, pop_decile := cut(pop, 
                           breaks = quantile(pop, probs = 0:10/10, na.rm = TRUE), 
                           labels = FALSE,
                           include.lowest = TRUE)]
dt_results <- dt_all[,
       list(nu_lsoa = .N,
            pop = sum(pop),
            pop_mean = sum(pop) / .N,
            nu_cl = sum(nu_cl, na.rm = TRUE),
            freq_n = sum(nu_cl, na.rm = TRUE) / .N,
            freq_pop = sum(nu_cl, na.rm = TRUE) / sum(pop)
            ),
       by = 'pop_decile']
setorder(dt_results, pop_decile)
dt_results

dt_stats19_lsoa <- dt_all
colnames(dt_stats19_lsoa)
rm(dt_all)

dt_stats19_lsoa[, pop_decile := NULL]
colnames(dt_stats19_lsoa)
save(dt_stats19_lsoa, file = file.path(dir_rdata, 'dt_stats19_lsoa.RData'))




