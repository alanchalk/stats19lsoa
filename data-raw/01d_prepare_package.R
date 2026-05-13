
rm(list=ls())
dir_project <- getwd()

library(devtools)
library(usethis)
library(roxygen2)

# load data prepared
load(file = file.path('/Users/axc/Dropbox/CAS/stats19_data_prep/data', 'dt_stats19_lsoa.RData'))

create_package("/Users/axc/Dropbox/CAS/packages/stats19lsoa")

# load the data as .rda into /data directory
usethis::use_data(dt_stats19_lsoa, overwrite = TRUE)

usethis::use_git()

devtools::document()
devtools::check()
devtools::build()

