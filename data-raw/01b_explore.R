# 01b_explore.R

# libraries needed
library(data.table)
library(lubridate)

library(mgcv)

dir_project <- getwd()
dir_rawdata <- file.path(dir_project, 'rawdata')
dir_rdata <- file.path(dir_project, 'data')

load(file = file.path(dir_rdata, 'dt_stats19_lsoa.RData'))
dim(dt_stats19_lsoa)

dt_all <- dt_stats19_lsoa
rm(dt_stats19_lsoa)

vars_all <- colnames(dt_all)
sum(dt_all$nu_cl)

dt_all[, ex := pop/1000]

fn_table <- function(var_check, order='ex'){
  dt_results <- dt_all[, list(ex=round(sum(ex)/1000,0),
                                   nu_cl=sum(nu_cl),
                                   freq=100*sum(nu_cl) / sum(ex)),
                            by=var_check]
  if (order=='ex') {
    setorder(dt_results, -ex)
  } else {
    setorderv(dt_results, var_check)
  }
  dt_results
}

#var_check = 'area'
fn_table_decile <- function(var_check, order='ex'){
  var_dec <- paste0(var_check, '_dec')
  dt_all[,
         paste0(var_check, '_dec') := cut(get(var_check),
                             breaks = quantile(get(var_check),
                                               probs = 0:10/10,
                                               na.rm = TRUE),
                             labels = FALSE,
                             include.lowest = TRUE)]
  dt_results <- dt_all[,
                       list(nu_lsoa = .N,
                            ex = sum(ex),
                            nu_cl = sum(nu_cl, na.rm = TRUE),
                            freq = sum(nu_cl, na.rm = TRUE) / sum(ex)
                       ),
                       by = get(var_dec)]
  setnames(dt_results, 'get', paste0(var_check, '_dec'))
  setorderv(dt_results, paste0(var_check, '_dec'))
  dt_results
}

var_target <- 'nu_cl'
var_offset <- 'ex'

vars_ind <- setdiff(vars_all,
                    c(var_target,
                      var_offset,
                      c("lsoa_code", "lsoa", "pop",
                        "lad_code", "lad",
                        "number_of_vehicles", "number_of_casualties", "pop_decile")))

vars_ind <- setdiff(vars_ind,
                    vars_ind[grep('_dec$', vars_ind)])



# area
fn_table_decile('area')

# "popdens"
fn_table_decile("popdens")

fn_table_decile("imd_rank")
fn_table_decile("income_rank")
fn_table_decile("employment_rank")
fn_table_decile("est_rank")
fn_table_decile("hd_rank")
fn_table_decile("crime_rank")
fn_table_decile("bhs_rank")
fn_table_decile("le_rank")


fn_table_decile("cayp_rank")
fn_table_decile("as_rank")
fn_table_decile("gb_rank")
fn_table_decile("wb_rank")
fn_table_decile("i_rank")
fn_table_decile("o_rank")


vars_types <- sapply(dt_all[, vars_ind, with = FALSE], is.numeric)
vars_ind_num <- vars_ind[vars_types]
vars_ind_char <- setdiff(vars_ind, vars_ind_num)

# dt_all <- dt_all[,
#                  (vars_ind_char) := lapply(.SD, as.factor),
#                  .SDcols = vars_ind_char]


sapply(dt_all[, vars_ind_char, with = FALSE], function(x) length(unique(x)))
sapply(dt_all[, vars_ind_char, with = FALSE], function(x) sum(is.na(x)))

sapply(dt_all[, vars_ind_num, with = FALSE], function(x) sum(is.na(x)))

fmla_n <- paste0(paste0("s(", vars_ind_num, ", bs='cr', k=3)"), collapse = " + ")

fmla_ <- as.formula(paste0(var_target, ' ~ ',
                           fmla_n
                           #, ' + ',
                           #paste0(vars_ind_char, collapse = " + ")
                           ))

bam_1 <- bam(
  fmla_,
  family=poisson(),
  offset = log(dt_all[[var_offset]]),
  data = dt_all)

plot(bam_1)
summary(bam_1)


