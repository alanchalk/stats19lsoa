# stats19lsoa/data-raw/02_create_freq_lsoa.R
#
# Purpose
# -------
# GLMStudio build path for the en-auto-stats19 dataset family. Consumes the
# legacy monograph object produced by 01a_create_data.R and produces a
# corrected modelling-grain object suitable for Lab 7.
#
# Input  : data/dt_stats19_lsoa.RData    (output of 01a_create_data.R, the
#                                         canonical monograph build —
#                                         loaded read-only here; this script
#                                         does not modify the legacy object
#                                         or its file)
# Output : data/dt_stats19_freq_lsoa.rda
#
# Spec   : see GLMStudio_monorepo/docs/en-auto-stats19-dataset-build-plan.md
#
# Divergences from the monograph
# ------------------------------
#   * Monograph silently drops LSOA-quarter rows with zero accidents
#     (selection bias on "had at least one accident in this period").
#     This script restores them via a full LSOA x quarter cross-join with
#     zero-fill — every (LSOA, covid_ind) exposure-bearing row is present.
#   * Monograph treats exposure as pop/1000 per quarter; this script uses
#     ex = sum(pop / 4000) over the quarters in the covid_ind period,
#     i.e. thousand person-years.
#   * Fold assignment, LSOA-constant assertion, MONOGRAPH_CH7_EXCLUDE
#     constant (for downstream PCA / Lab 7 use), and other tightening
#     details are documented inline below.
#
# 01a_create_data.R is preserved unchanged; the monograph dataset and the
# stats19_analysis/RCode/02a-05a* path remain reproducible.

library(data.table)

source(file.path("data-raw", "_constants.R"))
# Brings vars_ind_toUse and MONOGRAPH_CH7_EXCLUDE into scope.

dir_rdata <- file.path(".", "data")

# Load the legacy monograph object (read-only).
load(file.path(dir_rdata, "dt_stats19_lsoa.RData"))
stopifnot(exists("dt_stats19_lsoa"))

# Work on a copy; never mutate the legacy object.
dt_freq <- copy(dt_stats19_lsoa)

# ---- Step 1: typo fix -------------------------------------------------------
# Defensive: 01a now produces c_ts001_restype_ce_pct correctly, but older
# legacy snapshots had c_ts001_restype_cepct. Rename if encountered.
if ("c_ts001_restype_cepct" %in% colnames(dt_freq)) {
  setnames(dt_freq, "c_ts001_restype_cepct", "c_ts001_restype_ce_pct")
}

# ---- Step 2: drop Wales (LSOAs with NA IoD ranks — England-only) -----------
dt_freq <- dt_freq[!is.na(imd_rank)]

# ---- Step 3: zero-variance + redundant column drops -------------------------
mlslib::fn_zeroVariance(dt_freq)
mlslib::fn_removeRedundantCols(dt_freq)

# ---- Step 4: cross-join to ensure all (LSOA x quarter) rows; zero-fill ------
# The legacy merge in 01a joins LSOA-grain features against dt_crashes_lsoa,
# which only contains (lsoa, yyqq) rows where >= 1 accident was recorded.
# The left-join silently omits LSOA-quarter combinations with zero accidents.
# For a frequency model, zero-claim exposure rows are valid observations.
# This step rebuilds the full grid (all surviving LSOAs) x (all 20 quarters)
# and fills nu_cl = 0 where no accident was recorded.

# Universe of quarters in the source crash data — expect Q1 2018 through
# Q4 2022 (20 quarters).
all_yyqq <- sort(unique(dt_freq$yyqq[!is.na(dt_freq$yyqq)]))
stopifnot(length(all_yyqq) == 20)

# LSOA-level feature snapshot — one row per LSOA, drops the yyqq-varying
# columns that the legacy crash merge produced.
yyqq_varying_cols <- intersect(c("yyqq", "nu_cl",
                                  "number_of_vehicles", "number_of_casualties"),
                                colnames(dt_freq))
lsoa_feature_cols <- setdiff(colnames(dt_freq), yyqq_varying_cols)
lsoa_features     <- unique(dt_freq[, ..lsoa_feature_cols])
stopifnot(uniqueN(lsoa_features$lsoa) == nrow(lsoa_features))

# Observed crash counts at (lsoa, yyqq) grain. Drop the LSOAs-with-no-
# accidents-ever rows (yyqq is NA there) — the cross-join below regenerates
# a full per-LSOA grid and fills zero for them.
crash_cols     <- intersect(c("lsoa", "yyqq", "nu_cl",
                              "number_of_vehicles", "number_of_casualties"),
                            colnames(dt_freq))
lsoa_yyqq_obs  <- dt_freq[!is.na(nu_cl) & !is.na(yyqq), ..crash_cols]

# Cross-join: all (LSOA, quarter) combinations.
grid <- CJ(lsoa = lsoa_features$lsoa, yyqq = all_yyqq)

dt_freq <- merge(grid, lsoa_features, by = "lsoa", all.x = TRUE)
dt_freq <- merge(dt_freq, lsoa_yyqq_obs,
                 by = c("lsoa", "yyqq"), all.x = TRUE)

# Fill zeros for LSOA-quarter combinations that had no recorded accidents.
dt_freq[is.na(nu_cl), nu_cl := 0L]
if ("number_of_vehicles" %in% colnames(dt_freq)) {
  dt_freq[is.na(number_of_vehicles), number_of_vehicles := 0]
}
if ("number_of_casualties" %in% colnames(dt_freq)) {
  dt_freq[is.na(number_of_casualties), number_of_casualties := 0]
}

# Sanity: exactly n_lsoas * 20 rows after the cross-join, and no NA targets.
stopifnot(nrow(dt_freq) == uniqueN(dt_freq$lsoa) * 20L)
stopifnot(!any(is.na(dt_freq$nu_cl)))

rm(grid, lsoa_features, lsoa_yyqq_obs, all_yyqq,
   yyqq_varying_cols, lsoa_feature_cols, crash_cols)

# ---- Step 5: covid_ind (4 levels from yyqq_idx) -----------------------------
# yyqq_idx is not present in the legacy .RData; derive from yyqq.
dt_freq[, yyqq_idx := 4 * (as.numeric(substr(yyqq, 1, 4)) - 2018) +
                      as.numeric(substr(yyqq, 7, 7))]
dt_freq[, covid_ind := "A"]
dt_freq[yyqq_idx %in% c(9, 11, 12), covid_ind := "B"]
dt_freq[yyqq_idx %in% c(10, 13),    covid_ind := "C"]
dt_freq[yyqq_idx >= 14,             covid_ind := "D"]
dt_freq[, covid_ind := factor(covid_ind, levels = c("A", "B", "C", "D"))]

# ---- Step 6: covid_date (period start date per covid_ind level) -------------
covid_date_map <- c(A = "2018-01-01",
                    B = "2020-01-01",
                    C = "2020-04-01",
                    D = "2021-04-01")
dt_freq[, covid_date := as.Date(covid_date_map[as.character(covid_ind)])]

# ---- Step 7: fold assignment (LSOA-level, set.seed for reproducibility) -----
set.seed(2024)
dt_fold <- dt_freq[, .(fold = sample(1:10, 1)), by = lsoa]
dt_freq  <- merge(dt_freq, dt_fold, by = "lsoa")
rm(dt_fold)

# Sanity assertion: each LSOA must have exactly one fold value.
stopifnot(dt_freq[, .(n = uniqueN(fold)), by = lsoa][, max(n)] == 1)

# ---- Step 8: feature curation -----------------------------------------------
# Keep only the by-keys, target, fold, derived time columns, and
# vars_ind_toUse (which includes 'pop' via vars_trap_extra in _constants.R).
# Drops raw counts (TS00*_tot, individual sub-categories), the dec ranks,
# lsoa_desc, lad_code, lad, lsoa_code, number_of_vehicles,
# number_of_casualties, yyqq, yyqq_idx, location_*.
#
# Note: no derived _pct aggregates are built in this script. The monograph's
# 03a_EDA.R created _pct versions of c_ts003_hc_sfhmcp* and
# c_ts007a_age_70_74 just above its vars_ind_toUse definition, but
# vars_ind_toUse referenced the raw count columns rather than those _pct
# versions. Building them here would be dead code; the raw count columns
# already exist from 01a's earlier dt_ts003 / dt_ts007a processing.
KEEP_COLS <- c("lsoa", "covid_ind", "covid_date", "fold",
               "nu_cl", vars_ind_toUse)
KEEP_COLS <- intersect(KEEP_COLS, colnames(dt_freq))
dt_freq   <- dt_freq[, ..KEEP_COLS]

# ---- Step 9: LSOA-constant sanity assertion ---------------------------------
# Every column claimed as LSOA-constant must be constant within an LSOA.
# Explicit setdiff of by-keys ('lsoa', 'covid_ind') so they don't end up in
# .SDcols alongside the by-group. vars_ind_toUse already includes 'pop' via
# vars_trap_extra in _constants.R.
LSOA_CONSTANT_COLS <- setdiff(vars_ind_toUse, c("lsoa", "covid_ind"))
LSOA_CONSTANT_COLS <- intersect(LSOA_CONSTANT_COLS, colnames(dt_freq))
const_check <- dt_freq[, lapply(.SD, uniqueN), by = lsoa,
                        .SDcols = LSOA_CONSTANT_COLS]
stopifnot(all(as.matrix(const_check[, -"lsoa", with = FALSE]) == 1))

# ---- Step 10: collapse to (lsoa, covid_ind) grain ---------------------------
# nu_cl is summed; ex is constructed in the same step from sum(pop / 4000)
# (thousand-person-years across quarters in this row's covid_ind period);
# LSOA-constant features are carried through with first().
dt_stats19_freq_lsoa <- dt_freq[, c(
  .(nu_cl = sum(nu_cl),
    ex    = sum(pop / 4000)),
  lapply(.SD, first)
),
by      = .(lsoa, covid_ind, covid_date, fold),
.SDcols = LSOA_CONSTANT_COLS]

# ---- Step 11: unique_id (composite string row key) + column ordering --------
dt_stats19_freq_lsoa[, unique_id :=
                     paste0(lsoa, "_", as.character(covid_ind))]

# Place identifiers, time, fold, target, exposure first; predictors after.
setcolorder(dt_stats19_freq_lsoa,
            c("unique_id", "lsoa", "covid_ind", "covid_date",
              "fold", "nu_cl", "ex"))

# Sanity assertion: unique_id is unique across all rows.
stopifnot(nrow(dt_stats19_freq_lsoa) ==
          uniqueN(dt_stats19_freq_lsoa$unique_id))

# ---- Step 12: save ----------------------------------------------------------
save(dt_stats19_freq_lsoa,
     file = file.path(dir_rdata, "dt_stats19_freq_lsoa.rda"))

# Tidy
rm(dt_stats19_lsoa, dt_freq, const_check, KEEP_COLS, LSOA_CONSTANT_COLS,
   covid_date_map)
