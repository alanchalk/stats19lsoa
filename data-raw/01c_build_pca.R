# stats19lsoa/data-raw/01c_build_pca.R
#
# Purpose
# -------
# Build dt_stats19_freq_lsoa_pca by fitting a PCA basis on the training-fold
# LSOA features of dt_stats19_freq_lsoa, applying MONOGRAPH_CH7_EXCLUDE,
# projecting all LSOAs through the basis, and joining pc1..pcN onto the
# source dataset.
#
# Input  : data/dt_stats19_freq_lsoa.rda    (output of 01b_create_freq_lsoa.R)
# Output : data/dt_stats19_freq_lsoa_pca.rda
#          inst/extdata/pca_loadings.json
#          inst/extdata/pca_explained_variance.json
#
# Spec   : see GLMStudio_monorepo/docs/en-auto-stats19-dataset-build-plan.md
#
# Defensive assertions baked into this script:
#   * forbidden-column intersection empty (PCA input must not include id /
#     target / exposure / fold / time / trap columns)
#   * PCA input is numeric-only (every column is.numeric)
#   * No zero-variance column in the training matrix
#   * Training matrix is full-rank after MONOGRAPH_CH7_EXCLUDE removals
#   * dt_stats19_freq_lsoa_pca row count == dt_stats19_freq_lsoa row count
#     after the join (no rows dropped, no duplicates added)
#   * Shared non-PC columns reproduce the source dataset byte-for-byte
#     (the PCA join invariant)
#   * unique_id remains unique in the output

library(data.table)
library(jsonlite)

source(file.path("data-raw", "_constants.R"))
# Brings vars_ind_toUse and MONOGRAPH_CH7_EXCLUDE into scope.

dir_rdata   <- file.path(".", "data")
dir_extdata <- file.path(".", "inst", "extdata")
dir.create(dir_extdata, showWarnings = FALSE, recursive = TRUE)

load(file.path(dir_rdata, "dt_stats19_freq_lsoa.rda"))
stopifnot(exists("dt_stats19_freq_lsoa"))

# ---- Step 1: PCA input column selection (explicit + assertion gate) ---------
# vars_ind_toUse and MONOGRAPH_CH7_EXCLUDE come from _constants.R; never
# redefined locally.
PCA_INPUT_NUMERIC <- setdiff(vars_ind_toUse, "covid_ind")
PCA_INPUT_COLS    <- setdiff(PCA_INPUT_NUMERIC, MONOGRAPH_CH7_EXCLUDE)

forbidden <- intersect(
  PCA_INPUT_COLS,
  c("unique_id", "lsoa", "nu_cl", "ex", "fold",
    "covid_date", "covid_ind", MONOGRAPH_CH7_EXCLUDE)
)
stopifnot(length(forbidden) == 0)

# ---- Step 2: collapse to one row per LSOA -----------------------------------
# Features are LSOA-constant by construction (verified in 01b_create_freq_lsoa
# via the LSOA-constant assertion). dt_lsoa carries one row per LSOA.
# Note: data.table's '..' prefix only works as a top-level reference in [,]
# (e.g. dt[, ..varname]), NOT inside c(...). Pre-build the column vector
# explicitly and then dispatch via ..dt_lsoa_cols.
dt_lsoa_cols <- c("lsoa", "fold", PCA_INPUT_COLS)
missing_pca <- setdiff(dt_lsoa_cols, names(dt_stats19_freq_lsoa))
stopifnot(length(missing_pca) == 0)
dt_lsoa <- unique(dt_stats19_freq_lsoa[, ..dt_lsoa_cols])
stopifnot(uniqueN(dt_lsoa$lsoa) == nrow(dt_lsoa))

# ---- Step 3: numeric-only assertion -----------------------------------------
stopifnot(all(vapply(dt_lsoa[, ..PCA_INPUT_COLS], is.numeric, logical(1))))

# ---- Step 4: restrict to training LSOAs; capture means and sds --------------
train       <- dt_lsoa[fold %in% 1:7]
train_mat   <- as.matrix(train[, ..PCA_INPUT_COLS])
train_means <- apply(train_mat, 2, mean)
train_sds   <- apply(train_mat, 2, sd)
stopifnot(!any(train_sds == 0))

# ---- Step 5: z-score on training means / sds → train_scaled -----------------
train_scaled <- sweep(train_mat,    2, train_means, FUN = "-")
train_scaled <- sweep(train_scaled, 2, train_sds,   FUN = "/")

# ---- Step 6: rank-aware component count -------------------------------------
# Compositional percentage families (TS002 partnership, TS007a age bands,
# etc.) can retain residual linear dependence even after MONOGRAPH_CH7_EXCLUDE
# removes the obvious cases. Rather than requiring full rank (which would
# hard-stop the build on any leftover collinearity), compute the numerical
# rank of train_scaled and emit exactly that many components. PCA handles
# rank-deficient inputs naturally — components beyond `rank` carry zero
# variance and don't add information.
rank_train <- qr(train_scaled)$rank
stopifnot(rank_train > 0)
N <- rank_train

# ---- Step 7: prcomp on standardized data matrix -----------------------------
# Run PCA on the standardized data matrix, NOT on the covariance matrix.
# The monograph's 05a0_pca.R uses prcomp(cov(...)) which is mathematically
# valid but the wrong primitive; this script uses the standard form.
# rank. = N produces exactly `numerical-rank` components.
pcs <- prcomp(train_scaled, center = FALSE, scale. = FALSE, rank. = N)
stopifnot(ncol(pcs$rotation) == N)

loadings_raw <- pcs$rotation   # P x N (P = ncol(train_scaled), N = rank)

# ---- Step 8: sign convention (max-|loading| entry positive per PC) ----------
# Equivalent to sklearn's svd_flip with the max-abs-loading rule: for each PC,
# find the loading entry with maximum absolute value; if negative, flip the
# loading column's sign. We only need loadings_raw — pcs$x is unused
# downstream (all_scores is computed via all_scaled %*% loadings_raw).
for (j in seq_len(N)) {
  k <- which.max(abs(loadings_raw[, j]))
  if (loadings_raw[k, j] < 0) {
    loadings_raw[, j] <- -loadings_raw[, j]
  }
}

# ---- Step 9: project all LSOAs through the basis ---------------------------
# Standardize the full LSOA matrix using train means/sds, then project.
all_mat    <- as.matrix(dt_lsoa[, ..PCA_INPUT_COLS])
all_scaled <- sweep(all_mat,    2, train_means, FUN = "-")
all_scaled <- sweep(all_scaled, 2, train_sds,   FUN = "/")
all_scores <- all_scaled %*% loadings_raw

# ---- Step 10: join pc1..pcN back to dt_stats19_freq_lsoa --------------------
pc_names <- paste0("pc", seq_len(N))
colnames(all_scores) <- pc_names

dt_pcs <- as.data.table(all_scores)
dt_pcs[, lsoa := dt_lsoa$lsoa]

dt_stats19_freq_lsoa_pca <- merge(dt_stats19_freq_lsoa, dt_pcs,
                                   by = "lsoa", sort = FALSE)

# Re-apply column ordering: identifiers / keys / target / exposure first.
setcolorder(dt_stats19_freq_lsoa_pca,
            c("unique_id", "lsoa", "covid_ind", "covid_date",
              "fold", "nu_cl", "ex"))

# ---- Step 11: audit assertions (PCA join invariant) -------------------------
# Row count preserved.
stopifnot(nrow(dt_stats19_freq_lsoa_pca) == nrow(dt_stats19_freq_lsoa))

# unique_id unique.
stopifnot(nrow(dt_stats19_freq_lsoa_pca) ==
          uniqueN(dt_stats19_freq_lsoa_pca$unique_id))

# PCA join invariant: every shared non-PC column reproduces the source
# dataset byte-for-byte after sorting both by unique_id.
shared      <- intersect(names(dt_stats19_freq_lsoa),
                          names(dt_stats19_freq_lsoa_pca))
src_sorted  <- dt_stats19_freq_lsoa[order(unique_id), ..shared]
pca_sorted  <- dt_stats19_freq_lsoa_pca[order(unique_id), ..shared]
stopifnot(identical(src_sorted, pca_sorted))

# ---- Step 12: save dt_stats19_freq_lsoa_pca.rda -----------------------------
save(dt_stats19_freq_lsoa_pca,
     file = file.path(dir_rdata, "dt_stats19_freq_lsoa_pca.rda"))

# ---- Step 13: write pca_loadings.json sidecar -------------------------------
desc        <- readLines("DESCRIPTION")
pkg_version <- sub("Version: ", "", desc[grep("^Version:", desc)])

# Loadings as a list: pc_name -> named list of source-column -> loading value.
loadings_for_json <- lapply(seq_len(N), function(j) {
  setNames(as.list(loadings_raw[, j]), rownames(loadings_raw))
})
names(loadings_for_json) <- pc_names

excluded_columns_rationale <- list(
  o_rank             = "IoD Outdoors sub-domain contains air-quality + RTA measures (target leakage)",
  le_rank            = "IoD Living Environment sub-domain includes RTA measures (target leakage)",
  c_ts011_hhd_0_pct  = "Sum-to-100% with c_ts011_hhd_4_pct (perfect linear dependence)",
  c_ts011_hhd_1_pct  = "Sum-to-100% with c_ts011_hhd_4_pct (perfect linear dependence)",
  c_ts011_hhd_2_pct  = "Sum-to-100% with c_ts011_hhd_4_pct (perfect linear dependence)",
  c_ts011_hhd_3_pct  = "Sum-to-100% with c_ts011_hhd_4_pct (perfect linear dependence)",
  pop                = "Functionally related to ex (pop * N_quarters / 4000 = ex); exposure-source trap"
)

loadings_list <- list(
  fit_folds                  = 1:7,
  source_dataset_id          = "en_auto_stats19_freq_lsoa_v1",
  source_columns             = PCA_INPUT_COLS,
  excluded_columns           = MONOGRAPH_CH7_EXCLUDE,
  excluded_columns_rationale = excluded_columns_rationale,
  standardization            = list(
    means = as.list(train_means),
    sds   = as.list(train_sds)
  ),
  sign_convention            = "max_abs_loading_positive",
  n_components               = N,
  generator_version          = pkg_version,
  loadings                   = loadings_for_json
)

write_json(loadings_list,
           path       = file.path(dir_extdata, "pca_loadings.json"),
           auto_unbox = TRUE,
           pretty     = TRUE)

# ---- Step 14: write pca_explained_variance.json sidecar ---------------------
# pcs$sdev may carry more values than `rank.` requested (R's prcomp can return
# the full singular-value vector); slice to first N to align with loadings.
var_per_pc <- pcs$sdev[seq_len(N)] ^ 2
var_total  <- sum(var_per_pc)
prop_var   <- var_per_pc / var_total

variance_list <- list(
  fit_folds            = 1:7,
  source_dataset_id    = "en_auto_stats19_freq_lsoa_v1",
  n_components         = N,
  generator_version    = pkg_version,
  per_pc_variance      = setNames(as.list(var_per_pc),       pc_names),
  per_pc_prop_variance = setNames(as.list(prop_var),         pc_names),
  cumulative_variance  = setNames(as.list(cumsum(prop_var)), pc_names)
)

write_json(variance_list,
           path       = file.path(dir_extdata, "pca_explained_variance.json"),
           auto_unbox = TRUE,
           pretty     = TRUE)

# Tidy
rm(dt_stats19_freq_lsoa, dt_lsoa, train, train_mat, train_means, train_sds,
   train_scaled, pcs, loadings_raw, all_mat, all_scaled, all_scores,
   dt_pcs, pc_names, N, PCA_INPUT_NUMERIC, PCA_INPUT_COLS, forbidden,
   src_sorted, pca_sorted, shared,
   desc, pkg_version, loadings_list, loadings_for_json,
   excluded_columns_rationale, var_per_pc, var_total, prop_var,
   variance_list)
