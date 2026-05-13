# stats19lsoa/data-raw/_constants.R
#
# Build-time constants for the stats19lsoa data-raw scripts.
# Sourced by both 01a_create_data.R and 01b_build_pca.R.
#
# Single source of truth for:
#   * vars_ind_toUse       : canonical curated feature list
#                            (mirrors stats19_analysis/RCode/03a_EDA.R:152-164)
#   * MONOGRAPH_CH7_EXCLUDE : columns excluded from the PCA basis and from
#                            Lab 7's Ridge/LASSO fits. The columns themselves
#                            remain in dt_stats19_freq_lsoa for pedagogical
#                            reasons (Lab 7 teaches their identification and
#                            exclusion).

# ---- curated feature list ---------------------------------------------------

vars_time      <- "covid_ind"
vars_popdens   <- c("popdens", "area")

vars_dep_file2 <- c("imd_rank",
                    "income_rank",
                    "employment_rank",
                    "est_rank", "hd_rank",
                    "crime_rank",
                    "bhs_rank",
                    "le_rank")

vars_dep_file4 <- c("cayp_rank", "as_rank", "gb_rank", "wb_rank",
                    "i_rank", "o_rank")

vars_ts001 <- "c_ts001_restype_ce_pct"

vars_ts002 <- c("c_ts002_mcp_tot_pct",  "c_ts002_mcp_never_pct",
                "c_ts002_mcp_mrcp_pct", "c_ts002_mcp_m_pct",
                "c_ts002_mcp_mos_pct",  "c_ts002_mcp_mss_pct",
                "c_ts002_mcp_rcp_pct",  "c_ts002_mcp_rcpos_pct",
                "c_ts002_mcp_rcpss_pct","c_ts002_mcp_slmcp_pct",
                "c_ts002_mcp_slm_pct",  "c_ts002_mcp_slcp_pct",
                "c_ts002_mcp_dcpd_pct", "c_ts002_mcp_d_pct",
                "c_ts002_mcp_cpd_pct",  "c_ts002_mcp_wscp_pct",
                "c_ts002_mcp_w_pct",    "c_ts002_mcp_scp_pct")

# Note: 03a_EDA.R creates _pct versions of c_ts003_hc_sfhmcp, sfhmcpdc, and
# sfhmcpacnd just above the vars_ts003 definition, but vars_ind_toUse
# intentionally keeps the raw count columns listed here.
vars_ts003 <- c("c_ts003_hc_oph_pct",
                "c_ts003_hc_sfh_pct",
                "c_ts003_hc_oht_pct",
                "c_ts003_hc_sfhmcp",
                "c_ts003_hc_sfhmcpdc",
                "c_ts003_hc_sfhmcpacnd")

vars_ts004 <- c("c_ts004_cob_uk_pct", "c_ts004_cob_eu_pct",
                "c_ts004_cob_oth_pct")

# Note: 03a_EDA.R creates a _pct version of c_ts007a_age_70_74 just above the
# vars_ts007a definition, but vars_ind_toUse intentionally keeps the raw count
# column listed here.
vars_ts007a <- c("c_ts007a_age_00_14_pct",
                 "c_ts007a_age_15_29_pct",
                 "c_ts007a_age_30_64_pct",
                 "c_ts007a_age_65_pct",
                 "c_ts007a_age_70_74")

vars_ts008 <- "c_ts008_sex_f_pct"

vars_ts011 <- c("c_ts011_hhd_0_pct", "c_ts011_hhd_1_pct",
                "c_ts011_hhd_2_pct", "c_ts011_hhd_3_pct")

vars_ts058 <- c("c_ts058_dtw_00_01_pct", "c_ts058_dtw_02_04_pct",
                "c_ts058_dtw_05_09_pct", "c_ts058_dtw_10_19_pct",
                "c_ts058_dtw_20__pct",   "c_ts058_dtw_home_pct",
                "c_ts058_dtw_offshore_pct")

vars_ts063 <- c("c_ts063_occ_1_pct", "c_ts063_occ_2_pct",
                "c_ts063_occ_3_pct", "c_ts063_occ_4_pct",
                "c_ts063_occ_5_pct", "c_ts063_occ_6_pct",
                "c_ts063_occ_7_pct", "c_ts063_occ_8_pct",
                "c_ts063_occ_9_pct")

# Trap columns that are not in the monograph's vars_ind_toUse but are
# deliberately retained in the curated dataset for pedagogical reasons
# (see MONOGRAPH_CH7_EXCLUDE below for the rationale). Including them in
# vars_ind_toUse makes the trap-column retention explicit and makes
# MONOGRAPH_CH7_EXCLUDE's reference to 'pop' mechanically consistent.
vars_trap_extra <- "pop"

vars_ind_toUse <- c(vars_time,
                    vars_popdens,
                    vars_dep_file2,
                    vars_dep_file4,
                    vars_ts001,
                    vars_ts002,
                    vars_ts003,
                    vars_ts004,
                    vars_ts007a,
                    vars_ts008,
                    vars_ts011,
                    vars_ts058,
                    vars_ts063,
                    vars_trap_extra)

# Guard: no duplicates in the curated feature list
stopifnot(!anyDuplicated(vars_ind_toUse))

# ---- MONOGRAPH_CH7_EXCLUDE --------------------------------------------------
#
#   o_rank, le_rank          : IoD sub-domains that contain
#                              road-traffic-accident measures (target leakage)
#   c_ts011_hhd_0_pct..3_pct : sum-to-100% with the unused _4_pct column
#                              (perfect linear dependence)
#   pop                      : functionally related to ex
#                              (pop * N_quarters / 4000 = ex)
#
# Applied at fit time, not at dataset-build time. See the build plan for
# the actuarial rationale per column.

MONOGRAPH_CH7_EXCLUDE <- c("o_rank", "le_rank",
                           "c_ts011_hhd_0_pct", "c_ts011_hhd_1_pct",
                           "c_ts011_hhd_2_pct", "c_ts011_hhd_3_pct",
                           "pop")
