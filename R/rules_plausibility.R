# ---------------------------------------------------------------------------
# Module 4 — Plausibility & Range Checks (R031–R039)
#
# R032 (unit mismatch heuristic) requires a controlled terminology reference
# mapping units to parameters — outside the scope of a single-domain rule
# pack.
# R036 (site-level distribution shift) requires a historical baseline
# distribution and is Snapshot-scope across two data cuts.
# Both are noted in the catalog but not implemented here.
# ---------------------------------------------------------------------------

#' Rule pack — plausibility and range checks
#'
#' Builds executable `FlagRule` objects for the plausibility and value-range
#' checks from the core observability rule catalog.
#'
#' | Rule ID | Name | Scope |
#' |---|---|---|
#' | R031 | Physiologically impossible values | Record |
#' | R033 | Negative values where not allowed | Record |
#' | R034 | Suspicious zero values | Group |
#' | R035 | Extreme statistical outliers (IQR) | Record |
#' | R037 | Within-subject implausible change | Group |
#' | R038 | Too-perfect data (low variance / CV) | Group |
#' | R039 | Rounding pattern anomaly | Group |
#'
#' @param domain Domain name targeted by the rules.
#' @param key_vars Character vector of key variables.
#' @param subject_var Subject identifier variable. Default `"USUBJID"`.
#' @param group_var Group identifier variable. Default `"SITEID"`.
#' @param numeric_vars Character vector of numeric variables to include in
#'   the statistical checks (R034, R035, R037, R038, R039).
#' @param non_negative_vars Variables that must not contain negative values
#'   (R033).
#' @param value_ranges Named list of two-element numeric vectors specifying
#'   hard `c(min, max)` bounds for R031 (e.g.
#'   `list(AGE = c(0, 120), WEIGHT = c(1, 600))`).
#' @param visit_order_var Numeric variable used to order records within a
#'   subject for R037 (e.g. `"ADY"` or `"VISITNUM"`).
#' @param max_within_change Named list of maximum permitted consecutive
#'   change per numeric variable for R037 (e.g. `list(AVAL = 100)`).
#'   Variables absent from this list are skipped.
#' @param iqr_multiplier Multiplier applied to the IQR to define the outer
#'   fence for R035. Default `3.0` (Tukey far-out rule).
#' @param zero_rate_threshold Fraction of non-missing values that are zero
#'   above which R034 fires. Default `0.5`.
#' @param low_cv_threshold Coefficient of variation below which R038 flags
#'   suspiciously low variance. Default `0.01`.
#' @param round_rate_threshold Fraction of values divisible by 5 above
#'   which R039 fires. Default `0.9`.
#' @param owner_team Team assigned to follow up on flagged records.
#' @param ... Reserved for future extensions; currently ignored.
#'
#' @return A list of `FlagRule` objects compatible with `run_flags()`.
#' @export
#'
#' @examples
#' pack <- make_rule_pack_plausibility(
#'   domain           = "ADVS",
#'   key_vars         = c("USUBJID", "PARAMCD", "AVISIT"),
#'   numeric_vars     = "AVAL",
#'   non_negative_vars = "AVAL",
#'   value_ranges     = list(AVAL = c(40, 300))
#' )
#' length(pack)
make_rule_pack_plausibility <- function(domain,
                                        key_vars,
                                        subject_var        = "USUBJID",
                                        group_var           = "SITEID",
                                        numeric_vars       = character(),
                                        non_negative_vars  = character(),
                                        value_ranges       = list(),
                                        visit_order_var    = NULL,
                                        max_within_change  = list(),
                                        iqr_multiplier     = 3.0,
                                        zero_rate_threshold = 0.5,
                                        low_cv_threshold   = 0.01,
                                        round_rate_threshold = 0.9,
                                        owner_team         = "Biostats",
                                        ...) {
  rules <- list(
    R031 = rule_r031_impossible_values(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, numeric_vars = numeric_vars, non_negative_vars = non_negative_vars, value_ranges = value_ranges, visit_order_var = visit_order_var, max_within_change = max_within_change, iqr_multiplier = iqr_multiplier, zero_rate_threshold = zero_rate_threshold, low_cv_threshold = low_cv_threshold, round_rate_threshold = round_rate_threshold, owner_team = owner_team, ...),
    R032 = rule_r032_unit_mismatch(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, numeric_vars = numeric_vars, non_negative_vars = non_negative_vars, value_ranges = value_ranges, visit_order_var = visit_order_var, max_within_change = max_within_change, iqr_multiplier = iqr_multiplier, zero_rate_threshold = zero_rate_threshold, low_cv_threshold = low_cv_threshold, round_rate_threshold = round_rate_threshold, owner_team = owner_team, ...),
    R033 = rule_r033_negative_not_allowed(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, numeric_vars = numeric_vars, non_negative_vars = non_negative_vars, value_ranges = value_ranges, visit_order_var = visit_order_var, max_within_change = max_within_change, iqr_multiplier = iqr_multiplier, zero_rate_threshold = zero_rate_threshold, low_cv_threshold = low_cv_threshold, round_rate_threshold = round_rate_threshold, owner_team = owner_team, ...),
    R034 = rule_r034_suspicious_zeros(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, numeric_vars = numeric_vars, non_negative_vars = non_negative_vars, value_ranges = value_ranges, visit_order_var = visit_order_var, max_within_change = max_within_change, iqr_multiplier = iqr_multiplier, zero_rate_threshold = zero_rate_threshold, low_cv_threshold = low_cv_threshold, round_rate_threshold = round_rate_threshold, owner_team = owner_team, ...),
    R035 = rule_r035_statistical_outliers(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, numeric_vars = numeric_vars, non_negative_vars = non_negative_vars, value_ranges = value_ranges, visit_order_var = visit_order_var, max_within_change = max_within_change, iqr_multiplier = iqr_multiplier, zero_rate_threshold = zero_rate_threshold, low_cv_threshold = low_cv_threshold, round_rate_threshold = round_rate_threshold, owner_team = owner_team, ...),
    R036 = rule_r036_site_distribution_shift(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, numeric_vars = numeric_vars, non_negative_vars = non_negative_vars, value_ranges = value_ranges, visit_order_var = visit_order_var, max_within_change = max_within_change, iqr_multiplier = iqr_multiplier, zero_rate_threshold = zero_rate_threshold, low_cv_threshold = low_cv_threshold, round_rate_threshold = round_rate_threshold, owner_team = owner_team, ...),
    R037 = rule_r037_implausible_change(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, numeric_vars = numeric_vars, non_negative_vars = non_negative_vars, value_ranges = value_ranges, visit_order_var = visit_order_var, max_within_change = max_within_change, iqr_multiplier = iqr_multiplier, zero_rate_threshold = zero_rate_threshold, low_cv_threshold = low_cv_threshold, round_rate_threshold = round_rate_threshold, owner_team = owner_team, ...),
    R038 = rule_r038_low_variance(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, numeric_vars = numeric_vars, non_negative_vars = non_negative_vars, value_ranges = value_ranges, visit_order_var = visit_order_var, max_within_change = max_within_change, iqr_multiplier = iqr_multiplier, zero_rate_threshold = zero_rate_threshold, low_cv_threshold = low_cv_threshold, round_rate_threshold = round_rate_threshold, owner_team = owner_team, ...),
    R039 = rule_r039_rounding_anomaly(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, numeric_vars = numeric_vars, non_negative_vars = non_negative_vars, value_ranges = value_ranges, visit_order_var = visit_order_var, max_within_change = max_within_change, iqr_multiplier = iqr_multiplier, zero_rate_threshold = zero_rate_threshold, low_cv_threshold = low_cv_threshold, round_rate_threshold = round_rate_threshold, owner_team = owner_team, ...)
  )

  Filter(Negate(is.null), rules)
}
