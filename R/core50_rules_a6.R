#' Build A6 cross-snapshot aggregate pack
#'
#' @param owner_team Owner team.
#' @param domain_primary Primary domain for A6 checks.
#' @param keys Identity keys for churn checks.
#' @param group_var Grouping variable.
#' @param value_var Numeric value variable for drift checks.
#' @param threshold_row_abs Threshold for SNAP-006 absolute row change.
#' @param threshold_row_rel Threshold for SNAP-006 relative row change.
#' @param churn_threshold Threshold for SNAP-007 churn rate.
#' @param missingness_threshold Threshold for SNAP-008 missingness increase.
#' @param ks_threshold Threshold for SNAP-009 KS drift.
#' @param site_metric_threshold Threshold for SNAP-010 metric drift.
#' @param event_rate_threshold Threshold for SNAP-011 event-rate change.
#'
#' @return List of `CoreRule` objects.
#'
#' @details
#' A6 rules are same-domain aggregate longitudinal checks dispatched as
#' `fn(previous, current, context, rule)`.
#'
#' This pack assembles `SNAP-006` through `SNAP-011` aggregate drift/change
#' checks.
#' @export
pack_core_50_a6 <- function(owner_team = "Clinical Ops",
                            domain_primary = "ADSL",
                            keys = c("USUBJID"),
                            group_var = "SITEID",
                            value_var = "AGE",
                            threshold_row_abs = 100,
                            threshold_row_rel = 0.2,
                            churn_threshold = 0.1,
                            missingness_threshold = 0.1,
                            ks_threshold = 0.2,
                            site_metric_threshold = 5,
                            event_rate_threshold = 0.5) {
  f_snap_006 <- get("rule_snap_006_row_count_spike_drop", mode = "function", inherits = TRUE)
  f_snap_007 <- get("rule_snap_007_key_churn_spike", mode = "function", inherits = TRUE)
  f_snap_008 <- get("rule_snap_008_missingness_rate_drift", mode = "function", inherits = TRUE)
  f_snap_009 <- get("rule_snap_009_distribution_drift", mode = "function", inherits = TRUE)
  f_snap_010 <- get("rule_snap_010_site_metric_drift", mode = "function", inherits = TRUE)
  f_snap_011 <- get("rule_snap_011_event_rate_change_anomaly", mode = "function", inherits = TRUE)

  list(
    f_snap_006(
      domain = domain_primary,
      threshold_abs = threshold_row_abs,
      threshold_rel = threshold_row_rel,
      owner_team = owner_team
    ),
    f_snap_007(
      domain = domain_primary,
      keys = keys,
      churn_threshold = churn_threshold,
      owner_team = owner_team
    ),
    f_snap_008(
      domain = domain_primary,
      group_var = group_var,
      value_var = value_var,
      threshold_increase = missingness_threshold,
      owner_team = owner_team
    ),
    f_snap_009(
      domain = domain_primary,
      value_var = value_var,
      ks_threshold = ks_threshold,
      owner_team = owner_team
    ),
    f_snap_010(
      domain = domain_primary,
      group_var = group_var,
      value_var = value_var,
      threshold_abs_diff = site_metric_threshold,
      owner_team = owner_team
    ),
    f_snap_011(
      domain = "AE",
      group_var = group_var,
      subject_var = "USUBJID",
      threshold_abs_change = event_rate_threshold,
      owner_team = owner_team
    )
  )
}

#' Register A6 cross-snapshot aggregate rules
#'
#' @inheritParams pack_core_50_a6
#' @param overwrite Whether to overwrite existing IDs.
#'
#' @return Invisibly returns registered IDs.
#'
#' @details
#' Equivalent to `pack_core_50_a6(...)` followed by `register_rule()` for each
#' produced rule.
#' @export
register_core_50_a6 <- function(owner_team = "Clinical Ops",
                                domain_primary = "ADSL",
                                keys = c("USUBJID"),
                                group_var = "SITEID",
                                value_var = "AGE",
                                threshold_row_abs = 100,
                                threshold_row_rel = 0.2,
                                churn_threshold = 0.1,
                                missingness_threshold = 0.1,
                                ks_threshold = 0.2,
                                site_metric_threshold = 5,
                                event_rate_threshold = 0.5,
                                overwrite = FALSE) {
  rules <- pack_core_50_a6(
    owner_team = owner_team,
    domain_primary = domain_primary,
    keys = keys,
    group_var = group_var,
    value_var = value_var,
    threshold_row_abs = threshold_row_abs,
    threshold_row_rel = threshold_row_rel,
    churn_threshold = churn_threshold,
    missingness_threshold = missingness_threshold,
    ks_threshold = ks_threshold,
    site_metric_threshold = site_metric_threshold,
    event_rate_threshold = event_rate_threshold
  )

  ids <- vapply(rules, function(rule) register_rule(rule, overwrite = overwrite), character(1))
  invisible(ids)
}
