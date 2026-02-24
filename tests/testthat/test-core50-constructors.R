test_that("Core50 exported rule constructors return CoreRule with expected IDs", {
  default_arg_value <- function(arg_name) {
    if (grepl("domains$", arg_name)) {
      return(c("ADSL", "DM"))
    }
    if (grepl("domain", arg_name)) {
      if (grepl("_b$|secondary|right|parent", arg_name)) {
        return("DM")
      }
      return("ADSL")
    }
    if (grepl("keys$|key_vars$", arg_name)) {
      return(c("STUDYID", "USUBJID"))
    }
    if (grepl("_var$|_by$|_field$", arg_name)) {
      return("USUBJID")
    }
    if (grepl("_vars$|_fields$", arg_name)) {
      return(c("USUBJID"))
    }
    if (grepl("levels$", arg_name)) {
      return(c("M", "F"))
    }
    if (grepl("threshold|tol|rate|k$|window|days|multiplier|min|max", arg_name)) {
      return(1)
    }
    1
  }

  build_args <- function(fn, explicit = list()) {
    fn_formals <- formals(fn)
    required <- names(fn_formals)[vapply(fn_formals, function(x) identical(x, quote(expr = )), logical(1))]
    out <- explicit
    for (arg_name in required) {
      if (!(arg_name %in% names(out))) {
        out[[arg_name]] <- default_arg_value(arg_name)
      }
    }
    out
  }

  constructors <- list(
    list(fn = rule_sch_001_new_columns_added, id = "SCH-001", args = list(domain = "ADSL")),
    list(fn = rule_sch_002_columns_removed, id = "SCH-002", args = list(domain = "ADSL")),
    list(fn = rule_sch_003_column_type_drift, id = "SCH-003", args = list(domain = "ADSL")),
    list(fn = rule_id_001_missing_key_fields, id = "ID-001"),
    list(fn = rule_tmp_001_invalid_date_format, id = "TMP-001"),
    list(fn = rule_tmp_002_start_after_end, id = "TMP-002"),
    list(fn = rule_tmp_003_end_without_start, id = "TMP-003"),
    list(fn = rule_tmp_004_time_without_date, id = "TMP-004"),
    list(fn = rule_de_001_required_field_missing_conditionally, id = "DE-001"),
    list(fn = rule_de_002_out_of_range_numeric_values, id = "DE-002"),
    list(fn = rule_de_003_invalid_categorical_code, id = "DE-003"),
    list(fn = rule_de_004_negative_duration, id = "DE-004"),
    list(fn = rule_de_005_impossible_logical_combination, id = "DE-005"),
    list(fn = rule_id_002_duplicate_keys, id = "ID-002"),
    list(fn = rule_id_003_high_duplicate_rate_by_site, id = "ID-003"),
    list(fn = rule_id_004_high_duplicate_rate_by_subject, id = "ID-004"),
    list(fn = rule_grp_001_excessive_missingness_rate, id = "GRP-001"),
    list(fn = rule_grp_002_subject_excessive_event_frequency, id = "GRP-002"),
    list(fn = rule_grp_003_repeated_identical_lab_values, id = "GRP-003"),
    list(fn = rule_grp_004_site_level_outlier_rate, id = "GRP-004"),
    list(fn = rule_grp_005_distribution_skew_anomaly, id = "GRP-005"),
    list(fn = rule_xdom_001_ae_without_dm, id = "XDOM-001"),
    list(fn = rule_xdom_002_ex_without_dm, id = "XDOM-002"),
    list(fn = rule_xdom_003_lb_without_expected_visit, id = "XDOM-003"),
    list(fn = rule_xdom_004_cm_outside_ae_window, id = "XDOM-004"),
    list(fn = rule_xdom_005_ae_outside_exposure_window, id = "XDOM-005"),
    list(fn = rule_xdom_006_visit_mismatch_across_domains, id = "XDOM-006"),
    list(fn = rule_xdom_007_orphan_child_records, id = "XDOM-007"),
    list(fn = rule_xdom_008_demographic_inconsistency, id = "XDOM-008"),
    list(fn = rule_xagg_001_subject_count_mismatch, id = "XAGG-001"),
    list(fn = rule_xagg_002_visit_count_mismatch, id = "XAGG-002"),
    list(fn = rule_xagg_003_domain_completeness_imbalance, id = "XAGG-003"),
    list(fn = rule_xagg_004_exposure_event_rate_discrepancy, id = "XAGG-004"),
    list(fn = rule_snap_001_unexpected_key_deletions, id = "SNAP-001"),
    list(fn = rule_snap_002_record_changed_unexpectedly, id = "SNAP-002"),
    list(fn = rule_snap_003_record_reappeared, id = "SNAP-003"),
    list(fn = rule_snap_004_identity_drift, id = "SNAP-004"),
    list(fn = rule_snap_005_critical_field_modified_post_lock, id = "SNAP-005"),
    list(fn = rule_snap_006_row_count_spike_drop, id = "SNAP-006"),
    list(fn = rule_snap_007_key_churn_spike, id = "SNAP-007"),
    list(fn = rule_snap_008_missingness_rate_drift, id = "SNAP-008"),
    list(fn = rule_snap_009_distribution_drift, id = "SNAP-009"),
    list(fn = rule_snap_010_site_metric_drift, id = "SNAP-010"),
    list(fn = rule_snap_011_event_rate_change_anomaly, id = "SNAP-011"),
    list(fn = rule_both_001_new_ae_without_new_ex, id = "BOTH-001"),
    list(fn = rule_both_002_dose_changed_lab_worsened, id = "BOTH-002"),
    list(fn = rule_both_003_subject_removed_dm_present_elsewhere, id = "BOTH-003"),
    list(fn = rule_both_004_risk_score_escalation_trend, id = "BOTH-004")
  )

  for (item in constructors) {
    args <- build_args(item$fn, if (is.null(item$args)) list() else item$args)
    rule <- do.call(item$fn, args)
    expect_true(S7::S7_inherits(rule, CoreRule))
    expect_identical(rule@id, item$id)
  }
})
