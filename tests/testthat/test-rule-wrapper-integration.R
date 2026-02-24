module_specs <- list(
  keys = list(
    wrapper = make_rule_pack_keys,
    wrapper_args = list(domain = "X", key_vars = c("ID", "SEQ")),
    rule_fns = c(
      "rule_r001_key_missing",
      "rule_r002_duplicate_keys",
      "rule_r009_high_dup_rate_site",
      "rule_r010_high_dup_rate_subject"
    )
  ),
  dates = list(
    wrapper = make_rule_pack_dates,
    wrapper_args = list(
      domain = "X",
      key_vars = c("ID"),
      start_end_pairs = list(c("STARTDT", "ENDDT")),
      date_vars = c("DT", "STARTDT", "ENDDT"),
      time_date_pairs = list(c("TM", "DT")),
      visit_order_var = "VISITNUM",
      study_start = "2020-01-01",
      study_end = "2025-12-31",
      cutoff_date = as.Date("2024-01-01")
    ),
    rule_fns = c(
      "rule_r011_start_after_end",
      "rule_r012_date_outside_study",
      "rule_r013_date_format",
      "rule_r014_end_without_start",
      "rule_r015_time_without_date",
      "rule_r016_visit_order",
      "rule_r017_identical_timestamps",
      "rule_r018_future_dated",
      "rule_r019_scheduled_gap",
      "rule_r020_timezone_inconsistency"
    )
  ),
  missingness = list(
    wrapper = make_rule_pack_missingness,
    wrapper_args = list(
      domain = "X",
      key_vars = c("ID"),
      required_vars = c("ID", "VAL"),
      baseline_flag_var = "ABLFL",
      visit_var = "VISIT"
    ),
    rule_fns = c(
      "rule_r021_missingness_overall",
      "rule_r022_missingness_by_site",
      "rule_r023_missingness_by_visit",
      "rule_r024_required_missing",
      "rule_r025_domain_empty",
      "rule_r026_no_followup",
      "rule_r027_missing_baseline"
    )
  ),
  plausibility = list(
    wrapper = make_rule_pack_plausibility,
    wrapper_args = list(
      domain = "X",
      key_vars = c("ID"),
      numeric_vars = c("VAL"),
      non_negative_vars = c("VAL"),
      value_ranges = list(VAL = c(0, 100)),
      visit_order_var = "VISITNUM",
      max_within_change = list(VAL = 20)
    ),
    rule_fns = c(
      "rule_r031_impossible_values",
      "rule_r032_unit_mismatch",
      "rule_r033_negative_not_allowed",
      "rule_r034_suspicious_zeros",
      "rule_r035_statistical_outliers",
      "rule_r036_site_distribution_shift",
      "rule_r037_implausible_change",
      "rule_r038_low_variance",
      "rule_r039_rounding_anomaly"
    )
  ),
  consistency = list(
    wrapper = make_rule_pack_consistency,
    wrapper_args = list(
      domain = "X",
      key_vars = c("ID"),
      impossible_combos = list(c(SEX = "M", PREGNANT = "Y")),
      inconsistent_pairs = list(c(A = "Y", B = "N")),
      categorical_vars = c("PARAM"),
      seq_vars = c("SEQ")
    ),
    rule_fns = c(
      "rule_r040_impossible_combos",
      "rule_r041_inconsistent_pairs",
      "rule_r042_derived_inconsistent",
      "rule_r043_placeholder_text",
      "rule_r044_ct_drift",
      "rule_r045_distinct_values_explosion",
      "rule_r046_child_without_parent",
      "rule_r047_seq_gaps",
      "rule_r048_record_count_outlier",
      "rule_r049_enrollment_assessment_mismatch",
      "rule_r050_record_deletions"
    )
  )
)

test_that("module wrappers are integrated with standalone rule constructors", {
  for (module_name in names(module_specs)) {
    spec <- module_specs[[module_name]]

    wrapper_rules <- do.call(spec$wrapper, spec$wrapper_args)

    direct_rules <- lapply(spec$rule_fns, function(rule_fn_name) {
      do.call(get(rule_fn_name, mode = "function"), spec$wrapper_args)
    })
    direct_rules <- Filter(Negate(is.null), direct_rules)

    expect_identical(
      unname(vapply(wrapper_rules, function(rule) rule@id, character(1))),
      unname(vapply(direct_rules, function(rule) rule@id, character(1))),
      info = paste("module", module_name)
    )
  }
})
