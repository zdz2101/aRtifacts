#' Build A2 intra-domain group pack
#'
#' @param domain Domain name.
#' @param key_vars Key variables for duplicate checks.
#' @param site_var Site/group variable.
#' @param subject_var Subject variable.
#' @param dup_rate_threshold Threshold for duplicate-rate rules.
#' @param missing_vars Variables for missingness checks.
#' @param missingness_threshold Threshold for missingness checks.
#' @param max_events Maximum events per subject.
#' @param lab_test_var Lab test variable for GRP-003.
#' @param lab_value_var Lab value variable for GRP-003.
#' @param min_lab_repeats Minimum repeats for GRP-003.
#' @param event_flag_var Optional event flag for GRP-004.
#' @param outlier_k Robust z-score threshold for GRP-004.
#' @param skew_value_var Numeric variable for GRP-005.
#' @param skew_threshold Absolute skewness threshold for GRP-005.
#' @param owner_team Owner team.
#'
#' @return List of `CoreRule` objects.
#' @export
pack_core_50_a2 <- function(domain,
                            key_vars,
                            site_var = "SITEID",
                            subject_var = "USUBJID",
                            dup_rate_threshold = 0.1,
                            missing_vars = character(),
                            missingness_threshold = 0.1,
                            max_events = 20,
                            lab_test_var = "LBTESTCD",
                            lab_value_var = "LBSTRESN",
                            min_lab_repeats = 3,
                            event_flag_var = NULL,
                            outlier_k = 3,
                            skew_value_var = NULL,
                            skew_threshold = 2,
                            owner_team = "DM") {
  grp003_ctor <- get("rule_grp_003_repeated_identical_lab_values", mode = "function", inherits = TRUE)
  grp004_ctor <- get("rule_grp_004_site_level_outlier_rate", mode = "function", inherits = TRUE)

  rules <- list(
    rule_id_002_duplicate_keys(
      domain = domain,
      key_vars = key_vars,
      entity_key_var = subject_var,
      owner_team = owner_team
    ),
    rule_id_003_high_duplicate_rate_by_site(
      domain = domain,
      key_vars = key_vars,
      site_var = site_var,
      threshold = dup_rate_threshold,
      entity_key_var = subject_var,
      owner_team = owner_team
    ),
    rule_id_004_high_duplicate_rate_by_subject(
      domain = domain,
      key_vars = key_vars,
      subject_var = subject_var,
      threshold = dup_rate_threshold,
      owner_team = owner_team
    ),
    rule_grp_001_excessive_missingness_rate(
      domain = domain,
      target_vars = missing_vars,
      group_var = site_var,
      threshold = missingness_threshold,
      owner_team = owner_team
    ),
    rule_grp_002_subject_excessive_event_frequency(
      domain = domain,
      subject_var = subject_var,
      max_events = max_events,
      owner_team = owner_team
    ),
    grp003_ctor(
      domain = domain,
      subject_var = subject_var,
      test_var = lab_test_var,
      value_var = lab_value_var,
      min_repeats = min_lab_repeats,
      owner_team = owner_team
    ),
    grp004_ctor(
      domain = domain,
      site_var = site_var,
      event_flag_var = event_flag_var,
      k = outlier_k,
      owner_team = owner_team
    )
  )

  if (!is.null(skew_value_var)) {
    rules[[length(rules) + 1L]] <- rule_grp_005_distribution_skew_anomaly(
      domain = domain,
      value_var = skew_value_var,
      threshold = skew_threshold,
      owner_team = owner_team
    )
  }

  rules
}

#' Register A2 intra-domain group rules
#'
#' @inheritParams pack_core_50_a2
#' @param overwrite Whether to overwrite existing IDs.
#'
#' @return Invisibly returns registered IDs.
#' @export
register_core_50_a2 <- function(domain,
                                key_vars,
                                site_var = "SITEID",
                                subject_var = "USUBJID",
                                dup_rate_threshold = 0.1,
                                missing_vars = character(),
                                missingness_threshold = 0.1,
                                max_events = 20,
                                lab_test_var = "LBTESTCD",
                                lab_value_var = "LBSTRESN",
                                min_lab_repeats = 3,
                                event_flag_var = NULL,
                                outlier_k = 3,
                                skew_value_var = NULL,
                                skew_threshold = 2,
                                owner_team = "DM",
                                overwrite = FALSE) {
  rules <- pack_core_50_a2(
    domain = domain,
    key_vars = key_vars,
    site_var = site_var,
    subject_var = subject_var,
    dup_rate_threshold = dup_rate_threshold,
    missing_vars = missing_vars,
    missingness_threshold = missingness_threshold,
    max_events = max_events,
    lab_test_var = lab_test_var,
    lab_value_var = lab_value_var,
    min_lab_repeats = min_lab_repeats,
    event_flag_var = event_flag_var,
    outlier_k = outlier_k,
    skew_value_var = skew_value_var,
    skew_threshold = skew_threshold,
    owner_team = owner_team
  )

  ids <- vapply(rules, function(rule) register_rule(rule, overwrite = overwrite), character(1))
  invisible(ids)
}
