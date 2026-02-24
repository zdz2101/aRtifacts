#' Build A4 cross-domain aggregate pack
#'
#' @param owner_team Owner team.
#' @param subject_var Subject variable.
#' @param group_var Group variable.
#' @param subject_count_tolerance Tolerance for XAGG-001.
#' @param visit_count_tolerance Tolerance for XAGG-002.
#' @param completeness_domains Domains for XAGG-003.
#' @param completeness_gap_threshold Gap threshold for XAGG-003.
#' @param ae_high_threshold AE high-rate threshold for XAGG-004.
#' @param ex_low_threshold EX low-rate threshold for XAGG-004.
#'
#' @return List of `CoreRule` objects.
#' @export
pack_core_50_a4 <- function(owner_team = "Standards",
                            subject_var = "USUBJID",
                            group_var = "SITEID",
                            subject_count_tolerance = 0,
                            visit_count_tolerance = 1,
                            completeness_domains = c("VS", "LB", "EG"),
                            completeness_gap_threshold = 0.2,
                            ae_high_threshold = 0.5,
                            ex_low_threshold = 0.5) {
  list(
    rule_xagg_001_subject_count_mismatch(
      left_domain = "DM",
      right_domain = "AE",
      subject_var = subject_var,
      tolerance = subject_count_tolerance,
      owner_team = owner_team
    ),
    rule_xagg_002_visit_count_mismatch(
      left_domain = "VS",
      right_domain = "LB",
      group_var = group_var,
      visit_var = "VISIT",
      tolerance = visit_count_tolerance,
      owner_team = owner_team
    ),
    rule_xagg_003_domain_completeness_imbalance(
      domains_set = completeness_domains,
      group_var = group_var,
      threshold_max_gap = completeness_gap_threshold,
      owner_team = owner_team
    ),
    rule_xagg_004_exposure_event_rate_discrepancy(
      ae_domain = "AE",
      ex_domain = "EX",
      group_var = group_var,
      ae_high_threshold = ae_high_threshold,
      ex_low_threshold = ex_low_threshold,
      owner_team = owner_team
    )
  )
}

#' Register A4 cross-domain aggregate rules
#'
#' @inheritParams pack_core_50_a4
#' @param overwrite Whether to overwrite existing IDs.
#'
#' @return Invisibly returns registered IDs.
#' @export
register_core_50_a4 <- function(owner_team = "Standards",
                                subject_var = "USUBJID",
                                group_var = "SITEID",
                                subject_count_tolerance = 0,
                                visit_count_tolerance = 1,
                                completeness_domains = c("VS", "LB", "EG"),
                                completeness_gap_threshold = 0.2,
                                ae_high_threshold = 0.5,
                                ex_low_threshold = 0.5,
                                overwrite = FALSE) {
  rules <- pack_core_50_a4(
    owner_team = owner_team,
    subject_var = subject_var,
    group_var = group_var,
    subject_count_tolerance = subject_count_tolerance,
    visit_count_tolerance = visit_count_tolerance,
    completeness_domains = completeness_domains,
    completeness_gap_threshold = completeness_gap_threshold,
    ae_high_threshold = ae_high_threshold,
    ex_low_threshold = ex_low_threshold
  )

  ids <- vapply(rules, function(rule) register_rule(rule, overwrite = overwrite), character(1))
  invisible(ids)
}
