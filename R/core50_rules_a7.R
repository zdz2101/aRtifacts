#' Build A7 cross-domain + cross-snapshot pack
#'
#' @param owner_team Owner team.
#' @param group_var Grouping variable for group-level checks.
#' @param subject_var Subject identifier variable.
#' @param risk_delta_threshold Threshold for BOTH-004.
#'
#' @return List of `CoreRule` objects.
#' @export
pack_core_50_a7 <- function(owner_team = "Clinical Ops",
                            group_var = "SITEID",
                            subject_var = "USUBJID",
                            risk_delta_threshold = 0.5) {
  f_both_001 <- get("rule_both_001_new_ae_without_new_ex", mode = "function", inherits = TRUE)
  f_both_002 <- get("rule_both_002_dose_changed_lab_worsened", mode = "function", inherits = TRUE)
  f_both_003 <- get("rule_both_003_subject_removed_dm_present_elsewhere", mode = "function", inherits = TRUE)
  f_both_004 <- get("rule_both_004_risk_score_escalation_trend", mode = "function", inherits = TRUE)

  list(
    f_both_001(
      ae_domain = "AE",
      ex_domain = "EX",
      ae_keys = c(subject_var, "AESEQ"),
      match_keys = c(subject_var),
      owner_team = "Clinical Review"
    ),
    f_both_002(
      ex_domain = "EX",
      lb_domain = "LB",
      subject_var = subject_var,
      dose_var = "EXDOSE",
      lab_var = "LBSTRESN",
      dose_delta_min = 0,
      lab_delta_min = 0,
      owner_team = "Clinical Review"
    ),
    f_both_003(
      dm_domain = "DM",
      other_domain = "AE",
      subject_var = subject_var,
      owner_team = "Standards"
    ),
    f_both_004(
      domains_set = c("AE", "EX", "LB"),
      group_var = group_var,
      subject_var = subject_var,
      lb_var = "LBSTRESN",
      lb_high_cutoff = 200,
      risk_delta_threshold = risk_delta_threshold,
      owner_team = owner_team
    )
  )
}

#' Register A7 cross-domain + cross-snapshot rules
#'
#' @inheritParams pack_core_50_a7
#' @param overwrite Whether to overwrite existing IDs.
#'
#' @return Invisibly returns registered IDs.
#' @export
register_core_50_a7 <- function(owner_team = "Clinical Ops",
                                group_var = "SITEID",
                                subject_var = "USUBJID",
                                risk_delta_threshold = 0.5,
                                overwrite = FALSE) {
  rules <- pack_core_50_a7(
    owner_team = owner_team,
    group_var = group_var,
    subject_var = subject_var,
    risk_delta_threshold = risk_delta_threshold
  )

  ids <- vapply(rules, function(rule) register_rule(rule, overwrite = overwrite), character(1))
  invisible(ids)
}
