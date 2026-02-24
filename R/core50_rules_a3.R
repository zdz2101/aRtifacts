#' Build A3 cross-domain relational pack
#'
#' @param owner_team Owner team.
#' @param lb_visit_var Visit variable in LB.
#' @param vs_visit_var Visit variable in VS.
#' @param cm_window_days Window days for CM vs AE.
#' @param ae_ex_window_days Window days for AE vs EX.
#' @param orphan_child_domain Child domain for orphan check.
#' @param orphan_parent_domain Parent domain for orphan check.
#' @param orphan_parent_keys Keys for orphan check.
#' @param demo_left_domain Left domain for demographic consistency.
#' @param demo_right_domain Right domain for demographic consistency.
#' @param demo_key_var Join key for demographic consistency.
#' @param demo_left_field Left field for demographic consistency.
#' @param demo_right_field Right field for demographic consistency.
#'
#' @return A list of `CoreRule` objects.
#' @export
pack_core_50_a3 <- function(owner_team = "DM",
                            lb_visit_var = "VISIT",
                            vs_visit_var = "VISIT",
                            cm_window_days = 7,
                            ae_ex_window_days = 0,
                            orphan_child_domain = "SUPPAE",
                            orphan_parent_domain = "AE",
                            orphan_parent_keys = c("USUBJID", "AESEQ"),
                            demo_left_domain = "DM",
                            demo_right_domain = "VS",
                            demo_key_var = "USUBJID",
                            demo_left_field = "SEX",
                            demo_right_field = "SEX") {
  list(
    rule_xdom_001_ae_without_dm(owner_team = owner_team),
    rule_xdom_002_ex_without_dm(owner_team = owner_team),
    rule_xdom_003_lb_without_expected_visit(
      lb_visit_var = lb_visit_var,
      vs_visit_var = vs_visit_var,
      owner_team = owner_team
    ),
    rule_xdom_004_cm_outside_ae_window(
      window_days = cm_window_days,
      owner_team = owner_team
    ),
    rule_xdom_005_ae_outside_exposure_window(
      window_days = ae_ex_window_days,
      owner_team = owner_team
    ),
    rule_xdom_006_visit_mismatch_across_domains(
      left_domain = "VS",
      right_domain = "LB",
      subject_var = "USUBJID",
      left_visit_var = vs_visit_var,
      right_visit_var = lb_visit_var,
      owner_team = owner_team
    ),
    rule_xdom_007_orphan_child_records(
      child_domain = orphan_child_domain,
      parent_domain = orphan_parent_domain,
      parent_keys = orphan_parent_keys,
      owner_team = owner_team
    ),
    rule_xdom_008_demographic_inconsistency(
      left_domain = demo_left_domain,
      right_domain = demo_right_domain,
      key_var = demo_key_var,
      left_field = demo_left_field,
      right_field = demo_right_field,
      owner_team = owner_team
    )
  )
}

#' Register A3 cross-domain relational rules
#'
#' @inheritParams pack_core_50_a3
#' @param overwrite Whether to overwrite existing IDs.
#'
#' @return Invisibly returns registered IDs.
#' @export
register_core_50_a3 <- function(owner_team = "DM",
                                lb_visit_var = "VISIT",
                                vs_visit_var = "VISIT",
                                cm_window_days = 7,
                                ae_ex_window_days = 0,
                                orphan_child_domain = "SUPPAE",
                                orphan_parent_domain = "AE",
                                orphan_parent_keys = c("USUBJID", "AESEQ"),
                                demo_left_domain = "DM",
                                demo_right_domain = "VS",
                                demo_key_var = "USUBJID",
                                demo_left_field = "SEX",
                                demo_right_field = "SEX",
                                overwrite = FALSE) {
  rules <- pack_core_50_a3(
    owner_team = owner_team,
    lb_visit_var = lb_visit_var,
    vs_visit_var = vs_visit_var,
    cm_window_days = cm_window_days,
    ae_ex_window_days = ae_ex_window_days,
    orphan_child_domain = orphan_child_domain,
    orphan_parent_domain = orphan_parent_domain,
    orphan_parent_keys = orphan_parent_keys,
    demo_left_domain = demo_left_domain,
    demo_right_domain = demo_right_domain,
    demo_key_var = demo_key_var,
    demo_left_field = demo_left_field,
    demo_right_field = demo_right_field
  )

  ids <- vapply(rules, function(rule) register_rule(rule, overwrite = overwrite), character(1))
  invisible(ids)
}
