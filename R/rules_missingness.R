# ---------------------------------------------------------------------------
# Module 3 — Missingness & Completeness Drift (R021–R027)
#
# R028 (late-arriving data volume spike), R029 (unexpected data plateau),
# and R030 (form completion rate outlier) require cross-snapshot comparison
# or an external completion log and are not implementable in a single-domain
# rule pack.
# ---------------------------------------------------------------------------

#' Rule pack — missingness and completeness
#'
#' Builds executable `FlagRule` objects for the missingness and completeness
#' checks from the core observability rule catalog.
#'
#' | Rule ID | Name | Scope |
#' |---|---|---|
#' | R021 | Overall missingness spike | Snapshot |
#' | R022 | Missingness spike by site | Group |
#' | R023 | Missingness spike by visit/timepoint | Group |
#' | R024 | Required variable missingness > 0 | Snapshot |
#' | R025 | Expected domain empty | Snapshot |
#' | R026 | Subjects without follow-up after baseline | Group |
#' | R027 | New subjects missing baseline assessments | Group |
#'
#' @param domain Domain name targeted by the rules.
#' @param key_vars Character vector of key variables.
#' @param required_vars Character vector of variables that must be present
#'   and non-missing in every record (R024).
#' @param subject_var Subject identifier variable. Default `"USUBJID"`.
#' @param site_var Site identifier variable. Default `"SITEID"`.
#' @param visit_var Optional variable that identifies visit or timepoint
#'   (used by R023).
#' @param baseline_flag_var Variable whose value `"Y"` marks the baseline
#'   record for a subject (used by R026 and R027). Typically `"ABLFL"` or
#'   `"DTYPE"`.
#' @param missingness_threshold Numeric \[0, 1\]. Variables with a missing
#'   rate above this threshold are flagged by R021–R023. Default `0.1`.
#' @param owner_team Team assigned to follow up on flagged records.
#' @param ... Reserved for future extensions; currently ignored.
#'
#' @return A list of `FlagRule` objects compatible with `run_flags()`.
#' @export
#'
#' @examples
#' pack <- make_rule_pack_missingness(
#'   domain        = "ADVS",
#'   key_vars      = c("USUBJID", "PARAMCD", "AVISIT"),
#'   required_vars = c("USUBJID", "PARAMCD", "AVAL"),
#'   baseline_flag_var = "ABLFL"
#' )
#' length(pack)
make_rule_pack_missingness <- function(domain,
                                       key_vars,
                                       required_vars         = character(),
                                       subject_var           = "USUBJID",
                                       site_var              = "SITEID",
                                       visit_var             = NULL,
                                       baseline_flag_var     = NULL,
                                       missingness_threshold = 0.1,
                                       owner_team            = "DM",
                                       ...) {
  rules <- list(
    R021 = rule_r021_missingness_overall(domain = domain, key_vars = key_vars, required_vars = required_vars, subject_var = subject_var, site_var = site_var, visit_var = visit_var, baseline_flag_var = baseline_flag_var, missingness_threshold = missingness_threshold, owner_team = owner_team, ...),
    R022 = rule_r022_missingness_by_site(domain = domain, key_vars = key_vars, required_vars = required_vars, subject_var = subject_var, site_var = site_var, visit_var = visit_var, baseline_flag_var = baseline_flag_var, missingness_threshold = missingness_threshold, owner_team = owner_team, ...),
    R023 = rule_r023_missingness_by_visit(domain = domain, key_vars = key_vars, required_vars = required_vars, subject_var = subject_var, site_var = site_var, visit_var = visit_var, baseline_flag_var = baseline_flag_var, missingness_threshold = missingness_threshold, owner_team = owner_team, ...),
    R024 = rule_r024_required_missing(domain = domain, key_vars = key_vars, required_vars = required_vars, subject_var = subject_var, site_var = site_var, visit_var = visit_var, baseline_flag_var = baseline_flag_var, missingness_threshold = missingness_threshold, owner_team = owner_team, ...),
    R025 = rule_r025_domain_empty(domain = domain, key_vars = key_vars, required_vars = required_vars, subject_var = subject_var, site_var = site_var, visit_var = visit_var, baseline_flag_var = baseline_flag_var, missingness_threshold = missingness_threshold, owner_team = owner_team, ...),
    R026 = rule_r026_no_followup(domain = domain, key_vars = key_vars, required_vars = required_vars, subject_var = subject_var, site_var = site_var, visit_var = visit_var, baseline_flag_var = baseline_flag_var, missingness_threshold = missingness_threshold, owner_team = owner_team, ...),
    R027 = rule_r027_missing_baseline(domain = domain, key_vars = key_vars, required_vars = required_vars, subject_var = subject_var, site_var = site_var, visit_var = visit_var, baseline_flag_var = baseline_flag_var, missingness_threshold = missingness_threshold, owner_team = owner_team, ...),
    R028 = rule_r028_late_arrival_spike(domain = domain, key_vars = key_vars, required_vars = required_vars, subject_var = subject_var, site_var = site_var, visit_var = visit_var, baseline_flag_var = baseline_flag_var, missingness_threshold = missingness_threshold, owner_team = owner_team, ...),
    R029 = rule_r029_data_plateau(domain = domain, key_vars = key_vars, required_vars = required_vars, subject_var = subject_var, site_var = site_var, visit_var = visit_var, baseline_flag_var = baseline_flag_var, missingness_threshold = missingness_threshold, owner_team = owner_team, ...),
    R030 = rule_r030_completion_rate_outlier(domain = domain, key_vars = key_vars, required_vars = required_vars, subject_var = subject_var, site_var = site_var, visit_var = visit_var, baseline_flag_var = baseline_flag_var, missingness_threshold = missingness_threshold, owner_team = owner_team, ...)
  )

  unname(Filter(Negate(is.null), rules))
}
