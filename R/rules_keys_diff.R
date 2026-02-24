# ---------------------------------------------------------------------------
# Module 1B (Diff) — Identity, Keys & Duplicates (R003–R008)
# ---------------------------------------------------------------------------

#' Diff rule pack — identity, keys, and schema drift
#'
#' Builds executable `DiffFlagRule` objects for key/schema drift checks that
#' require comparing two snapshots of the same domain.
#'
#' | Rule ID | Name | Scope |
#' |---|---|---|
#' | R003 | Key type drift | Snapshot-diff |
#' | R004 | Unexpected key deletions | Snapshot-diff |
#' | R005 | Unexpected key churn spike | Snapshot-diff |
#' | R006 | New columns added | Snapshot-diff |
#' | R007 | Columns removed | Snapshot-diff |
#' | R008 | Column type drift | Snapshot-diff |
#'
#' @param domain Domain name targeted by the rules.
#' @param key_vars Character vector of key variables.
#' @param subject_var Subject identifier variable. Default `"USUBJID"`.
#' @param group_var Group identifier variable. Default `"SITEID"`.
#' @param dup_rate_threshold Numeric \[0, 1\]. Churn rates above this threshold
#'   are flagged by R005. Default `0.1`.
#' @param owner_team Team assigned to follow up on flagged records.
#' @param ... Reserved for future extensions; currently ignored.
#'
#' @return A list of `DiffFlagRule` objects compatible with `run_diff_flags()`.
#' @export
#'
#' @examples
#' pack <- make_rule_pack_keys_diff(
#'   domain = "LB",
#'   key_vars = c("STUDYID", "USUBJID", "LBSEQ")
#' )
#' length(pack)
make_rule_pack_keys_diff <- function(domain,
                                     key_vars,
                                     subject_var = "USUBJID",
                                     group_var = "SITEID",
                                     dup_rate_threshold = 0.1,
                                     owner_team = "DM",
                                     ...) {
  rules <- list(
    R003 = rule_r003_key_type_drift(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, dup_rate_threshold = dup_rate_threshold, owner_team = owner_team, ...),
    R004 = rule_r004_unexpected_key_deletions(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, dup_rate_threshold = dup_rate_threshold, owner_team = owner_team, ...),
    R005 = rule_r005_key_churn_spike(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, dup_rate_threshold = dup_rate_threshold, owner_team = owner_team, ...),
    R006 = rule_r006_new_columns_added(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, dup_rate_threshold = dup_rate_threshold, owner_team = owner_team, ...),
    R007 = rule_r007_columns_removed(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, dup_rate_threshold = dup_rate_threshold, owner_team = owner_team, ...),
    R008 = rule_r008_column_type_drift(domain = domain, key_vars = key_vars, subject_var = subject_var, group_var = group_var, dup_rate_threshold = dup_rate_threshold, owner_team = owner_team, ...)
  )

  unname(Filter(Negate(is.null), rules))
}
