# ---------------------------------------------------------------------------
# Module 1 — Identity, Keys & Duplicates (R001, R002, R009, R010)
#
# Rules R003–R008 are Snapshot-scope (they compare two snapshots) so they
# belong in the diff engine, not in a single-domain rule pack.
# ---------------------------------------------------------------------------

#' Rule pack — identity, keys, and duplicates
#'
#' Builds executable `FlagRule` objects for the identity and key-integrity
#' checks from the core observability rule catalog.
#'
#' | Rule ID | Name | Scope |
#' |---|---|---|
#' | R001 | Missing key fields | Record |
#' | R002 | Duplicate keys | Snapshot |
#' | R009 | High duplicate rate by site | Group |
#' | R010 | High duplicate rate by subject | Group |
#'
#' Rules R003–R008 (key-type drift, column additions/removals, column-type
#' drift) are **Snapshot-scope** — they compare two snapshots and therefore
#' belong in the diff engine rather than a single-domain rule pack.
#'
#' @param domain Domain name targeted by the rules.
#' @param key_vars Character vector of key variables.
#' @param subject_var Subject identifier variable. Default `"USUBJID"`.
#' @param site_var Site identifier variable. Default `"SITEID"`.
#' @param dup_rate_threshold Numeric \[0, 1\]. Sites or subjects with a
#'   duplicate-key rate above this threshold are flagged. Default `0.1`.
#' @param owner_team Team assigned to follow up on flagged records.
#' @param ... Reserved for future extensions; currently ignored.
#'
#' @return A list of `FlagRule` objects compatible with `run_flags()`.
#' @export
#'
#' @examples
#' pack <- make_rule_pack_keys(
#'   domain   = "LB",
#'   key_vars = c("STUDYID", "USUBJID", "LBSEQ")
#' )
#' length(pack)
make_rule_pack_keys <- function(domain,
                                key_vars,
                                subject_var       = "USUBJID",
                                site_var          = "SITEID",
                                dup_rate_threshold = 0.1,
                                owner_team        = "DM",
                                ...) {
  rules <- list(
    R001 = rule_r001_key_missing(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, dup_rate_threshold = dup_rate_threshold, owner_team = owner_team, ...),
    R002 = rule_r002_duplicate_keys(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, dup_rate_threshold = dup_rate_threshold, owner_team = owner_team, ...),
    R009 = rule_r009_high_dup_rate_site(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, dup_rate_threshold = dup_rate_threshold, owner_team = owner_team, ...),
    R010 = rule_r010_high_dup_rate_subject(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, dup_rate_threshold = dup_rate_threshold, owner_team = owner_team, ...)
  )

  unname(Filter(Negate(is.null), rules))
}
