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
  .context_cols <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  list(

    # R001 — Missing key fields -----------------------------------------------
    new_flag_rule(
      id         = "R001_KEY_MISSING",
      domain     = domain,
      severity   = "critical",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        present <- intersect(key_vars, names(df))
        if (!length(present)) return(data.frame())
        miss <- rowSums(
          is.na(df[, present, drop = FALSE]) |
            df[, present, drop = FALSE] == ""
        ) > 0
        if (!any(miss)) return(data.frame())
        df[miss, .context_cols(df), drop = FALSE]
      }
    ),

    # R002 — Duplicate keys ---------------------------------------------------
    new_flag_rule(
      id         = "R002_DUPLICATE_KEYS",
      domain     = domain,
      severity   = "critical",
      category   = "programming_etl",
      owner_team = owner_team,
      fn = function(df) {
        present <- intersect(key_vars, names(df))
        if (!length(present)) return(data.frame())
        dup <- duplicated(df[, present, drop = FALSE]) |
          duplicated(df[, present, drop = FALSE], fromLast = TRUE)
        if (!any(dup)) return(data.frame())
        df[dup, .context_cols(df), drop = FALSE]
      }
    ),

    # R009 — High duplicate rate by site ------------------------------------
    new_flag_rule(
      id         = "R009_HIGH_DUP_RATE_SITE",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        if (!(site_var %in% names(df))) return(data.frame())
        present <- intersect(key_vars, names(df))
        if (!length(present)) return(data.frame())
        dup <- duplicated(df[, present, drop = FALSE]) |
          duplicated(df[, present, drop = FALSE], fromLast = TRUE)
        by_site <- tapply(dup, df[[site_var]], mean, na.rm = TRUE)
        bad <- names(by_site)[!is.na(by_site) & by_site > dup_rate_threshold]
        if (!length(bad)) return(data.frame())
        out <- df[dup & df[[site_var]] %in% bad, .context_cols(df), drop = FALSE]
        out$DUP_RATE <- by_site[out[[site_var]]]
        out
      }
    ),

    # R010 — High duplicate rate by subject ---------------------------------
    new_flag_rule(
      id         = "R010_HIGH_DUP_RATE_SUBJECT",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        if (!(subject_var %in% names(df))) return(data.frame())
        present <- intersect(key_vars, names(df))
        if (!length(present)) return(data.frame())
        dup <- duplicated(df[, present, drop = FALSE]) |
          duplicated(df[, present, drop = FALSE], fromLast = TRUE)
        by_subj <- tapply(dup, df[[subject_var]], mean, na.rm = TRUE)
        bad <- names(by_subj)[!is.na(by_subj) & by_subj > dup_rate_threshold]
        if (!length(bad)) return(data.frame())
        out <- df[dup & df[[subject_var]] %in% bad, .context_cols(df), drop = FALSE]
        out$DUP_RATE <- by_subj[out[[subject_var]]]
        out
      }
    )
  )
}
