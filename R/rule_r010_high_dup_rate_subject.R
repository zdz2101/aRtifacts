#' Rule R010 — high duplicate rate by subject
#'
#' Builds a single-snapshot `FlagRule` that flags duplicated rows for subjects
#' where duplicate-key rate exceeds `dup_rate_threshold`.
#'
#' @inheritParams make_rule_pack_keys
#' @return A `FlagRule` object.
rule_r010_high_dup_rate_subject <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", dup_rate_threshold = 0.1, owner_team = "DM", ...) {
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
        out <- df[dup & df[[subject_var]] %in% bad, .select_context_columns(df, key_vars, subject_var, group_var), drop = FALSE]
        out$DUP_RATE <- by_subj[out[[subject_var]]]
        out
      }
    )
}
