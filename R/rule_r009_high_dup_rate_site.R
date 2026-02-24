rule_r009_high_dup_rate_site <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", dup_rate_threshold = 0.1, owner_team = "DM", ...) {
new_flag_rule(
      id         = "R009_HIGH_DUP_RATE_SITE",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        if (!(group_var %in% names(df))) return(data.frame())
        present <- intersect(key_vars, names(df))
        if (!length(present)) return(data.frame())
        dup <- duplicated(df[, present, drop = FALSE]) |
          duplicated(df[, present, drop = FALSE], fromLast = TRUE)
        by_site <- tapply(dup, df[[group_var]], mean, na.rm = TRUE)
        bad <- names(by_site)[!is.na(by_site) & by_site > dup_rate_threshold]
        if (!length(bad)) return(data.frame())
        out <- df[dup & df[[group_var]] %in% bad, .select_context_columns(df, key_vars, subject_var, group_var), drop = FALSE]
        out$DUP_RATE <- by_site[out[[group_var]]]
        out
      }
    )
}
