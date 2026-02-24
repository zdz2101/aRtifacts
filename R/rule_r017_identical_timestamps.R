rule_r017_identical_timestamps <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", date_vars = character(), owner_team = "DM", ...) {
  if (length(date_vars)) {
    new_flag_rule(
      id         = "R017_IDENTICAL_TIMESTAMPS",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (dv in date_vars) {
          if (!(dv %in% names(df))) next
          dup <- duplicated(df[[dv]]) | duplicated(df[[dv]], fromLast = TRUE)
          dup <- dup & !is.na(df[[dv]])
          if (!any(dup)) next
          k <- k + 1L
          piece <- df[dup, .select_context_columns(df, key_vars, subject_var, group_var, dv), drop = FALSE]
          piece$DATE_VAR <- dv
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
