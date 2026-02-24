rule_r018_future_dated <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", date_vars = character(), cutoff_date = Sys.Date(), owner_team = "DM", ...) {
  if (length(date_vars)) {
    new_flag_rule(
      id         = "R018_FUTURE_DATED",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        cutoff <- as.Date(cutoff_date)
        for (dv in date_vars) {
          if (!(dv %in% names(df))) next
          d   <- suppressWarnings(as.Date(df[[dv]]))
          bad <- !is.na(d) & d > cutoff
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .select_context_columns(df, key_vars, subject_var, group_var, dv), drop = FALSE]
          piece$DATE_VAR   <- dv
          piece$DATE_VALUE <- as.character(d[bad])
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
