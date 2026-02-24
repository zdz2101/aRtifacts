rule_r033_negative_not_allowed <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", non_negative_vars = character(), owner_team = "Biostats", ...) {
  if (length(non_negative_vars)) {
    new_flag_rule(
      id         = "R033_NEGATIVE_NOT_ALLOWED",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (v in non_negative_vars) {
          if (!(v %in% names(df))) next
          vals <- suppressWarnings(as.numeric(df[[v]]))
          bad  <- !is.na(vals) & vals < 0
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .select_context_columns(df, key_vars, subject_var, group_var, v), drop = FALSE]
          piece$VARIABLE <- v
          piece$VALUE    <- vals[bad]
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
