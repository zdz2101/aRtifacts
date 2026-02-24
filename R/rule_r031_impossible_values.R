rule_r031_impossible_values <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", value_ranges = list(), owner_team = "Biostats", ...) {
  if (length(value_ranges)) {
    new_flag_rule(
      id         = "R031_IMPOSSIBLE_VALUES",
      domain     = domain,
      severity   = "error",
      category   = "clinical_review",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (v in names(value_ranges)) {
          if (!(v %in% names(df))) next
          bounds <- value_ranges[[v]]
          vals   <- suppressWarnings(as.numeric(df[[v]]))
          bad    <- !is.na(vals) & (vals < bounds[1] | vals > bounds[2])
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .select_context_columns(df, key_vars, subject_var, group_var, v), drop = FALSE]
          piece$VARIABLE  <- v
          piece$VALUE     <- vals[bad]
          piece$RANGE_MIN <- bounds[1]
          piece$RANGE_MAX <- bounds[2]
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
