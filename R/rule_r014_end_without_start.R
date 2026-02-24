rule_r014_end_without_start <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", start_end_pairs = list(), owner_team = "DM", ...) {
  if (length(start_end_pairs)) {
    new_flag_rule(
      id         = "R014_END_WITHOUT_START",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (pair in start_end_pairs) {
          if (length(pair) != 2) next
          s <- pair[[1]]; e <- pair[[2]]
          if (!(s %in% names(df) && e %in% names(df))) next
          sd  <- suppressWarnings(as.Date(df[[s]]))
          ed  <- suppressWarnings(as.Date(df[[e]]))
          bad <- is.na(sd) & !is.na(ed)
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .select_context_columns(df, key_vars, subject_var, group_var, c(s, e)), drop = FALSE]
          piece$START_VAR <- s; piece$END_VAR <- e
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
