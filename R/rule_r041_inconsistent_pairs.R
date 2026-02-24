rule_r041_inconsistent_pairs <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", inconsistent_pairs = list(), owner_team = "Standards", ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  if (length(inconsistent_pairs)) {
    new_flag_rule(
      id         = "R041_INCONSISTENT_PAIRS",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (pair in inconsistent_pairs) {
          vars <- names(pair)
          if (!all(vars %in% names(df))) next
          match_vec <- vapply(vars, function(v) !is.na(df[[v]]) & df[[v]] == pair[[v]], logical(nrow(df)))
          if (is.vector(match_vec)) {
            bad <- match_vec
          } else {
            bad <- rowSums(match_vec) == length(vars)
          }
          if (!any(bad, na.rm = TRUE)) next
          k <- k + 1L
          piece <- df[which(bad), .ctx(df, vars), drop = FALSE]
          piece$INCONSISTENCY <- paste(paste(vars, pair, sep = "="), collapse = " & ")
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
