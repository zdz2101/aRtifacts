rule_r040_impossible_combos <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", impossible_combos = list(), owner_team = "Standards", ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  if (length(impossible_combos)) {
    new_flag_rule(
      id         = "R040_IMPOSSIBLE_COMBOS",
      domain     = domain,
      severity   = "error",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (combo in impossible_combos) {
          vars <- names(combo)
          if (!all(vars %in% names(df))) next
          match_vec <- vapply(vars, function(v) df[[v]] == combo[[v]], logical(nrow(df)))
          if (is.vector(match_vec)) {
            bad <- match_vec
          } else {
            bad <- rowSums(match_vec) == length(vars)
          }
          if (!any(bad, na.rm = TRUE)) next
          k <- k + 1L
          piece <- df[which(bad), .ctx(df, vars), drop = FALSE]
          piece$COMBO <- paste(paste(vars, combo, sep = "="), collapse = " & ")
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
