rule_r017_identical_timestamps <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", date_vars = character(), owner_team = "DM", ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

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
          piece <- df[dup, .ctx(df, dv), drop = FALSE]
          piece$DATE_VAR <- dv
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
