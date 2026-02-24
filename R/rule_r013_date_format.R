rule_r013_date_format <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", date_vars = character(), owner_team = "DM", ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  if (length(date_vars)) {
    new_flag_rule(
      id         = "R013_DATE_FORMAT",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (dv in date_vars) {
          if (!(dv %in% names(df))) next
          vals    <- as.character(df[[dv]])
          present <- !is.na(vals) & nzchar(vals)
          bad     <- present & !grepl("^\\d{4}-\\d{2}-\\d{2}", vals)
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .ctx(df, dv), drop = FALSE]
          piece$DATE_VAR   <- dv
          piece$BAD_VALUE  <- vals[bad]
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
