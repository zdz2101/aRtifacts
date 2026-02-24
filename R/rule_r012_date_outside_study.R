rule_r012_date_outside_study <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", date_vars = character(), study_start = NULL, study_end = NULL, owner_team = "DM", ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  if (!is.null(study_start) || !is.null(study_end)) {
    s_start <- if (!is.null(study_start)) as.Date(study_start) else NULL
    s_end   <- if (!is.null(study_end))   as.Date(study_end)   else NULL
    new_flag_rule(
      id         = "R012_DATE_OUTSIDE_STUDY",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (dv in date_vars) {
          if (!(dv %in% names(df))) next
          d <- suppressWarnings(as.Date(df[[dv]]))
          bad <- rep(FALSE, length(d))
          if (!is.null(s_start)) bad <- bad | (!is.na(d) & d < s_start)
          if (!is.null(s_end))   bad <- bad | (!is.na(d) & d > s_end)
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .ctx(df, dv), drop = FALSE]
          piece$DATE_VAR <- dv
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
