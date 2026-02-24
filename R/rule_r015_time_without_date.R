rule_r015_time_without_date <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", time_date_pairs = list(), owner_team = "DM", ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  if (length(time_date_pairs)) {
    new_flag_rule(
      id         = "R015_TIME_WITHOUT_DATE",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (pair in time_date_pairs) {
          if (length(pair) != 2) next
          tv <- pair[[1]]; dv <- pair[[2]]
          if (!(tv %in% names(df) && dv %in% names(df))) next
          t_present <- !is.na(df[[tv]]) & nzchar(as.character(df[[tv]]))
          d_missing <- is.na(df[[dv]])  | !nzchar(as.character(df[[dv]]))
          bad <- t_present & d_missing
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .ctx(df, c(tv, dv)), drop = FALSE]
          piece$TIME_VAR <- tv; piece$DATE_VAR <- dv
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
