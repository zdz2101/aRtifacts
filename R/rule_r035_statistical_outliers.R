rule_r035_statistical_outliers <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", numeric_vars = character(), iqr_multiplier = 3.0, owner_team = "Biostats", ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  if (length(numeric_vars)) {
    new_flag_rule(
      id         = "R035_STATISTICAL_OUTLIERS",
      domain     = domain,
      severity   = "warning",
      category   = "clinical_review",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (v in numeric_vars) {
          if (!(v %in% names(df))) next
          vals <- suppressWarnings(as.numeric(df[[v]]))
          nonmiss <- !is.na(vals)
          if (sum(nonmiss) < 4L) next
          q1  <- stats::quantile(vals[nonmiss], 0.25)
          q3  <- stats::quantile(vals[nonmiss], 0.75)
          iqr <- q3 - q1
          if (iqr == 0) next
          lo  <- q1 - iqr_multiplier * iqr
          hi  <- q3 + iqr_multiplier * iqr
          bad <- nonmiss & (vals < lo | vals > hi)
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .ctx(df, v), drop = FALSE]
          piece$VARIABLE <- v
          piece$VALUE    <- vals[bad]
          piece$FENCE_LO <- lo
          piece$FENCE_HI <- hi
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
