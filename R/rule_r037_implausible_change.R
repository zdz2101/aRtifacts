rule_r037_implausible_change <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", visit_order_var = NULL, max_within_change = list(), owner_team = "Biostats", ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  if (!is.null(visit_order_var) && length(max_within_change)) {
    new_flag_rule(
      id         = "R037_IMPLAUSIBLE_CHANGE",
      domain     = domain,
      severity   = "warning",
      category   = "clinical_review",
      owner_team = owner_team,
      fn = function(df) {
        if (!(subject_var %in% names(df) && visit_order_var %in% names(df))) {
          return(data.frame())
        }
        ord    <- order(df[[subject_var]], df[[visit_order_var]], na.last = TRUE)
        sorted <- df[ord, , drop = FALSE]
        out <- list(); k <- 0L
        for (v in names(max_within_change)) {
          if (!(v %in% names(sorted))) next
          threshold <- max_within_change[[v]]
          grps      <- split(seq_len(nrow(sorted)), sorted[[subject_var]])
          bad_idx   <- integer(0)
          for (idx in grps) {
            vals  <- suppressWarnings(as.numeric(sorted[[v]][idx]))
            chg   <- abs(diff(vals))
            flags <- which(!is.na(chg) & chg > threshold)
            if (length(flags)) bad_idx <- c(bad_idx, idx[flags + 1L])
          }
          if (!length(bad_idx)) next
          k <- k + 1L
          piece <- sorted[bad_idx, .ctx(sorted, c(v, visit_order_var)), drop = FALSE]
          piece$VARIABLE  <- v
          piece$VALUE     <- suppressWarnings(as.numeric(sorted[[v]][bad_idx]))
          piece$THRESHOLD <- threshold
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
