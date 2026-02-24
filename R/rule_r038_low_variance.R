rule_r038_low_variance <- function(domain, numeric_vars = character(), low_cv_threshold = 0.01, owner_team = "Biostats", ...) {
  if (length(numeric_vars)) {
    new_flag_rule(
      id         = "R038_LOW_VARIANCE",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (v in numeric_vars) {
          if (!(v %in% names(df))) next
          vals    <- suppressWarnings(as.numeric(df[[v]]))
          nonmiss <- vals[!is.na(vals)]
          if (length(nonmiss) < 2L) next
          mn  <- mean(nonmiss)
          cv  <- if (mn != 0) stats::sd(nonmiss) / abs(mn) else stats::sd(nonmiss)
          if (cv > low_cv_threshold) next
          k <- k + 1L
          out[[k]] <- data.frame(
            VARIABLE  = v,
            CV        = cv,
            MEAN      = mn,
            SD        = stats::sd(nonmiss),
            N         = length(nonmiss),
            stringsAsFactors = FALSE
          )
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
