rule_r034_suspicious_zeros <- function(domain, numeric_vars = character(), zero_rate_threshold = 0.5, owner_team = "Biostats", ...) {
  if (length(numeric_vars)) {
    new_flag_rule(
      id         = "R034_SUSPICIOUS_ZEROS",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (v in numeric_vars) {
          if (!(v %in% names(df))) next
          vals     <- suppressWarnings(as.numeric(df[[v]]))
          nonmiss  <- !is.na(vals)
          if (!any(nonmiss)) next
          zero_rate <- mean(vals[nonmiss] == 0, na.rm = TRUE)
          if (zero_rate <= zero_rate_threshold) next
          k <- k + 1L
          out[[k]] <- data.frame(
            VARIABLE   = v,
            ZERO_RATE  = zero_rate,
            N_ZERO     = sum(vals == 0, na.rm = TRUE),
            N_NONMISS  = sum(nonmiss),
            stringsAsFactors = FALSE
          )
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
