rule_r039_rounding_anomaly <- function(domain, numeric_vars = character(), round_rate_threshold = 0.9, owner_team = "Biostats", ...) {
  if (length(numeric_vars)) {
    new_flag_rule(
      id         = "R039_ROUNDING_ANOMALY",
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
          if (length(nonmiss) < 5L) next
          round_rate <- mean(nonmiss %% 5 == 0, na.rm = TRUE)
          if (round_rate <= round_rate_threshold) next
          k <- k + 1L
          out[[k]] <- data.frame(
            VARIABLE   = v,
            ROUND_RATE = round_rate,
            N          = length(nonmiss),
            stringsAsFactors = FALSE
          )
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
