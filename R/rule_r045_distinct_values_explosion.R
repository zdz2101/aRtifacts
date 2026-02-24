rule_r045_distinct_values_explosion <- function(domain, categorical_vars = character(), max_distinct_values = 50L, owner_team = "Standards", ...) {
  if (length(categorical_vars)) {
    new_flag_rule(
      id         = "R045_DISTINCT_VALUES_EXPLOSION",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (v in categorical_vars) {
          if (!(v %in% names(df))) next
          n_distinct <- length(unique(df[[v]][!is.na(df[[v]])]))
          if (n_distinct <= max_distinct_values) next
          k <- k + 1L
          out[[k]] <- data.frame(
            VARIABLE   = v,
            N_DISTINCT = n_distinct,
            THRESHOLD  = max_distinct_values,
            stringsAsFactors = FALSE
          )
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
