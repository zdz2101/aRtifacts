rule_r021_missingness_overall <- function(domain, missingness_threshold = 0.1, owner_team = "DM", ...) {
new_flag_rule(
      id         = "R021_MISSINGNESS_OVERALL",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        if (!nrow(df)) return(data.frame())
        rates <- colMeans(is.na(df))
        bad   <- rates[rates > missingness_threshold]
        if (!length(bad)) return(data.frame())
        data.frame(
          VARIABLE     = names(bad),
          MISS_RATE    = unname(bad),
          N_MISSING    = as.integer(unname(bad) * nrow(df)),
          stringsAsFactors = FALSE
        )
      }
    )
}
