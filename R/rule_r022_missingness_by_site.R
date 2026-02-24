rule_r022_missingness_by_site <- function(domain, site_var = "SITEID", key_vars = character(), subject_var = "USUBJID", missingness_threshold = 0.1, owner_team = "DM", ...) {
new_flag_rule(
      id         = "R022_MISSINGNESS_BY_SITE",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        if (!nrow(df) || !(site_var %in% names(df))) return(data.frame())
        check_vars <- setdiff(names(df), c(key_vars, site_var, subject_var))
        if (!length(check_vars)) return(data.frame())
        sites <- unique(df[[site_var]])
        out <- list(); k <- 0L
        for (s in sites) {
          sub   <- df[df[[site_var]] == s, check_vars, drop = FALSE]
          if (!nrow(sub)) next
          rates <- colMeans(is.na(sub))
          bad   <- rates[rates > missingness_threshold]
          if (!length(bad)) next
          k <- k + 1L
          out[[k]] <- data.frame(
            SITE_ID   = s,
            VARIABLE  = names(bad),
            MISS_RATE = unname(bad),
            stringsAsFactors = FALSE
          )
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
}
