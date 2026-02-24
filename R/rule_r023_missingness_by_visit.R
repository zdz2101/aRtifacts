rule_r023_missingness_by_visit <- function(domain, visit_var = NULL, key_vars = character(), site_var = "SITEID", subject_var = "USUBJID", missingness_threshold = 0.1, owner_team = "DM", ...) {
new_flag_rule(
      id         = "R023_MISSINGNESS_BY_VISIT",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        vv <- visit_var
        if (is.null(vv) || !(vv %in% names(df)) || !nrow(df)) return(data.frame())
        check_vars <- setdiff(names(df), c(key_vars, site_var, subject_var, vv))
        if (!length(check_vars)) return(data.frame())
        visits <- unique(df[[vv]])
        out <- list(); k <- 0L
        for (v in visits) {
          sub   <- df[!is.na(df[[vv]]) & df[[vv]] == v, check_vars, drop = FALSE]
          if (!nrow(sub)) next
          rates <- colMeans(is.na(sub))
          bad   <- rates[rates > missingness_threshold]
          if (!length(bad)) next
          k <- k + 1L
          out[[k]] <- data.frame(
            VISIT     = as.character(v),
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
