rule_r001_key_missing <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", owner_team = "DM", ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }
new_flag_rule(
      id         = "R001_KEY_MISSING",
      domain     = domain,
      severity   = "critical",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        present <- intersect(key_vars, names(df))
        if (!length(present)) return(data.frame())
        miss <- rowSums(
          is.na(df[, present, drop = FALSE]) |
            df[, present, drop = FALSE] == ""
        ) > 0
        if (!any(miss)) return(data.frame())
        df[miss, .ctx(df), drop = FALSE]
      }
    )
}
