rule_r002_duplicate_keys <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", owner_team = "DM", ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }
new_flag_rule(
      id         = "R002_DUPLICATE_KEYS",
      domain     = domain,
      severity   = "critical",
      category   = "programming_etl",
      owner_team = owner_team,
      fn = function(df) {
        present <- intersect(key_vars, names(df))
        if (!length(present)) return(data.frame())
        dup <- duplicated(df[, present, drop = FALSE]) |
          duplicated(df[, present, drop = FALSE], fromLast = TRUE)
        if (!any(dup)) return(data.frame())
        df[dup, .ctx(df), drop = FALSE]
      }
    )
}
