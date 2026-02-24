rule_r024_required_missing <- function(domain, required_vars = character(), owner_team = "DM", ...) {
new_flag_rule(
      id         = "R024_REQUIRED_MISSING",
      domain     = domain,
      severity   = "error",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        absent <- setdiff(required_vars, names(df))
        present_with_na <- character()
        for (v in intersect(required_vars, names(df))) {
          if (any(is.na(df[[v]]))) present_with_na <- c(present_with_na, v)
        }
        issues <- c(
          if (length(absent))          paste0(absent, " (column absent)"),
          if (length(present_with_na)) paste0(present_with_na, " (NA values present)")
        )
        if (!length(issues)) return(data.frame())
        data.frame(ISSUE = issues, stringsAsFactors = FALSE)
      }
    )
}
