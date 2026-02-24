rule_r025_domain_empty <- function(domain, owner_team = "DM", ...) {
new_flag_rule(
      id         = "R025_DOMAIN_EMPTY",
      domain     = domain,
      severity   = "error",
      category   = "programming_etl",
      owner_team = owner_team,
      fn = function(df) {
        if (nrow(df) > 0L) return(data.frame())
        data.frame(ISSUE = paste0("Domain '", domain, "' contains zero records."), stringsAsFactors = FALSE)
      }
    )
}
