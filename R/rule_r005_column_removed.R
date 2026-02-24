rule_r005_column_removed <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", dup_rate_threshold = 0.1, owner_team = "DM", ...) {
  list(
    id = "R005_COLUMN_REMOVED",
    domain = domain,
    severity = "warning",
    category = "programming_etl",
    owner_team = owner_team,
    scope = "diff",
    fn = function(old_dom, new_dom) {
      .assert_domain(old_dom, "old_dom")
      .assert_domain(new_dom, "new_dom")

      out <- diff_columns(old_dom, new_dom)
      out <- out[out$status == "removed", c("column", "status"), drop = FALSE]
      if (!nrow(out)) return(out)
      out[order(out$column), , drop = FALSE]
    }
  )
}
