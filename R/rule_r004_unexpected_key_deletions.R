rule_r004_unexpected_key_deletions <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", dup_rate_threshold = 0.1, owner_team = "DM", ...) {
  base_rule <- new_flag_rule(
    id = "R004_UNEXPECTED_KEY_DELETIONS",
    domain = domain,
    severity = "warning",
    category = "programming_etl",
    owner_team = owner_team,
    fn = function(old_dom, new_dom) {
      .assert_domain(old_dom, "old_dom")
      .assert_domain(new_dom, "new_dom")

      out <- diff_keys(old_dom, new_dom)$removed
      if (!nrow(out)) return(out)
      out[do.call(order, out), , drop = FALSE]
    }
  )

  list(
    id = base_rule@id,
    domain = base_rule@domain,
    severity = base_rule@severity,
    category = base_rule@category,
    owner_team = base_rule@owner_team,
    scope = "diff",
    fn = base_rule@fn
  )
}
