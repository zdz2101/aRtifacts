rule_r005_key_churn_spike <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", dup_rate_threshold = 0.1, owner_team = "DM", ...) {
  base_rule <- new_flag_rule(
    id = "R005_KEY_CHURN_SPIKE",
    domain = domain,
    severity = "warning",
    category = "ops_anomaly",
    owner_team = owner_team,
    fn = function(old_dom, new_dom) {
      .assert_domain(old_dom, "old_dom")
      .assert_domain(new_dom, "new_dom")

      summary <- compare_key_churn(old_dom, new_dom)
      if (!(summary$churn_rate > dup_rate_threshold)) {
        return(data.frame(
          old_n_keys = integer(0),
          new_n_keys = integer(0),
          added_n = integer(0),
          removed_n = integer(0),
          churn_n = integer(0),
          churn_rate = numeric(0),
          threshold = numeric(0),
          stringsAsFactors = FALSE
        ))
      }

      data.frame(
        old_n_keys = summary$old_n_keys,
        new_n_keys = summary$new_n_keys,
        added_n = summary$added_n,
        removed_n = summary$removed_n,
        churn_n = summary$churn_n,
        churn_rate = summary$churn_rate,
        threshold = dup_rate_threshold,
        stringsAsFactors = FALSE
      )
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
