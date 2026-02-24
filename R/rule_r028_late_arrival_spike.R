rule_r028_late_arrival_spike <- function(domain, key_vars, required_vars = character(), subject_var = "USUBJID", group_var = "SITEID", visit_var = NULL, baseline_flag_var = NULL, missingness_threshold = 0.1, owner_team = "DM", ...) {
  list(
    id = "R028_LATE_ARRIVAL_SPIKE",
    domain = domain,
    severity = "warning",
    category = "ops_anomaly",
    owner_team = owner_team,
    scope = "diff",
    fn = function(old_dom, new_dom) {
      .assert_domain(old_dom, "old_dom")
      .assert_domain(new_dom, "new_dom")

      vol <- compare_record_volume(old_dom, new_dom)
      if (!(vol$relative_change > missingness_threshold)) {
        return(data.frame(
          old_n_records = integer(0),
          new_n_records = integer(0),
          absolute_change = integer(0),
          relative_change = numeric(0),
          threshold = numeric(0),
          stringsAsFactors = FALSE
        ))
      }

      data.frame(
        old_n_records = vol$old_n_records,
        new_n_records = vol$new_n_records,
        absolute_change = vol$absolute_change,
        relative_change = vol$relative_change,
        threshold = missingness_threshold,
        stringsAsFactors = FALSE
      )
    }
  )
}
