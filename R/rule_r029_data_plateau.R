#' Rule R029 — data plateau
#'
#' Builds a diff-scope rule specification that flags unchanged cuts where both
#' record volume and key churn remain flat.
#'
#' @inheritParams make_rule_pack_missingness
#' @return A diff-scope rule specification list with fields
#'   `id`, `domain`, `severity`, `category`, `owner_team`, `scope`, and
#'   `fn(old_dom, new_dom)`.
rule_r029_data_plateau <- function(domain, key_vars, required_vars = character(), subject_var = "USUBJID", group_var = "SITEID", visit_var = NULL, baseline_flag_var = NULL, missingness_threshold = 0.1, owner_team = "DM", ...) {
  list(
    id = "R029_DATA_PLATEAU",
    domain = domain,
    severity = "warning",
    category = "ops_anomaly",
    owner_team = owner_team,
    scope = "diff",
    fn = function(old_dom, new_dom) {
      .assert_domain(old_dom, "old_dom")
      .assert_domain(new_dom, "new_dom")

      vol <- compare_record_volume(old_dom, new_dom)
      churn <- compare_key_churn(old_dom, new_dom)
      plateau <- vol$absolute_change == 0L && churn$churn_n == 0L
      if (!plateau) {
        return(data.frame(
          old_n_records = integer(0),
          new_n_records = integer(0),
          absolute_change = integer(0),
          key_churn_n = integer(0),
          stringsAsFactors = FALSE
        ))
      }

      data.frame(
        old_n_records = vol$old_n_records,
        new_n_records = vol$new_n_records,
        absolute_change = vol$absolute_change,
        key_churn_n = churn$churn_n,
        stringsAsFactors = FALSE
      )
    }
  )
}
