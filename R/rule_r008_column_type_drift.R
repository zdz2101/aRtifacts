#' Rule R008 — column type drift
#'
#' Builds a diff-scope rule specification that flags shared columns whose
#' classes changed between cuts.
#'
#' @inheritParams make_rule_pack_keys
#' @return A diff-scope rule specification list with fields
#'   `id`, `domain`, `severity`, `category`, `owner_team`, `scope`, and
#'   `fn(old_dom, new_dom)`.
rule_r008_column_type_drift <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", dup_rate_threshold = 0.1, owner_team = "DM", ...) {
  base_rule <- new_flag_rule(
    id = "R008_COLUMN_TYPE_DRIFT",
    domain = domain,
    severity = "warning",
    category = "programming_etl",
    owner_team = owner_team,
    fn = function(old_dom, new_dom) {
      .assert_domain(old_dom, "old_dom")
      .assert_domain(new_dom, "new_dom")

      out <- diff_column_types(old_dom, new_dom)
      out <- out[out$changed, c("column", "old_type", "new_type"), drop = FALSE]
      if (!nrow(out)) return(out)
      out[order(out$column), , drop = FALSE]
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
