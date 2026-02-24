#' Rule R007 — columns removed
#'
#' Builds a `DiffFlagRule` that flags columns present in the old
#' cut but removed in the new cut.
#'
#' @inheritParams make_rule_pack_keys
#' @return A `DiffFlagRule` object.
rule_r007_columns_removed <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", dup_rate_threshold = 0.1, owner_team = "DM", ...) {
  new_diff_flag_rule(
    id = "R007_COLUMNS_REMOVED",
    domain = domain,
    severity = "warning",
    category = "programming_etl",
    owner_team = owner_team,
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
