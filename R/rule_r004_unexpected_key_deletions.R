#' Rule R004 — unexpected key deletions
#'
#' Builds a `DiffFlagRule` that flags keys present in the old
#' cut but missing in the new cut.
#'
#' @inheritParams make_rule_pack_keys
#' @return A `DiffFlagRule` object.
rule_r004_unexpected_key_deletions <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", dup_rate_threshold = 0.1, owner_team = "DM", ...) {
  new_diff_flag_rule(
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
}
