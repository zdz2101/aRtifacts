#' Rule R003 — key type drift
#'
#' Builds a `DiffFlagRule` that flags key variables whose
#' underlying classes changed between two `DomainSnapshot` cuts.
#'
#' @inheritParams make_rule_pack_keys
#' @return A `DiffFlagRule` object.
rule_r003_key_type_drift <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", dup_rate_threshold = 0.1, owner_team = "DM", ...) {
  new_diff_flag_rule(
    id = "R003_KEY_TYPE_DRIFT",
    domain = domain,
    severity = "warning",
    category = "programming_etl",
    owner_team = owner_team,
    fn = function(old_dom, new_dom) {
      .assert_domain(old_dom, "old_dom")
      .assert_domain(new_dom, "new_dom")

      out <- diff_key_types(old_dom, new_dom)
      out <- out[out$changed, c("key_var", "old_type", "new_type"), drop = FALSE]
      if (!nrow(out)) return(out)
      out[order(out$key_var), , drop = FALSE]
    }
  )
}
