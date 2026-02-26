#' SNAP-007: Key churn spike
#'
#' @param domain Domain name.
#' @param keys Identity key columns.
#' @param churn_threshold Maximum expected churn rate.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_snap_007_key_churn_spike <- function(domain = "PRIMARY",
                                          keys = c("entity_id"),
                                          churn_threshold = 0.1,
                                          owner_team = "Data Operations") {
  new_rule(
    id = "SNAP-007",
    title = "Key churn spike",
    topology = "A6",
    scope = "snapshot",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    identity_spec = new_identity_spec(keys = keys, method = "exact"),
    fn = function(previous, current, context, rule) {
      by <- intersect(keys, intersect(names(previous), names(current)))
      if (!length(by)) {
        return(data.frame())
      }

      prev_k <- unique(as.data.frame(previous, stringsAsFactors = FALSE)[, by, drop = FALSE])
      cur_k <- unique(as.data.frame(current, stringsAsFactors = FALSE)[, by, drop = FALSE])

      prev_dom <- new_domain_snapshot(name = domain, data = prev_k, key_vars = by)
      cur_dom <- new_domain_snapshot(name = domain, data = cur_k, key_vars = by)
      churn <- compare_key_churn(prev_dom, cur_dom)

      added_n <- as.integer(churn$added_n[[1]])
      removed_n <- as.integer(churn$removed_n[[1]])
      prev_n <- as.integer(churn$old_n_keys[[1]])
      churn_rate <- as.numeric(churn$churn_rate[[1]])
      cur_n <- as.integer(churn$new_n_keys[[1]])

      if (is.na(churn_rate) || churn_rate <= churn_threshold) {
        return(data.frame())
      }

      data.frame(
        prev_n_keys = as.integer(prev_n),
        cur_n_keys = as.integer(cur_n),
        added_n = as.integer(added_n),
        removed_n = as.integer(removed_n),
        churn_rate = churn_rate,
        threshold = as.numeric(churn_threshold),
        reason = "Key churn exceeds threshold",
        stringsAsFactors = FALSE
      )
    }
  )
}
