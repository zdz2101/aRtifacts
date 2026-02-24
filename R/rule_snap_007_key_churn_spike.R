#' SNAP-007: Key churn spike
#'
#' @param domain Domain name.
#' @param keys Identity key columns.
#' @param churn_threshold Maximum expected churn rate.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_snap_007_key_churn_spike <- function(domain = "ADSL",
                                          keys = c("USUBJID"),
                                          churn_threshold = 0.1,
                                          owner_team = "Clinical Ops") {
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

      prev_k <- unique(previous[, by, drop = FALSE])
      cur_k <- unique(current[, by, drop = FALSE])
      prev_id <- .key_id(prev_k, by)
      cur_id <- .key_id(cur_k, by)

      added_n <- sum(!(cur_id %in% prev_id))
      removed_n <- sum(!(prev_id %in% cur_id))
      prev_n <- length(prev_id)
      churn_rate <- if (prev_n > 0) as.numeric(added_n + removed_n) / as.numeric(prev_n) else NA_real_

      if (is.na(churn_rate) || churn_rate <= churn_threshold) {
        return(data.frame())
      }

      data.frame(
        prev_n_keys = as.integer(prev_n),
        cur_n_keys = as.integer(length(cur_id)),
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
