#' SNAP-006: Row count spike/drop
#'
#' @param domain Domain name.
#' @param threshold_abs Absolute row-count change threshold.
#' @param threshold_rel Relative row-count change threshold.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_snap_006_row_count_spike_drop <- function(domain = "ADSL",
                                                threshold_abs = 100,
                                                threshold_rel = 0.2,
                                                owner_team = "Clinical Ops") {
  new_rule(
    id = "SNAP-006",
    title = "Row count spike/drop",
    topology = "A6",
    scope = "snapshot",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      n_prev <- nrow(previous)
      n_cur <- nrow(current)
      delta_n <- as.integer(n_cur - n_prev)
      delta_abs <- abs(delta_n)
      delta_rel <- if (n_prev > 0) as.numeric(delta_n) / as.numeric(n_prev) else NA_real_

      bad_abs <- !is.na(threshold_abs) && delta_abs > threshold_abs
      bad_rel <- !is.na(threshold_rel) && !is.na(delta_rel) && abs(delta_rel) > threshold_rel
      if (!(bad_abs || bad_rel)) {
        return(data.frame())
      }

      data.frame(
        n_prev = as.integer(n_prev),
        n_cur = as.integer(n_cur),
        delta_n = delta_n,
        delta_abs = as.integer(delta_abs),
        delta_rel = delta_rel,
        threshold_abs = as.numeric(threshold_abs),
        threshold_rel = as.numeric(threshold_rel),
        reason = "Row count changed beyond threshold",
        stringsAsFactors = FALSE
      )
    }
  )
}
