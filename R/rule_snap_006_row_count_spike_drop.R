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
      prev_df <- as.data.frame(previous, stringsAsFactors = FALSE)
      cur_df <- as.data.frame(current, stringsAsFactors = FALSE)

      prev_df$.ROWID <- seq_len(nrow(prev_df))
      cur_df$.ROWID <- seq_len(nrow(cur_df))

      prev_dom <- new_domain_snapshot(name = domain, data = prev_df, key_vars = ".ROWID")
      cur_dom <- new_domain_snapshot(name = domain, data = cur_df, key_vars = ".ROWID")

      vol <- compare_record_volume(prev_dom, cur_dom)

      n_prev <- as.integer(vol$old_n_records[[1]])
      n_cur <- as.integer(vol$new_n_records[[1]])
      delta_n <- as.integer(vol$absolute_change[[1]])
      delta_abs <- abs(delta_n)
      delta_rel <- as.numeric(vol$relative_change[[1]])

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
