#' SNAP-010: Site metric drift
#'
#' @param domain Domain name.
#' @param group_var Grouping variable.
#' @param value_var Numeric metric variable.
#' @param threshold_abs_diff Absolute threshold for metric change.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_snap_010_site_metric_drift <- function(domain = "PRIMARY",
                                            group_var = "group_id",
                                            value_var = "value",
                                            threshold_abs_diff = 5,
                                            owner_team = "Data Operations") {
  new_rule(
    id = "SNAP-010",
    title = "Site metric drift",
    topology = "A6",
    scope = "group",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      prev_m <- .topology_metric_by_group(previous, group_var = group_var, value_var = value_var, fun = mean)
      cur_m <- .topology_metric_by_group(current, group_var = group_var, value_var = value_var, fun = mean)
      if (!nrow(prev_m) || !nrow(cur_m)) {
        return(data.frame())
      }

      merged <- merge(prev_m, cur_m, by = "group", all = TRUE, suffixes = c("_prev", "_cur"))
      merged$metric_prev[is.na(merged$metric_prev)] <- 0
      merged$metric_cur[is.na(merged$metric_cur)] <- 0
      merged$delta_abs <- abs(merged$metric_cur - merged$metric_prev)

      bad <- merged$delta_abs > threshold_abs_diff
      if (!any(bad)) {
        return(data.frame())
      }

      out <- merged[bad, c("group", "metric_prev", "metric_cur", "delta_abs"), drop = FALSE]
      names(out)[names(out) == "group"] <- group_var
      out$value_var <- value_var
      out$threshold <- as.numeric(threshold_abs_diff)
      out$reason <- "Site metric drift exceeds threshold"
      out
    }
  )
}
