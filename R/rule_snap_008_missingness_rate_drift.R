#' SNAP-008: Missingness rate drift
#'
#' @param domain Domain name.
#' @param group_var Grouping variable.
#' @param value_var Variable to assess missingness drift.
#' @param threshold_increase Threshold for missingness increase.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_snap_008_missingness_rate_drift <- function(domain = "PRIMARY",
                                                 group_var = "group_id",
                                                 value_var = "value",
                                                 threshold_increase = 0.1,
                                                 owner_team = "Data Operations") {
  new_rule(
    id = "SNAP-008",
    title = "Missingness rate drift",
    topology = "A6",
    scope = "group",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      f_missing_rate_by_group <- get(".topology_missing_rate_by_group", mode = "function", inherits = TRUE)

      prev_m <- f_missing_rate_by_group(previous, group_var = group_var, value_var = value_var)
      cur_m <- f_missing_rate_by_group(current, group_var = group_var, value_var = value_var)
      if (!nrow(prev_m) || !nrow(cur_m)) {
        return(data.frame())
      }

      merged <- merge(prev_m, cur_m, by = "group", all = TRUE, suffixes = c("_prev", "_cur"))
      merged$n_prev[is.na(merged$n_prev)] <- 0L
      merged$n_cur[is.na(merged$n_cur)] <- 0L
      merged$miss_rate_prev[is.na(merged$miss_rate_prev)] <- 0
      merged$miss_rate_cur[is.na(merged$miss_rate_cur)] <- 0
      merged$delta <- merged$miss_rate_cur - merged$miss_rate_prev

      bad <- merged$delta > threshold_increase
      if (!any(bad)) {
        return(data.frame())
      }

      out <- merged[bad, c("group", "miss_rate_prev", "miss_rate_cur", "delta", "n_prev", "n_cur"), drop = FALSE]
      names(out)[names(out) == "group"] <- group_var
      out$value_var <- value_var
      out$threshold <- as.numeric(threshold_increase)
      out$reason <- "Missingness rate increased beyond threshold"
      out
    }
  )
}
