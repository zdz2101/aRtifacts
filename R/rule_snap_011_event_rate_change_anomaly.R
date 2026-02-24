#' SNAP-011: Event rate change anomaly
#'
#' @param domain Domain name.
#' @param group_var Grouping variable.
#' @param subject_var Subject identifier variable.
#' @param threshold_abs_change Absolute threshold for event-rate change.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_snap_011_event_rate_change_anomaly <- function(domain = "AE",
                                                    group_var = "SITEID",
                                                    subject_var = "USUBJID",
                                                    threshold_abs_change = 0.5,
                                                    owner_team = "Clinical Ops") {
  new_rule(
    id = "SNAP-011",
    title = "Event rate change anomaly",
    topology = "A6",
    scope = "group",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      needed <- c(group_var, subject_var)
      if (!all(needed %in% names(previous)) || !all(needed %in% names(current))) {
        return(data.frame())
      }

      prev_evt <- as.data.frame(table(as.character(previous[[group_var]])), stringsAsFactors = FALSE)
      names(prev_evt) <- c("group", "events_prev")
      cur_evt <- as.data.frame(table(as.character(current[[group_var]])), stringsAsFactors = FALSE)
      names(cur_evt) <- c("group", "events_cur")

      prev_sub <- as.data.frame(table(as.character(unique(previous[, needed, drop = FALSE])[[group_var]])), stringsAsFactors = FALSE)
      names(prev_sub) <- c("group", "subjects_prev")
      cur_sub <- as.data.frame(table(as.character(unique(current[, needed, drop = FALSE])[[group_var]])), stringsAsFactors = FALSE)
      names(cur_sub) <- c("group", "subjects_cur")

      merged <- merge(prev_evt, cur_evt, by = "group", all = TRUE)
      merged <- merge(merged, prev_sub, by = "group", all = TRUE)
      merged <- merge(merged, cur_sub, by = "group", all = TRUE)
      merged[is.na(merged)] <- 0

      merged$rate_prev <- mapply(.core50_rate, merged$events_prev, merged$subjects_prev)
      merged$rate_cur <- mapply(.core50_rate, merged$events_cur, merged$subjects_cur)
      merged$delta_abs <- abs(merged$rate_cur - merged$rate_prev)

      bad <- !is.na(merged$delta_abs) & merged$delta_abs > threshold_abs_change
      if (!any(bad)) {
        return(data.frame())
      }

      out <- merged[bad, c("group", "rate_prev", "rate_cur", "delta_abs", "events_prev", "events_cur"), drop = FALSE]
      names(out)[names(out) == "group"] <- group_var
      out$threshold <- as.numeric(threshold_abs_change)
      out$reason <- "Event rate change exceeds threshold"
      out
    }
  )
}
