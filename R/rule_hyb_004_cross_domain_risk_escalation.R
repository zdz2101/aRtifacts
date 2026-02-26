#' HYB-004: Cross-domain risk escalation
#'
#' @param events_domain Events domain name.
#' @param support_domain Support domain name.
#' @param metric_domain Numeric metric domain name.
#' @param group_var Group variable.
#' @param entity_var Entity identifier variable.
#' @param metric_var Numeric metric variable.
#' @param metric_high_cutoff High-value threshold in metric domain.
#' @param risk_delta_threshold Absolute risk increase threshold.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_hyb_004_cross_domain_risk_escalation <- function(events_domain = "EVENTS",
                                                      support_domain = "SUPPORT",
                                                      metric_domain = "METRIC",
                                                      group_var = "group_id",
                                                      entity_var = "entity_id",
                                                      metric_var = "metric_value",
                                                      metric_high_cutoff = 100,
                                                      risk_delta_threshold = 0.3,
                                                      owner_team = "Data Operations") {
  new_rule(
    id = "HYB-004",
    title = "Cross-domain risk escalation",
    topology = "A7",
    scope = "group",
    intent = "ops_anomaly",
    domains = c(events_domain, support_domain, metric_domain),
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      score_by_group <- function(cut_domains) {
        events <- .topology_domain_df(cut_domains, events_domain)
        support <- .topology_domain_df(cut_domains, support_domain)
        metric <- .topology_domain_df(cut_domains, metric_domain)
        needed <- c(group_var, entity_var)
        if (is.null(events) || is.null(support) || is.null(metric) ||
            !all(needed %in% names(events)) || !all(needed %in% names(support)) ||
            !(group_var %in% names(metric)) || !(metric_var %in% names(metric))) {
          return(data.frame())
        }

        evt <- .topology_events_per_entity_by_group(events, group_var = group_var, entity_var = entity_var)

        sup_unique <- unique(support[, c(group_var, entity_var), drop = FALSE])
        sup <- as.data.frame(table(as.character(sup_unique[[group_var]])), stringsAsFactors = FALSE)
        names(sup) <- c("group", "support_entities")

        met <- .topology_metric_high_rate_by_group(metric, group_var = group_var, value_var = metric_var, high_cutoff = metric_high_cutoff)

        merged <- merge(evt, sup, by = "group", all = TRUE)
        merged <- merge(merged, met[, c("group", "high_rate"), drop = FALSE], by = "group", all = TRUE)
        merged[is.na(merged)] <- 0

        merged$support_coverage <- mapply(.topology_safe_rate, merged$support_entities, merged$n_entities)
        merged$risk_score <- rowMeans(cbind(merged$rate, 1 - merged$support_coverage, merged$high_rate), na.rm = TRUE)

        merged[, c("group", "rate", "support_coverage", "high_rate", "risk_score"), drop = FALSE]
      }

      prev <- score_by_group(previous)
      cur <- score_by_group(current)
      if (!nrow(prev) || !nrow(cur)) {
        return(data.frame())
      }

      merged <- merge(prev, cur, by = "group", all = TRUE, suffixes = c("_prev", "_cur"))
      merged[is.na(merged)] <- 0
      merged$risk_delta <- merged$risk_score_cur - merged$risk_score_prev

      bad <- merged$risk_delta > risk_delta_threshold
      if (!any(bad)) {
        return(data.frame())
      }

      out <- merged[bad, c("group", "risk_score_prev", "risk_score_cur", "risk_delta", "rate_prev", "rate_cur", "support_coverage_prev", "support_coverage_cur", "high_rate_prev", "high_rate_cur"), drop = FALSE]
      names(out)[names(out) == "group"] <- group_var
      out$threshold <- as.numeric(risk_delta_threshold)
      out$reason <- "Composite cross-domain risk increased beyond threshold"
      out
    }
  )
}
