#' AGG-004: Event-support rate discrepancy
#'
#' @param event_domain Event domain name.
#' @param support_domain Support domain name.
#' @param group_var Group variable.
#' @param entity_var Entity identifier variable.
#' @param event_high_threshold High-threshold for events per entity.
#' @param support_low_threshold Low-threshold for support coverage.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_agg_004_event_support_rate_discrepancy <- function(event_domain = "EVENTS",
                                                        support_domain = "SUPPORT",
                                                        group_var = "group_id",
                                                        entity_var = "entity_id",
                                                        event_high_threshold = 0.5,
                                                        support_low_threshold = 0.5,
                                                        owner_team = "Data Operations") {
  new_rule(
    id = "AGG-004",
    title = "Event-support rate discrepancy",
    topology = "A4",
    scope = "group",
    intent = "ops_anomaly",
    domains = c(event_domain, support_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      events <- .topology_domain_df(domains, event_domain)
      support <- .topology_domain_df(domains, support_domain)
      needed <- c(group_var, entity_var)
      if (is.null(events) || is.null(support) || !all(needed %in% names(events)) || !all(needed %in% names(support))) {
        return(data.frame())
      }

      evt <- .topology_events_per_entity_by_group(events, group_var = group_var, entity_var = entity_var)
      sup_ent <- unique(support[, c(group_var, entity_var), drop = FALSE])
      sup <- as.data.frame(table(as.character(sup_ent[[group_var]])), stringsAsFactors = FALSE)
      names(sup) <- c("group", "support_entities")

      merged <- merge(evt, sup, by = "group", all = TRUE)
      merged[is.na(merged)] <- 0
      merged$support_rate <- mapply(.topology_safe_rate, merged$support_entities, merged$n_entities)

      bad <- !is.na(merged$rate) & !is.na(merged$support_rate) &
        merged$rate > event_high_threshold & merged$support_rate < support_low_threshold
      if (!any(bad)) {
        return(data.frame())
      }

      out <- merged[bad, c("group", "n_events", "n_entities", "rate", "support_entities", "support_rate"), drop = FALSE]
      names(out)[names(out) == "group"] <- group_var
      out$event_threshold <- as.numeric(event_high_threshold)
      out$support_threshold <- as.numeric(support_low_threshold)
      out$reason <- "High event rate with low support coverage"
      out
    }
  )
}
