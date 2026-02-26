#' Build A7 cross-domain + cross-snapshot starter pack
#'
#' @param owner_team Owner team.
#' @param primary_domain Primary domain for HYB-001/HYB-003.
#' @param support_domain Support domain for HYB-001/HYB-004.
#' @param primary_keys Identity keys for HYB-001.
#' @param match_keys Match keys for HYB-001.
#' @param other_domain Other domain for HYB-003.
#' @param driver_domain Driver domain for HYB-002.
#' @param metric_domain Metric domain for HYB-002/HYB-004.
#' @param entity_var Entity variable.
#' @param driver_var Driver variable.
#' @param metric_var Metric variable.
#' @param driver_delta_min Driver threshold for HYB-002.
#' @param metric_delta_min Metric threshold for HYB-002.
#' @param events_domain Events domain for HYB-004.
#' @param group_var Group variable for HYB-004.
#' @param metric_high_cutoff High metric threshold for HYB-004.
#' @param risk_delta_threshold Risk delta threshold for HYB-004.
#'
#' @return List of `CoreRule` objects.
#' @export
pack_topology_a7 <- function(owner_team = "Data Operations",
                             primary_domain = "PRIMARY",
                             support_domain = "SUPPORT",
                             primary_keys = c("entity_id", "record_id"),
                             match_keys = c("entity_id"),
                             other_domain = "OTHER",
                             driver_domain = "DRIVER",
                             metric_domain = "METRIC",
                             entity_var = "entity_id",
                             driver_var = "driver_value",
                             metric_var = "metric_value",
                             driver_delta_min = 0,
                             metric_delta_min = 0,
                             events_domain = "EVENTS",
                             group_var = "group_id",
                             metric_high_cutoff = 100,
                             risk_delta_threshold = 0.3) {
  list(
    rule_hyb_001_new_primary_without_new_support(
      primary_domain = primary_domain,
      support_domain = support_domain,
      primary_keys = primary_keys,
      match_keys = match_keys,
      owner_team = owner_team
    ),
    rule_hyb_002_driver_change_with_metric_worsening(
      driver_domain = driver_domain,
      metric_domain = metric_domain,
      entity_var = entity_var,
      driver_var = driver_var,
      metric_var = metric_var,
      driver_delta_min = driver_delta_min,
      metric_delta_min = metric_delta_min,
      owner_team = owner_team
    ),
    rule_hyb_003_entity_removed_but_present_elsewhere(
      primary_domain = primary_domain,
      other_domain = other_domain,
      entity_var = entity_var,
      owner_team = owner_team
    ),
    rule_hyb_004_cross_domain_risk_escalation(
      events_domain = events_domain,
      support_domain = support_domain,
      metric_domain = metric_domain,
      group_var = group_var,
      entity_var = entity_var,
      metric_var = metric_var,
      metric_high_cutoff = metric_high_cutoff,
      risk_delta_threshold = risk_delta_threshold,
      owner_team = owner_team
    )
  )
}

#' Register A7 cross-domain + cross-snapshot starter rules
#'
#' @inheritParams pack_topology_a7
#' @param overwrite Whether to overwrite existing IDs.
#'
#' @return Invisibly returns registered IDs.
#' @export
register_topology_a7 <- function(owner_team = "Data Operations",
                                 primary_domain = "PRIMARY",
                                 support_domain = "SUPPORT",
                                 primary_keys = c("entity_id", "record_id"),
                                 match_keys = c("entity_id"),
                                 other_domain = "OTHER",
                                 driver_domain = "DRIVER",
                                 metric_domain = "METRIC",
                                 entity_var = "entity_id",
                                 driver_var = "driver_value",
                                 metric_var = "metric_value",
                                 driver_delta_min = 0,
                                 metric_delta_min = 0,
                                 events_domain = "EVENTS",
                                 group_var = "group_id",
                                 metric_high_cutoff = 100,
                                 risk_delta_threshold = 0.3,
                                 overwrite = FALSE) {
  rules <- pack_topology_a7(
    owner_team = owner_team,
    primary_domain = primary_domain,
    support_domain = support_domain,
    primary_keys = primary_keys,
    match_keys = match_keys,
    other_domain = other_domain,
    driver_domain = driver_domain,
    metric_domain = metric_domain,
    entity_var = entity_var,
    driver_var = driver_var,
    metric_var = metric_var,
    driver_delta_min = driver_delta_min,
    metric_delta_min = metric_delta_min,
    events_domain = events_domain,
    group_var = group_var,
    metric_high_cutoff = metric_high_cutoff,
    risk_delta_threshold = risk_delta_threshold
  )

  ids <- vapply(rules, function(rule) register_rule(rule, overwrite = overwrite), character(1))
  invisible(ids)
}
