#' Build A4 cross-domain aggregate starter pack
#'
#' @param owner_team Owner team.
#' @param left_domain Left domain name.
#' @param right_domain Right domain name.
#' @param entity_var Entity variable for count reconciliation.
#' @param group_var Group variable for grouped checks.
#' @param entity_count_tolerance Tolerance for AGG-001.
#' @param group_count_tolerance Tolerance for AGG-002.
#' @param completeness_domains Domains for AGG-003.
#' @param completeness_gap_threshold Gap threshold for AGG-003.
#' @param event_domain Event domain for AGG-004.
#' @param support_domain Support domain for AGG-004.
#' @param event_high_threshold Event-rate threshold for AGG-004.
#' @param support_low_threshold Support-rate threshold for AGG-004.
#'
#' @return List of `CoreRule` objects.
#' @export
pack_topology_a4 <- function(owner_team = "Data Operations",
                             left_domain = "LEFT",
                             right_domain = "RIGHT",
                             entity_var = "entity_id",
                             group_var = "group_id",
                             entity_count_tolerance = 0,
                             group_count_tolerance = 0,
                             completeness_domains = c("PRIMARY", "SUPPORT", "AUX"),
                             completeness_gap_threshold = 0.2,
                             event_domain = "EVENTS",
                             support_domain = "SUPPORT",
                             event_high_threshold = 0.5,
                             support_low_threshold = 0.5) {
  list(
    rule_agg_001_distinct_entity_count_mismatch(
      left_domain = left_domain,
      right_domain = right_domain,
      entity_var = entity_var,
      tolerance = entity_count_tolerance,
      owner_team = owner_team
    ),
    rule_agg_002_group_count_mismatch(
      left_domain = left_domain,
      right_domain = right_domain,
      group_var = group_var,
      tolerance = group_count_tolerance,
      owner_team = owner_team
    ),
    rule_agg_003_domain_completeness_imbalance(
      domains_set = completeness_domains,
      group_var = group_var,
      threshold_max_gap = completeness_gap_threshold,
      owner_team = owner_team
    ),
    rule_agg_004_event_support_rate_discrepancy(
      event_domain = event_domain,
      support_domain = support_domain,
      group_var = group_var,
      entity_var = entity_var,
      event_high_threshold = event_high_threshold,
      support_low_threshold = support_low_threshold,
      owner_team = owner_team
    )
  )
}

#' Register A4 cross-domain aggregate starter rules
#'
#' @inheritParams pack_topology_a4
#' @param overwrite Whether to overwrite existing IDs.
#'
#' @return Invisibly returns registered IDs.
#' @export
register_topology_a4 <- function(owner_team = "Data Operations",
                                 left_domain = "LEFT",
                                 right_domain = "RIGHT",
                                 entity_var = "entity_id",
                                 group_var = "group_id",
                                 entity_count_tolerance = 0,
                                 group_count_tolerance = 0,
                                 completeness_domains = c("PRIMARY", "SUPPORT", "AUX"),
                                 completeness_gap_threshold = 0.2,
                                 event_domain = "EVENTS",
                                 support_domain = "SUPPORT",
                                 event_high_threshold = 0.5,
                                 support_low_threshold = 0.5,
                                 overwrite = FALSE) {
  rules <- pack_topology_a4(
    owner_team = owner_team,
    left_domain = left_domain,
    right_domain = right_domain,
    entity_var = entity_var,
    group_var = group_var,
    entity_count_tolerance = entity_count_tolerance,
    group_count_tolerance = group_count_tolerance,
    completeness_domains = completeness_domains,
    completeness_gap_threshold = completeness_gap_threshold,
    event_domain = event_domain,
    support_domain = support_domain,
    event_high_threshold = event_high_threshold,
    support_low_threshold = support_low_threshold
  )

  ids <- vapply(rules, function(rule) register_rule(rule, overwrite = overwrite), character(1))
  invisible(ids)
}
