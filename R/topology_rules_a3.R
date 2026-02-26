#' Build A3 cross-domain relational starter pack
#'
#' @param owner_team Owner team.
#' @param primary_domain Domain expected to reference `reference_domain`.
#' @param reference_domain Domain used as relational reference.
#' @param child_domain Child domain for orphan detection.
#' @param parent_domain Parent domain for orphan detection.
#' @param parent_keys Parent key columns.
#' @param left_domain Left domain for pair mismatch checks.
#' @param right_domain Right domain for pair mismatch checks.
#' @param entity_var Entity variable for pair mismatch checks.
#' @param left_group_var Group variable in `left_domain`.
#' @param right_group_var Group variable in `right_domain`.
#' @param event_domain Event domain for window checks.
#' @param window_domain Window domain for window checks.
#' @param link_keys Link keys for event/window alignment.
#' @param event_time_var Event timestamp variable.
#' @param window_start_var Window start variable.
#' @param window_end_var Window end variable.
#' @param window_days Allowed window extension in days.
#'
#' @return List of `CoreRule` objects.
#' @export
pack_topology_a3 <- function(owner_team = "Data Stewardship",
                             primary_domain = "PRIMARY",
                             reference_domain = "REFERENCE",
                             child_domain = "CHILD",
                             parent_domain = "PARENT",
                             parent_keys = c("entity_id"),
                             left_domain = "LEFT",
                             right_domain = "RIGHT",
                             entity_var = "entity_id",
                             left_group_var = "group_id",
                             right_group_var = "group_id",
                             event_domain = "EVENTS",
                             window_domain = "WINDOW",
                             link_keys = c("entity_id"),
                             event_time_var = "event_time",
                             window_start_var = "window_start",
                             window_end_var = "window_end",
                             window_days = 0) {
  list(
    rule_rel_001_missing_reference_records(
      primary_domain = primary_domain,
      reference_domain = reference_domain,
      key_vars = link_keys,
      owner_team = owner_team
    ),
    rule_rel_002_orphan_child_records(
      child_domain = child_domain,
      parent_domain = parent_domain,
      parent_keys = parent_keys,
      owner_team = owner_team
    ),
    rule_rel_003_group_key_mismatch_across_domains(
      left_domain = left_domain,
      right_domain = right_domain,
      entity_var = entity_var,
      left_group_var = left_group_var,
      right_group_var = right_group_var,
      owner_team = owner_team
    ),
    rule_rel_004_timestamp_outside_linked_window(
      event_domain = event_domain,
      window_domain = window_domain,
      link_keys = link_keys,
      event_time_var = event_time_var,
      window_start_var = window_start_var,
      window_end_var = window_end_var,
      window_days = window_days,
      owner_team = owner_team
    )
  )
}

#' Register A3 cross-domain relational starter rules
#'
#' @inheritParams pack_topology_a3
#' @param overwrite Whether to overwrite existing IDs.
#'
#' @return Invisibly returns registered IDs.
#' @export
register_topology_a3 <- function(owner_team = "Data Stewardship",
                                 primary_domain = "PRIMARY",
                                 reference_domain = "REFERENCE",
                                 child_domain = "CHILD",
                                 parent_domain = "PARENT",
                                 parent_keys = c("entity_id"),
                                 left_domain = "LEFT",
                                 right_domain = "RIGHT",
                                 entity_var = "entity_id",
                                 left_group_var = "group_id",
                                 right_group_var = "group_id",
                                 event_domain = "EVENTS",
                                 window_domain = "WINDOW",
                                 link_keys = c("entity_id"),
                                 event_time_var = "event_time",
                                 window_start_var = "window_start",
                                 window_end_var = "window_end",
                                 window_days = 0,
                                 overwrite = FALSE) {
  rules <- pack_topology_a3(
    owner_team = owner_team,
    primary_domain = primary_domain,
    reference_domain = reference_domain,
    child_domain = child_domain,
    parent_domain = parent_domain,
    parent_keys = parent_keys,
    left_domain = left_domain,
    right_domain = right_domain,
    entity_var = entity_var,
    left_group_var = left_group_var,
    right_group_var = right_group_var,
    event_domain = event_domain,
    window_domain = window_domain,
    link_keys = link_keys,
    event_time_var = event_time_var,
    window_start_var = window_start_var,
    window_end_var = window_end_var,
    window_days = window_days
  )

  ids <- vapply(rules, function(rule) register_rule(rule, overwrite = overwrite), character(1))
  invisible(ids)
}
