#' Build A5 cross-snapshot row pack
#'
#' @param owner_team Owner team.
#' @param domain_primary Primary domain for snapshot checks.
#' @param keys Identity keys for A5 checks.
#' @param monitored_fields Fields for SNAP-002.
#' @param critical_fields Fields for SNAP-005.
#' @param numeric_tol Numeric tolerance for value changes.
#' @param removed_keys_option Option key for SNAP-003 ledger.
#'
#' @return List of `CoreRule` objects.
#'
#' @details
#' A5 rules are same-domain row-level longitudinal checks and are dispatched as
#' `fn(previous, current, context, rule)`.
#'
#' This pack assembles `SNAP-001` through `SNAP-005` style row-level
#' cross-cut checks.
#' @export
pack_core_50_a5 <- function(owner_team = "Standards",
                            domain_primary = "ADSL",
                            keys = c("USUBJID"),
                            monitored_fields = character(),
                            critical_fields = c("TRT01A"),
                            numeric_tol = 0,
                            removed_keys_option = "removed_keys_history") {
  f_snap_001 <- get("rule_snap_001_unexpected_key_deletions", mode = "function", inherits = TRUE)
  f_snap_002 <- get("rule_snap_002_record_changed_unexpectedly", mode = "function", inherits = TRUE)
  f_snap_003 <- get("rule_snap_003_record_reappeared", mode = "function", inherits = TRUE)
  f_snap_004 <- get("rule_snap_004_identity_drift", mode = "function", inherits = TRUE)
  f_snap_005 <- get("rule_snap_005_critical_field_modified_post_lock", mode = "function", inherits = TRUE)

  list(
    f_snap_001(
      domain = domain_primary,
      keys = keys,
      owner_team = owner_team
    ),
    f_snap_002(
      domain = domain_primary,
      keys = keys,
      monitored_fields = monitored_fields,
      numeric_tol = numeric_tol,
      owner_team = owner_team
    ),
    f_snap_003(
      domain = domain_primary,
      keys = keys,
      removed_keys_option = removed_keys_option,
      owner_team = "Clinical Ops"
    ),
    f_snap_004(
      domain = "AE",
      stable_fields = c("USUBJID", "AETERM", "AESTDTC"),
      identity_fields = c("AESEQ"),
      owner_team = owner_team
    ),
    f_snap_005(
      domain = domain_primary,
      keys = keys,
      critical_fields = critical_fields,
      numeric_tol = numeric_tol,
      owner_team = owner_team
    )
  )
}

#' Register A5 cross-snapshot row rules
#'
#' @inheritParams pack_core_50_a5
#' @param overwrite Whether to overwrite existing IDs.
#'
#' @return Invisibly returns registered IDs.
#'
#' @details
#' Equivalent to `pack_core_50_a5(...)` followed by `register_rule()` for each
#' produced rule.
#' @export
register_core_50_a5 <- function(owner_team = "Standards",
                                domain_primary = "ADSL",
                                keys = c("USUBJID"),
                                monitored_fields = character(),
                                critical_fields = c("TRT01A"),
                                numeric_tol = 0,
                                removed_keys_option = "removed_keys_history",
                                overwrite = FALSE) {
  rules <- pack_core_50_a5(
    owner_team = owner_team,
    domain_primary = domain_primary,
    keys = keys,
    monitored_fields = monitored_fields,
    critical_fields = critical_fields,
    numeric_tol = numeric_tol,
    removed_keys_option = removed_keys_option
  )

  ids <- vapply(rules, function(rule) register_rule(rule, overwrite = overwrite), character(1))
  invisible(ids)
}
