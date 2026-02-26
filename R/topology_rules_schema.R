#' SCH-001: New columns added
#'
#' Flags newly introduced columns between previous and current cuts.
#'
#' @param domain Domain name.
#'
#' @return A `CoreRule` object.
#' @export
rule_sch_001_new_columns_added <- function(domain) {
  new_rule(
    id = "SCH-001",
    title = "New columns added",
    topology = "A0",
    scope = "snapshot",
    intent = "programming_etl",
    domain = domain,
    fn = function(domain, context, rule) {
      prev_dom <- .resolve_previous_domain_by_name(context, domain@name)
      if (is.null(prev_dom)) {
        return(data.frame())
      }

      col_diff <- diff_columns(prev_dom, domain)
      added <- col_diff[col_diff$status == "added", c("column"), drop = FALSE]
      if (!nrow(added)) {
        return(data.frame())
      }

      data.frame(
        domain_name = domain@name,
        column = added$column,
        change = "added",
        stringsAsFactors = FALSE
      )
    }
  )
}

#' SCH-002: Columns removed
#'
#' Flags columns that disappeared from previous to current cut.
#'
#' @param domain Domain name.
#'
#' @return A `CoreRule` object.
#' @export
rule_sch_002_columns_removed <- function(domain) {
  new_rule(
    id = "SCH-002",
    title = "Columns removed",
    topology = "A0",
    scope = "snapshot",
    intent = "programming_etl",
    domain = domain,
    fn = function(domain, context, rule) {
      prev_dom <- .resolve_previous_domain_by_name(context, domain@name)
      if (is.null(prev_dom)) {
        return(data.frame())
      }

      col_diff <- diff_columns(prev_dom, domain)
      removed <- col_diff[col_diff$status == "removed", c("column"), drop = FALSE]
      if (!nrow(removed)) {
        return(data.frame())
      }

      data.frame(
        domain_name = domain@name,
        column = removed$column,
        change = "removed",
        stringsAsFactors = FALSE
      )
    }
  )
}

#' SCH-003: Column type drift
#'
#' Flags shared columns whose storage type changed between cuts.
#'
#' @param domain Domain name.
#'
#' @return A `CoreRule` object.
#' @export
rule_sch_003_column_type_drift <- function(domain) {
  new_rule(
    id = "SCH-003",
    title = "Column type drift",
    topology = "A0",
    scope = "snapshot",
    intent = "programming_etl",
    domain = domain,
    fn = function(domain, context, rule) {
      prev_dom <- .resolve_previous_domain_by_name(context, domain@name)
      if (is.null(prev_dom)) {
        return(data.frame())
      }

      type_diff <- diff_column_types(prev_dom, domain)
      changed <- type_diff[type_diff$changed, c("column", "old_type", "new_type"), drop = FALSE]
      if (!nrow(changed)) {
        return(data.frame())
      }

      changed$domain_name <- domain@name
      changed[, c("domain_name", "column", "old_type", "new_type"), drop = FALSE]
    }
  )
}

#' Build A0 schema pack
#'
#' Returns topology starter schema rules.
#'
#' @param domain Domain name.
#'
#' @return A list of `CoreRule` objects.
#'
#' @details
#' A0 rules evaluate schema/snapshot-level change on a single domain and are
#' typically run with both `current` and `previous` artifacts in
#' `new_rule_context()`.
#'
#' This pack includes:
#' - `SCH-001`: new columns added,
#' - `SCH-002`: columns removed,
#' - `SCH-003`: column type drift.
#' @export
pack_topology_a0 <- function(domain) {
  list(
    rule_sch_001_new_columns_added(domain),
    rule_sch_002_columns_removed(domain),
    rule_sch_003_column_type_drift(domain)
  )
}

#' Register A0 schema rules
#'
#' Registers the schema pack into the rule registry.
#'
#' @param domain Domain name.
#' @param overwrite Whether to overwrite existing IDs.
#'
#' @return Invisibly returns the registered IDs.
#'
#' @details
#' Equivalent to `pack_topology_a0(domain)` followed by `register_rule()` on
#' each returned rule.
#' @export
register_topology_a0 <- function(domain, overwrite = FALSE) {
  rules <- pack_topology_a0(domain)
  ids <- vapply(
    rules,
    function(rule) register_rule(rule, overwrite = overwrite),
    character(1)
  )
  invisible(ids)
}

.resolve_previous_domain_by_name <- function(context, domain_name) {
  if (is.null(context@previous) || !(domain_name %in% names(context@previous@domains))) {
    return(NULL)
  }

  context@previous@domains[[domain_name]]
}
