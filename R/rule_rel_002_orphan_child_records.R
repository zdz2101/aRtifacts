#' REL-002: Orphan child records
#'
#' @param child_domain Child domain name.
#' @param parent_domain Parent domain name.
#' @param parent_keys Parent keys expected in both domains.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_rel_002_orphan_child_records <- function(child_domain = "CHILD",
                                              parent_domain = "PARENT",
                                              parent_keys = c("entity_id"),
                                              owner_team = "Data Stewardship") {
  new_rule(
    id = "REL-002",
    title = "Orphan child records",
    topology = "A3",
    scope = "record",
    intent = "core_integrity",
    domains = c(child_domain, parent_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      child <- .topology_domain_df(domains, child_domain)
      parent <- .topology_domain_df(domains, parent_domain)
      if (is.null(child) || is.null(parent)) {
        return(data.frame())
      }

      by <- intersect(parent_keys, intersect(names(child), names(parent)))
      if (!length(by)) {
        return(data.frame())
      }

      orphan <- .topology_anti_join_rows(child, parent, by = by)
      if (!nrow(orphan)) {
        return(data.frame())
      }

      out <- orphan[, by, drop = FALSE]
      out$child_domain <- child_domain
      out$parent_domain <- parent_domain
      out$reason <- "Child record has no matching parent key"
      out
    }
  )
}
