#' REL-001: Missing reference records across domains
#'
#' @param primary_domain Domain expected to be fully represented in `reference_domain`.
#' @param reference_domain Domain acting as the reference set.
#' @param key_vars Join keys used for cross-domain reconciliation.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_rel_001_missing_reference_records <- function(primary_domain = "PRIMARY",
                                                   reference_domain = "REFERENCE",
                                                   key_vars = c("entity_id"),
                                                   owner_team = "Data Stewardship") {
  new_rule(
    id = "REL-001",
    title = "Missing reference records",
    topology = "A3",
    scope = "record",
    intent = "core_integrity",
    domains = c(primary_domain, reference_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      primary <- .topology_domain_df(domains, primary_domain)
      reference <- .topology_domain_df(domains, reference_domain)
      if (is.null(primary) || is.null(reference)) {
        return(data.frame())
      }

      by <- intersect(key_vars, intersect(names(primary), names(reference)))
      if (!length(by)) {
        return(data.frame())
      }

      bad <- .topology_anti_join_rows(primary, reference, by = by)
      if (!nrow(bad)) {
        return(data.frame())
      }

      out <- bad[, by, drop = FALSE]
      out$primary_domain <- primary_domain
      out$reference_domain <- reference_domain
      out$reason <- "Primary-domain key not present in reference domain"
      out
    }
  )
}
