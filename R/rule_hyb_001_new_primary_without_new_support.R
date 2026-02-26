#' HYB-001: New primary records without new support
#'
#' @param primary_domain Primary domain tracked across snapshots.
#' @param support_domain Supporting domain expected to co-evolve.
#' @param primary_keys Keys defining a primary record.
#' @param match_keys Keys used to match primary to support records.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_hyb_001_new_primary_without_new_support <- function(primary_domain = "PRIMARY",
                                                         support_domain = "SUPPORT",
                                                         primary_keys = c("entity_id", "record_id"),
                                                         match_keys = c("entity_id"),
                                                         owner_team = "Data Operations") {
  new_rule(
    id = "HYB-001",
    title = "New primary records without new support",
    topology = "A7",
    scope = "record",
    intent = "ops_anomaly",
    domains = c(primary_domain, support_domain),
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      primary_prev <- .topology_domain_df(previous, primary_domain)
      primary_cur <- .topology_domain_df(current, primary_domain)
      support_cur <- .topology_domain_df(current, support_domain)
      if (is.null(primary_prev) || is.null(primary_cur) || is.null(support_cur)) {
        return(data.frame())
      }

      by_primary <- intersect(primary_keys, intersect(names(primary_prev), names(primary_cur)))
      if (!length(by_primary)) {
        return(data.frame())
      }

      new_primary <- .topology_anti_join_rows(primary_cur, primary_prev, by = by_primary)
      if (!nrow(new_primary)) {
        return(data.frame())
      }

      by_match <- intersect(match_keys, intersect(names(new_primary), names(support_cur)))
      if (!length(by_match)) {
        return(data.frame())
      }

      bad <- .topology_anti_join_rows(new_primary, support_cur, by = by_match)
      if (!nrow(bad)) {
        return(data.frame())
      }

      out <- bad[, unique(c(by_primary, by_match)), drop = FALSE]
      out$primary_domain <- primary_domain
      out$support_domain <- support_domain
      out$reason <- "New primary record has no matching support record"
      out
    }
  )
}
