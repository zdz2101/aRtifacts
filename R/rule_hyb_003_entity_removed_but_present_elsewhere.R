#' HYB-003: Entity removed from primary but present elsewhere
#'
#' @param primary_domain Primary domain name.
#' @param other_domain Other domain checked in current snapshot.
#' @param entity_var Entity identifier variable.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_hyb_003_entity_removed_but_present_elsewhere <- function(primary_domain = "PRIMARY",
                                                              other_domain = "OTHER",
                                                              entity_var = "entity_id",
                                                              owner_team = "Data Stewardship") {
  new_rule(
    id = "HYB-003",
    title = "Entity removed from primary but present elsewhere",
    topology = "A7",
    scope = "record",
    intent = "programming_etl",
    domains = c(primary_domain, other_domain),
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      primary_prev <- .topology_domain_df(previous, primary_domain)
      primary_cur <- .topology_domain_df(current, primary_domain)
      other_cur <- .topology_domain_df(current, other_domain)
      if (is.null(primary_prev) || is.null(primary_cur) || is.null(other_cur)) {
        return(data.frame())
      }

      if (!(entity_var %in% names(primary_prev)) || !(entity_var %in% names(primary_cur)) || !(entity_var %in% names(other_cur))) {
        return(data.frame())
      }

      prev_ids <- unique(primary_prev[, entity_var, drop = FALSE])
      cur_ids <- unique(primary_cur[, entity_var, drop = FALSE])
      removed <- .topology_anti_join_rows(prev_ids, cur_ids, by = entity_var)
      if (!nrow(removed)) {
        return(data.frame())
      }

      other_ids <- unique(other_cur[, entity_var, drop = FALSE])
      bad <- merge(removed, other_ids, by = entity_var, all = FALSE)
      if (!nrow(bad)) {
        return(data.frame())
      }

      bad$primary_domain <- primary_domain
      bad$other_domain <- other_domain
      bad$reason <- "Entity removed from primary domain but still present elsewhere"
      bad
    }
  )
}
