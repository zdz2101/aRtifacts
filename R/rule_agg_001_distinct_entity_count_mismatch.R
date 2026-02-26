#' AGG-001: Distinct entity count mismatch
#'
#' @param left_domain Left domain name.
#' @param right_domain Right domain name.
#' @param entity_var Entity identifier variable.
#' @param tolerance Allowed absolute difference in counts.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_agg_001_distinct_entity_count_mismatch <- function(left_domain = "LEFT",
                                                        right_domain = "RIGHT",
                                                        entity_var = "entity_id",
                                                        tolerance = 0,
                                                        owner_team = "Data Stewardship") {
  new_rule(
    id = "AGG-001",
    title = "Distinct entity count mismatch",
    topology = "A4",
    scope = "snapshot",
    intent = "core_integrity",
    domains = c(left_domain, right_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      left <- .topology_domain_df(domains, left_domain)
      right <- .topology_domain_df(domains, right_domain)
      if (is.null(left) || is.null(right) || !(entity_var %in% names(left)) || !(entity_var %in% names(right))) {
        return(data.frame())
      }

      n_left <- .topology_distinct_count(left, entity_var)
      n_right <- .topology_distinct_count(right, entity_var)
      delta_abs <- abs(n_left - n_right)

      if (is.na(delta_abs) || delta_abs <= tolerance) {
        return(data.frame())
      }

      data.frame(
        left_domain = left_domain,
        right_domain = right_domain,
        entity_var = entity_var,
        n_left = as.integer(n_left),
        n_right = as.integer(n_right),
        delta_abs = as.integer(delta_abs),
        threshold = as.numeric(tolerance),
        reason = "Distinct entity counts differ beyond tolerance",
        stringsAsFactors = FALSE
      )
    }
  )
}
