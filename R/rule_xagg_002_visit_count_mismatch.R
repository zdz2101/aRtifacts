#' XAGG-002: Visit count mismatch
#'
#' @param left_domain First domain (e.g., VS).
#' @param right_domain Second domain (e.g., LB).
#' @param group_var Group variable (e.g., SITEID).
#' @param visit_var Visit variable.
#' @param tolerance Allowed absolute count difference by group.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_xagg_002_visit_count_mismatch <- function(left_domain = "VS",
                                               right_domain = "LB",
                                               group_var = "SITEID",
                                               visit_var = "VISIT",
                                               tolerance = 1,
                                               owner_team = "Standards") {
  new_rule(
    id = "XAGG-002",
    title = "Visit count mismatch",
    topology = "A4",
    scope = "group",
    intent = "programming_etl",
    domains = c(left_domain, right_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      left <- .core50_domain_df(domains, left_domain)
      right <- .core50_domain_df(domains, right_domain)
      needed <- c(group_var, visit_var)
      if (is.null(left) || is.null(right) || !all(needed %in% names(left)) || !all(needed %in% names(right))) {
        return(data.frame())
      }

      left_counts <- .core50_count_by(unique(left[, needed, drop = FALSE]), by = group_var)
      names(left_counts)[names(left_counts) == "n"] <- "n_left"
      right_counts <- .core50_count_by(unique(right[, needed, drop = FALSE]), by = group_var)
      names(right_counts)[names(right_counts) == "n"] <- "n_right"

      merged <- .core50_full_join_by(left_counts, right_counts, by = group_var)
      if (!nrow(merged)) {
        return(data.frame())
      }

      merged$n_left[is.na(merged$n_left)] <- 0L
      merged$n_right[is.na(merged$n_right)] <- 0L
      merged$diff_abs <- abs(merged$n_left - merged$n_right)
      bad <- merged$diff_abs > tolerance
      if (!any(bad)) {
        return(data.frame())
      }

      out <- merged[bad, c(group_var, "n_left", "n_right", "diff_abs"), drop = FALSE]
      out$tolerance <- as.numeric(tolerance)
      out$reason <- "Visit count mismatch by group"
      out
    }
  )
}
