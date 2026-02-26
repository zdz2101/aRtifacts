#' AGG-002: Group count mismatch across domains
#'
#' @param left_domain Left domain name.
#' @param right_domain Right domain name.
#' @param group_var Group variable present in both domains.
#' @param tolerance Allowed absolute difference per group.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_agg_002_group_count_mismatch <- function(left_domain = "LEFT",
                                              right_domain = "RIGHT",
                                              group_var = "group_id",
                                              tolerance = 0,
                                              owner_team = "Data Stewardship") {
  new_rule(
    id = "AGG-002",
    title = "Group count mismatch",
    topology = "A4",
    scope = "group",
    intent = "programming_etl",
    domains = c(left_domain, right_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      left <- .topology_domain_df(domains, left_domain)
      right <- .topology_domain_df(domains, right_domain)
      if (is.null(left) || is.null(right) || !(group_var %in% names(left)) || !(group_var %in% names(right))) {
        return(data.frame())
      }

      left_counts <- .topology_count_by(data.frame(group = as.character(left[[group_var]]), stringsAsFactors = FALSE), by = "group")
      right_counts <- .topology_count_by(data.frame(group = as.character(right[[group_var]]), stringsAsFactors = FALSE), by = "group")
      if (!nrow(left_counts) || !nrow(right_counts)) {
        return(data.frame())
      }

      names(left_counts)[names(left_counts) == "n"] <- "n_left"
      names(right_counts)[names(right_counts) == "n"] <- "n_right"

      merged <- .topology_full_join_by(left_counts, right_counts, by = "group")
      merged$n_left[is.na(merged$n_left)] <- 0L
      merged$n_right[is.na(merged$n_right)] <- 0L
      merged$delta_abs <- abs(merged$n_left - merged$n_right)

      bad <- merged$delta_abs > tolerance
      if (!any(bad)) {
        return(data.frame())
      }

      out <- merged[bad, c("group", "n_left", "n_right", "delta_abs"), drop = FALSE]
      names(out)[names(out) == "group"] <- group_var
      out$threshold <- as.numeric(tolerance)
      out$reason <- "Group counts differ beyond tolerance"
      out
    }
  )
}
