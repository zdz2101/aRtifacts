#' ID-003: High duplicate rate by site
#'
#' @param domain Domain name.
#' @param key_vars Key variables for duplicate detection.
#' @param site_var Grouping variable.
#' @param threshold Duplicate-rate threshold.
#' @param entity_key_var Optional entity key variable for output.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_id_003_high_duplicate_rate_by_site <- function(domain,
                                                    key_vars,
                                                    site_var = "group_id",
                                                    threshold = 0.1,
                                                    entity_key_var = "entity_id",
                                                    owner_team = "Data Stewardship") {
  new_rule(
    id = "ID-003",
    title = "High duplicate rate by group",
    topology = "A2",
    scope = "group",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      needed <- c(site_var, key_vars)
      if (!all(intersect(needed, names(data)) %in% names(data)) || !(site_var %in% names(data))) {
        return(data.frame())
      }

      present_keys <- intersect(key_vars, names(data))
      if (!length(present_keys)) {
        return(data.frame())
      }
      dup <- duplicated(data[, present_keys, drop = FALSE]) |
        duplicated(data[, present_keys, drop = FALSE], fromLast = TRUE)

      by_site <- tapply(dup, data[[site_var]], mean, na.rm = TRUE)
      bad_sites <- names(by_site)[!is.na(by_site) & by_site > threshold]
      if (!length(bad_sites)) {
        return(data.frame())
      }

      keep <- dup & data[[site_var]] %in% bad_sites
      out_cols <- intersect(unique(c(entity_key_var, site_var, present_keys)), names(data))
      out <- data[keep, out_cols, drop = FALSE]
      out$dup_rate <- unname(by_site[as.character(out[[site_var]])])
      out$reason <- "Group duplicate rate above threshold"
      out
    }
  )
}
