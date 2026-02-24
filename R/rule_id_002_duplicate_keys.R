#' ID-002: Duplicate keys
#'
#' @param domain Domain name.
#' @param key_vars Key variables expected to uniquely identify records.
#' @param entity_key_var Optional entity key variable for output.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_id_002_duplicate_keys <- function(domain,
                                       key_vars,
                                       entity_key_var = "USUBJID",
                                       owner_team = "DM") {
  new_rule(
    id = "ID-002",
    title = "Duplicate keys",
    topology = "A2",
    scope = "group",
    intent = "core_integrity",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      present <- intersect(key_vars, names(data))
      if (!length(present)) {
        return(data.frame())
      }

      key_df <- data[, present, drop = FALSE]
      dup <- duplicated(key_df) | duplicated(key_df, fromLast = TRUE)
      if (!any(dup)) {
        return(data.frame())
      }

      out <- data[dup, unique(c(entity_key_var, present)), drop = FALSE]
      out$row_id <- which(dup)
      out$reason <- "Duplicate key combination"
      out
    }
  )
}
