#' ID-001: Missing key fields
#'
#' @param domain Domain name.
#' @param key_vars Key variables expected to be non-missing/non-blank.
#' @param entity_key_var Optional entity key variable for output.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_id_001_missing_key_fields <- function(domain,
                                           key_vars,
                                           entity_key_var = "entity_id",
                                           owner_team = "Data Stewardship") {
  new_rule(
    id = "ID-001",
    title = "Missing key fields",
    topology = "A1",
    scope = "record",
    intent = "core_integrity",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      existing_keys <- intersect(key_vars, names(data))
      if (!length(existing_keys)) {
        return(data.frame())
      }

      miss_mat <- lapply(existing_keys, function(v) .topology_any_na_or_blank(data[[v]]))
      bad <- Reduce(`|`, miss_mat)
      .topology_row_output(
        data = data,
        bad = bad,
        fields = unique(c(entity_key_var, existing_keys)),
        reason = "Missing key field value"
      )
    }
  )
}
