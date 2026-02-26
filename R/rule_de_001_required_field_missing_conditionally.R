#' DE-001: Required field missing conditionally
#'
#' @param domain Domain name.
#' @param condition_var Condition variable.
#' @param required_var Required variable when condition is met.
#' @param condition_value Value in `condition_var` that triggers requirement.
#' @param entity_key_var Optional entity key variable for output.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_de_001_required_field_missing_conditionally <- function(domain,
                                                             condition_var,
                                                             required_var,
                                                             condition_value = "Y",
                                                             entity_key_var = "entity_id",
                                                             owner_team = "Data Stewardship") {
  new_rule(
    id = "DE-001",
    title = "Required field missing conditionally",
    topology = "A1",
    scope = "record",
    intent = "data_entry",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      needed <- c(condition_var, required_var)
      if (!all(needed %in% names(data))) {
        return(data.frame())
      }

      cond_hit <- as.character(data[[condition_var]]) == as.character(condition_value)
      req_missing <- .topology_any_na_or_blank(data[[required_var]])
      bad <- !is.na(cond_hit) & cond_hit & req_missing

      .topology_row_output(
        data = data,
        bad = bad,
        fields = unique(c(entity_key_var, needed)),
        reason = "Condition met but required field is missing"
      )
    }
  )
}
