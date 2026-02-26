#' DE-005: Impossible logical combination
#'
#' @param domain Domain name.
#' @param var_a First variable.
#' @param val_a Disallowed value for `var_a` in combination.
#' @param var_b Second variable.
#' @param val_b Disallowed value for `var_b` in combination.
#' @param entity_key_var Optional entity key variable for output.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_de_005_impossible_logical_combination <- function(domain,
                                                       var_a = "SEX",
                                                       val_a = "M",
                                                       var_b = "PREGNANT",
                                                       val_b = "Y",
                                                       entity_key_var = "entity_id",
                                                       owner_team = "Data Quality") {
  new_rule(
    id = "DE-005",
    title = "Impossible logical combination",
    topology = "A1",
    scope = "record",
    intent = "clinical_review",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      needed <- c(var_a, var_b)
      if (!all(needed %in% names(data))) {
        return(data.frame())
      }

      a_vals <- as.character(data[[var_a]])
      b_vals <- as.character(data[[var_b]])
      bad <- !is.na(a_vals) & !is.na(b_vals) & (a_vals == as.character(val_a)) & (b_vals == as.character(val_b))

      .topology_row_output(
        data = data,
        bad = bad,
        fields = unique(c(entity_key_var, needed)),
        reason = "Impossible logical combination"
      )
    }
  )
}
