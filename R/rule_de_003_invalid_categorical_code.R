#' DE-003: Invalid categorical code
#'
#' @param domain Domain name.
#' @param value_var Categorical variable.
#' @param allowed_levels Allowed values.
#' @param entity_key_var Optional entity key variable for output.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_de_003_invalid_categorical_code <- function(domain,
                                                 value_var,
                                                 allowed_levels,
                                                 entity_key_var = "USUBJID",
                                                 owner_team = "DM") {
  new_rule(
    id = "DE-003",
    title = "Invalid categorical code",
    topology = "A1",
    scope = "record",
    intent = "data_entry",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      if (!(value_var %in% names(data))) {
        return(data.frame())
      }

      vals <- as.character(data[[value_var]])
      present <- !.core50_any_na_or_blank(vals)
      bad <- present & !(vals %in% allowed_levels)

      .core50_row_output(
        data = data,
        bad = bad,
        fields = unique(c(entity_key_var, value_var)),
        reason = "Invalid categorical code"
      )
    }
  )
}
