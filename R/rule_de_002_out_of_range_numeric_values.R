#' DE-002: Out-of-range numeric values
#'
#' @param domain Domain name.
#' @param value_var Numeric variable to evaluate.
#' @param lower Lower bound (inclusive).
#' @param upper Upper bound (inclusive).
#' @param entity_key_var Optional entity key variable for output.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_de_002_out_of_range_numeric_values <- function(domain,
                                                    value_var,
                                                    lower,
                                                    upper,
                                                    entity_key_var = "entity_id",
                                                    owner_team = "Data Quality") {
  new_rule(
    id = "DE-002",
    title = "Out-of-range numeric values",
    topology = "A1",
    scope = "record",
    intent = "clinical_review",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      if (!(value_var %in% names(data))) {
        return(data.frame())
      }

      vals <- suppressWarnings(as.numeric(data[[value_var]]))
      bad <- !is.na(vals) & (vals < lower | vals > upper)

      .topology_row_output(
        data = data,
        bad = bad,
        fields = unique(c(entity_key_var, value_var)),
        reason = "Numeric value outside allowed range"
      )
    }
  )
}
