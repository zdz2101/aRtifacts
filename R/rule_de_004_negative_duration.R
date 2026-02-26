#' DE-004: Negative duration
#'
#' @param domain Domain name.
#' @param duration_var Duration variable.
#' @param entity_key_var Optional entity key variable for output.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_de_004_negative_duration <- function(domain,
                                          duration_var,
                                          entity_key_var = "entity_id",
                                          owner_team = "Data Quality") {
  new_rule(
    id = "DE-004",
    title = "Negative duration",
    topology = "A1",
    scope = "record",
    intent = "clinical_review",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      if (!(duration_var %in% names(data))) {
        return(data.frame())
      }

      vals <- suppressWarnings(as.numeric(data[[duration_var]]))
      bad <- !is.na(vals) & vals < 0

      .topology_row_output(
        data = data,
        bad = bad,
        fields = unique(c(entity_key_var, duration_var)),
        reason = "Negative duration"
      )
    }
  )
}
