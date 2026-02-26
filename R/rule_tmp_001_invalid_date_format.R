#' TMP-001: Invalid date format
#'
#' @param domain Domain name.
#' @param date_var Date variable expected in YYYY-MM-DD format.
#' @param entity_key_var Optional entity key variable for output.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_tmp_001_invalid_date_format <- function(domain,
                                             date_var,
                                             entity_key_var = "entity_id",
                                             owner_team = "Data Stewardship") {
  new_rule(
    id = "TMP-001",
    title = "Invalid date format",
    topology = "A1",
    scope = "record",
    intent = "data_entry",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      if (!(date_var %in% names(data))) {
        return(data.frame())
      }

      bad <- .topology_iso_date_bad(data[[date_var]])
      .topology_row_output(
        data = data,
        bad = bad,
        fields = unique(c(entity_key_var, date_var)),
        reason = "Invalid date format (expected YYYY-MM-DD)"
      )
    }
  )
}
