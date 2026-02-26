#' TMP-004: Time without date
#'
#' @param domain Domain name.
#' @param time_var Time variable.
#' @param date_var Date variable.
#' @param entity_key_var Optional entity key variable for output.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_tmp_004_time_without_date <- function(domain,
                                           time_var,
                                           date_var,
                                           entity_key_var = "entity_id",
                                           owner_team = "Data Stewardship") {
  new_rule(
    id = "TMP-004",
    title = "Time without date",
    topology = "A1",
    scope = "record",
    intent = "data_entry",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      needed <- c(time_var, date_var)
      if (!all(needed %in% names(data))) {
        return(data.frame())
      }

      time_present <- !.topology_any_na_or_blank(data[[time_var]])
      date_missing <- .topology_any_na_or_blank(data[[date_var]])
      bad <- time_present & date_missing

      .topology_row_output(
        data = data,
        bad = bad,
        fields = unique(c(entity_key_var, needed)),
        reason = "Time present while date is missing"
      )
    }
  )
}
