#' TMP-002: Start date after end date
#'
#' @param domain Domain name.
#' @param start_var Start date variable.
#' @param end_var End date variable.
#' @param entity_key_var Optional entity key variable for output.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_tmp_002_start_after_end <- function(domain,
                                         start_var,
                                         end_var,
                                         entity_key_var = "USUBJID",
                                         owner_team = "DM") {
  new_rule(
    id = "TMP-002",
    title = "Start date after end date",
    topology = "A1",
    scope = "record",
    intent = "data_entry",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      needed <- c(start_var, end_var)
      if (!all(needed %in% names(data))) {
        return(data.frame())
      }

      start_date <- as.Date(data[[start_var]])
      end_date <- as.Date(data[[end_var]])
      bad <- !is.na(start_date) & !is.na(end_date) & (start_date > end_date)

      .core50_row_output(
        data = data,
        bad = bad,
        fields = unique(c(entity_key_var, needed)),
        reason = "Start date occurs after end date"
      )
    }
  )
}
