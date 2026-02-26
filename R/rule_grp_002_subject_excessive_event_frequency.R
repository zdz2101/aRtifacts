#' GRP-002: Subject with excessive event frequency
#'
#' @param domain Domain name.
#' @param subject_var Subject variable.
#' @param max_events Maximum allowed events per subject.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_grp_002_subject_excessive_event_frequency <- function(domain,
                                                           subject_var = "entity_id",
                                                           max_events = 20,
                                                           owner_team = "Data Operations") {
  new_rule(
    id = "GRP-002",
    title = "Subject with excessive event frequency",
    topology = "A2",
    scope = "group",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      if (!(subject_var %in% names(data))) {
        return(data.frame())
      }

      counts <- table(data[[subject_var]])
      bad <- counts[counts > max_events]
      if (!length(bad)) {
        return(data.frame())
      }

      data.frame(
        subject_id = names(bad),
        event_n = as.integer(unname(bad)),
        threshold = as.integer(max_events),
        reason = "Subject event count above threshold",
        stringsAsFactors = FALSE
      )
    }
  )
}
