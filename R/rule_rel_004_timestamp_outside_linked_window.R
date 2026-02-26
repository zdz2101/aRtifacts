#' REL-004: Timestamp outside linked window
#'
#' @param event_domain Domain containing event timestamps.
#' @param window_domain Domain containing start/end window columns.
#' @param link_keys Keys used to align event records to window records.
#' @param event_time_var Timestamp/date variable in `event_domain`.
#' @param window_start_var Window start variable in `window_domain`.
#' @param window_end_var Window end variable in `window_domain`.
#' @param window_days Allowed extension (days) on each side of the linked window.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_rel_004_timestamp_outside_linked_window <- function(event_domain = "EVENTS",
                                                         window_domain = "WINDOW",
                                                         link_keys = c("entity_id"),
                                                         event_time_var = "event_time",
                                                         window_start_var = "window_start",
                                                         window_end_var = "window_end",
                                                         window_days = 0,
                                                         owner_team = "Data Operations") {
  new_rule(
    id = "REL-004",
    title = "Timestamp outside linked window",
    topology = "A3",
    scope = "record",
    intent = "data_entry",
    domains = c(event_domain, window_domain),
    owner_team = owner_team,
    match_spec = new_match_spec(by = link_keys, join_type = "inner", window_days = window_days),
    fn = function(domains, context, rule) {
      events <- .topology_domain_df(domains, event_domain)
      window <- .topology_domain_df(domains, window_domain)
      if (is.null(events) || is.null(window)) {
        return(data.frame())
      }

      needed_events <- c(link_keys, event_time_var)
      needed_window <- c(link_keys, window_start_var, window_end_var)
      if (!all(needed_events %in% names(events)) || !all(needed_window %in% names(window))) {
        return(data.frame())
      }

      event_dt <- .topology_parse_date(events[[event_time_var]])
      win_start <- .topology_parse_date(window[[window_start_var]])
      win_end <- .topology_parse_date(window[[window_end_var]])

      bad <- rep(FALSE, nrow(events))
      for (i in seq_len(nrow(events))) {
        rel <- rep(TRUE, nrow(window))
        for (k in link_keys) {
          rel <- rel & (as.character(window[[k]]) == as.character(events[[k]][[i]]))
        }
        if (!any(rel)) {
          next
        }

        bad[[i]] <- !.topology_within_any_window(
          index_date = event_dt[[i]],
          start_dates = win_start[rel],
          end_dates = win_end[rel],
          window_days = window_days
        )
      }

      if (!any(bad)) {
        return(data.frame())
      }

      out <- events[bad, needed_events, drop = FALSE]
      out$event_domain <- event_domain
      out$window_domain <- window_domain
      out$reason <- "Event timestamp outside linked window"
      out
    }
  )
}
