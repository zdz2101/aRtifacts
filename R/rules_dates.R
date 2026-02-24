# ---------------------------------------------------------------------------
# Module 2 — Date & Time Consistency (R011–R018)
#
# R019 (large date gaps for scheduled visits) requires a visit schedule
# reference that is outside the scope of a single-domain rule pack.
# R020 (timezone inconsistency) requires ISO 8601 datetime strings with
# offset information — not generalizable without additional assumptions.
# Both are noted in the catalog but not implemented here.
# ---------------------------------------------------------------------------

#' Rule pack — date and time consistency
#'
#' Builds executable `FlagRule` objects for the date and time integrity
#' checks from the core observability rule catalog.
#'
#' | Rule ID | Name | Scope |
#' |---|---|---|
#' | R011 | Start date after end date | Record |
#' | R012 | Event date outside study period | Record |
#' | R013 | Invalid or partial date format | Record |
#' | R014 | End date present but start missing | Record |
#' | R015 | Time present without date | Record |
#' | R016 | Visit dates out of order within subject | Group |
#' | R017 | Identical timestamps across multiple records | Group |
#' | R018 | Future-dated events | Record |
#'
#' @param domain Domain name targeted by the rules.
#' @param key_vars Character vector of key variables.
#' @param subject_var Subject identifier variable. Default `"USUBJID"`.
#' @param site_var Site identifier variable. Default `"SITEID"`.
#' @param start_end_pairs List of two-element character vectors naming a
#'   start/end date pair, e.g. `list(c("AESTDTC", "AEENDTC"))`.
#' @param date_vars Character vector of variables expected to hold full
#'   ISO 8601 dates (`YYYY-MM-DD`).
#' @param time_date_pairs List of two-element character vectors `c(time_var,
#'   date_var)` where the time variable should not be present when the
#'   corresponding date variable is missing.
#' @param visit_order_var Optional numeric variable used to order visits
#'   within a subject (e.g. `"VISITNUM"` or `"ADY"`).
#' @param study_start Optional `Date` or ISO string. Events before this date
#'   are flagged by R012.
#' @param study_end Optional `Date` or ISO string. Events after this date are
#'   flagged by R012.
#' @param cutoff_date Date used as "today" for the future-dating check
#'   (R018). Defaults to `Sys.Date()`.
#' @param owner_team Team assigned to follow up on flagged records.
#' @param ... Reserved for future extensions; currently ignored.
#'
#' @return A list of `FlagRule` objects compatible with `run_flags()`.
#' @export
#'
#' @examples
#' pack <- make_rule_pack_dates(
#'   domain          = "LB",
#'   key_vars        = c("STUDYID", "USUBJID", "LBSEQ"),
#'   date_vars       = "LBDTC",
#'   visit_order_var = "VISITNUM"
#' )
#' length(pack)
make_rule_pack_dates <- function(domain,
                                 key_vars,
                                 subject_var     = "USUBJID",
                                 site_var        = "SITEID",
                                 start_end_pairs = list(),
                                 date_vars       = character(),
                                 time_date_pairs = list(),
                                 visit_order_var = NULL,
                                 study_start     = NULL,
                                 study_end       = NULL,
                                 cutoff_date     = Sys.Date(),
                                 owner_team      = "DM",
                                 ...) {
  rules <- list(
    R011 = rule_r011_start_after_end(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, start_end_pairs = start_end_pairs, date_vars = date_vars, time_date_pairs = time_date_pairs, visit_order_var = visit_order_var, study_start = study_start, study_end = study_end, cutoff_date = cutoff_date, owner_team = owner_team, ...),
    R012 = rule_r012_date_outside_study(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, start_end_pairs = start_end_pairs, date_vars = date_vars, time_date_pairs = time_date_pairs, visit_order_var = visit_order_var, study_start = study_start, study_end = study_end, cutoff_date = cutoff_date, owner_team = owner_team, ...),
    R013 = rule_r013_date_format(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, start_end_pairs = start_end_pairs, date_vars = date_vars, time_date_pairs = time_date_pairs, visit_order_var = visit_order_var, study_start = study_start, study_end = study_end, cutoff_date = cutoff_date, owner_team = owner_team, ...),
    R014 = rule_r014_end_without_start(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, start_end_pairs = start_end_pairs, date_vars = date_vars, time_date_pairs = time_date_pairs, visit_order_var = visit_order_var, study_start = study_start, study_end = study_end, cutoff_date = cutoff_date, owner_team = owner_team, ...),
    R015 = rule_r015_time_without_date(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, start_end_pairs = start_end_pairs, date_vars = date_vars, time_date_pairs = time_date_pairs, visit_order_var = visit_order_var, study_start = study_start, study_end = study_end, cutoff_date = cutoff_date, owner_team = owner_team, ...),
    R016 = rule_r016_visit_order(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, start_end_pairs = start_end_pairs, date_vars = date_vars, time_date_pairs = time_date_pairs, visit_order_var = visit_order_var, study_start = study_start, study_end = study_end, cutoff_date = cutoff_date, owner_team = owner_team, ...),
    R017 = rule_r017_identical_timestamps(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, start_end_pairs = start_end_pairs, date_vars = date_vars, time_date_pairs = time_date_pairs, visit_order_var = visit_order_var, study_start = study_start, study_end = study_end, cutoff_date = cutoff_date, owner_team = owner_team, ...),
    R018 = rule_r018_future_dated(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, start_end_pairs = start_end_pairs, date_vars = date_vars, time_date_pairs = time_date_pairs, visit_order_var = visit_order_var, study_start = study_start, study_end = study_end, cutoff_date = cutoff_date, owner_team = owner_team, ...),
    R019 = rule_r019_scheduled_gap(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, start_end_pairs = start_end_pairs, date_vars = date_vars, time_date_pairs = time_date_pairs, visit_order_var = visit_order_var, study_start = study_start, study_end = study_end, cutoff_date = cutoff_date, owner_team = owner_team, ...),
    R020 = rule_r020_timezone_inconsistency(domain = domain, key_vars = key_vars, subject_var = subject_var, site_var = site_var, start_end_pairs = start_end_pairs, date_vars = date_vars, time_date_pairs = time_date_pairs, visit_order_var = visit_order_var, study_start = study_start, study_end = study_end, cutoff_date = cutoff_date, owner_team = owner_team, ...)
  )

  Filter(Negate(is.null), rules)
}
