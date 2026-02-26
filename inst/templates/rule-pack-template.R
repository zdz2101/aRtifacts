# ---------------------------------------------------------------------------
# Rule pack template for aRtifacts core (topology-first, domain-neutral)
# ---------------------------------------------------------------------------
# Copy this file into R/ (for example: R/rule_pack_<domain>.R), then:
# 1) Rename the constructor function.
# 2) Update default args and required columns.
# 3) Resolve topology with choose_topology() when needed.
# 4) Replace template rule logic.
# 5) Add tests in tests/testthat/ using template-rule-pack-test.R.

#' Template topology rule pack constructor
#'
#' Replace this roxygen block with your pack-specific documentation.
#'
#' @param domain Target domain name inside `ArtifactSnapshot`.
#' @param owner_team Team responsible for follow-up.
#' @param threshold Example numeric threshold used by a rule.
#'
#' @return A list of `CoreRule` objects compatible with `run_rules()`.
#' @export
make_topology_pack_template <- function(domain,
                                        owner_team = "Data Quality",
                                        threshold = 100) {

  list(

    # -----------------------------------------------------------------------
    # Rule example 1: simple record-level threshold check
    # -----------------------------------------------------------------------
    new_rule(
      id = "TMP001_MEASURE_OVER_THRESHOLD",
      title = "Measure over threshold",
      topology = "A1",
      scope = "record",
      intent = "quality_screening",
      domain = domain,
      severity = "warning",
      owner_team = owner_team,
      fn = function(data, context, rule) {
        required <- c("entity_id", "measure_value")
        if (!all(required %in% names(data))) return(data.frame())

        value <- suppressWarnings(as.numeric(data$measure_value))
        bad <- !is.na(value) & value > threshold
        if (!any(bad)) return(data.frame())

        data[bad, c("entity_id", "measure_value"), drop = FALSE]
      }
    ),

    # -----------------------------------------------------------------------
    # Rule example 2: cross-field consistency check
    # -----------------------------------------------------------------------
    new_rule(
      id = "TMP002_FLAG_WITHOUT_EVENT_DATE",
      title = "Flag without event date",
      topology = "A1",
      scope = "record",
      intent = "data_entry",
      domain = domain,
      severity = "error",
      owner_team = owner_team,
      fn = function(data, context, rule) {
        required <- c("entity_id", "event_flag", "event_date")
        if (!all(required %in% names(data))) return(data.frame())

        flag_on <- !is.na(data$event_flag) & toupper(trimws(as.character(data$event_flag))) == "Y"
        date_missing <- is.na(data$event_date) | !nzchar(trimws(as.character(data$event_date)))
        bad <- flag_on & date_missing
        if (!any(bad)) return(data.frame())

        data[bad, c("entity_id", "event_flag", "event_date"), drop = FALSE]
      }
    )
  )
}
