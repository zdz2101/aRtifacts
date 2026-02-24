# ---------------------------------------------------------------------------
# Rule pack template for aRtifacts
# ---------------------------------------------------------------------------
# Copy this file into R/ (for example: R/rule_pack_<domain>.R), then:
# 1) Rename the constructor function.
# 2) Update default args and required columns.
# 3) Replace template rule logic.
# 4) Add tests in tests/testthat/ using template-rule-pack-test.R.

#' Template rule pack constructor
#'
#' Replace this roxygen block with your pack-specific documentation.
#'
#' @param domain Target domain name inside ArtifactSnapshot.
#' @param owner_team Team responsible for follow-up.
#' @param threshold Example numeric threshold used by a rule.
#'
#' @return A list of `CoreRule` objects compatible with `run_rules()`.
#' @export
make_rule_pack_template <- function(domain,
                                    owner_team = "Data Management",
                                    threshold = 100) {

  list(

    # -----------------------------------------------------------------------
    # Rule example 1: simple record-level threshold check
    # -----------------------------------------------------------------------
    new_rule(
      id = "TMP001_VALUE_OVER_THRESHOLD",
      title = "Value over threshold",
      topology = "A1",
      scope = "record",
      intent = "clinical_review",
      domain = domain,
      severity = "warning",
      owner_team = owner_team,
      fn = function(data, context, rule) {
        required <- c("USUBJID", "VALUE")
        if (!all(required %in% names(data))) return(data.frame())

        value <- suppressWarnings(as.numeric(data$VALUE))
        bad <- !is.na(value) & value > threshold
        if (!any(bad)) return(data.frame())

        data[bad, c("USUBJID", "VALUE"), drop = FALSE]
      }
    ),

    # -----------------------------------------------------------------------
    # Rule example 2: cross-field consistency check
    # -----------------------------------------------------------------------
    new_rule(
      id = "TMP002_FLAG_WITHOUT_SUPPORTING_DATE",
      title = "Flag without supporting date",
      topology = "A1",
      scope = "record",
      intent = "data_entry",
      domain = domain,
      severity = "error",
      owner_team = owner_team,
      fn = function(data, context, rule) {
        required <- c("USUBJID", "FLAG", "DATE")
        if (!all(required %in% names(data))) return(data.frame())

        flag_on <- !is.na(data$FLAG) & as.character(data$FLAG) == "Y"
        date_missing <- is.na(data$DATE) | !nzchar(as.character(data$DATE))
        bad <- flag_on & date_missing
        if (!any(bad)) return(data.frame())

        data[bad, c("USUBJID", "FLAG", "DATE"), drop = FALSE]
      }
    )
  )
}
