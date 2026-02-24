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
#' @return A list of `FlagRule` objects compatible with `run_flags()`.
#' @export
make_rule_pack_template <- function(domain,
                                    owner_team = "Data Management",
                                    threshold = 100) {

  list(

    # -----------------------------------------------------------------------
    # Rule example 1: simple record-level threshold check
    # -----------------------------------------------------------------------
    new_flag_rule(
      id = "TMP001_VALUE_OVER_THRESHOLD",
      domain = domain,
      severity = "warning",
      category = "clinical_review",
      owner_team = owner_team,
      fn = function(df) {
        required <- c("USUBJID", "VALUE")
        if (!all(required %in% names(df))) return(data.frame())

        value <- suppressWarnings(as.numeric(df$VALUE))
        bad <- !is.na(value) & value > threshold
        if (!any(bad)) return(data.frame())

        df[bad, c("USUBJID", "VALUE"), drop = FALSE]
      }
    ),

    # -----------------------------------------------------------------------
    # Rule example 2: cross-field consistency check
    # -----------------------------------------------------------------------
    new_flag_rule(
      id = "TMP002_FLAG_WITHOUT_SUPPORTING_DATE",
      domain = domain,
      severity = "error",
      category = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        required <- c("USUBJID", "FLAG", "DATE")
        if (!all(required %in% names(df))) return(data.frame())

        flag_on <- !is.na(df$FLAG) & as.character(df$FLAG) == "Y"
        date_missing <- is.na(df$DATE) | !nzchar(as.character(df$DATE))
        bad <- flag_on & date_missing
        if (!any(bad)) return(data.frame())

        df[bad, c("USUBJID", "FLAG", "DATE"), drop = FALSE]
      }
    )
  )
}
