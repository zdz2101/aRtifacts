#' Example ADSL data
#'
#' Creates a small synthetic ADSL-like dataset for demos and tests.
#' Part of the bundled ADaM supplemental module.
#'
#' @return A `data.frame` with one row per subject.
#' @export
#'
#' @examples
#' adsl <- example_adsl()
#' head(adsl)
example_adsl <- function() {
  data.frame(
    STUDYID = "STUDY01",
    USUBJID = c("STUDY01-001", "STUDY01-002", "STUDY01-003"),
    SUBJID = c("001", "002", "003"),
    SITEID = c("SITE01", "SITE01", "SITE02"),
    AGE = c(63, 58, 71),
    AGEU = "YEARS",
    SEX = c("F", "M", "F"),
    RACE = c("WHITE", "WHITE", "ASIAN"),
    ARM = c("Drug A", "Drug A", "Placebo"),
    TRT01P = c("Drug A", "Drug A", "Placebo"),
    SAFFL = c("Y", "Y", "Y"),
    FASFL = c("Y", "Y", "N"),
    stringsAsFactors = FALSE
  )
}

#' Example ADVS-like BDS data
#'
#' Creates a small synthetic BDS-like dataset (vital signs) for demos and tests.
#' Part of the bundled ADaM supplemental module.
#'
#' @return A `data.frame` with one or more rows per subject/parameter.
#' @export
#'
#' @examples
#' advs <- example_advs()
#' head(advs)
example_advs <- function() {
  data.frame(
    STUDYID = "STUDY01",
    USUBJID = c("STUDY01-001", "STUDY01-001", "STUDY01-002", "STUDY01-002", "STUDY01-003"),
    PARAM = "Systolic Blood Pressure (mmHg)",
    PARAMCD = "SYSBP",
    AVAL = c(130, 122, 140, 136, 150),
    AVISIT = c("BASELINE", "WEEK 4", "BASELINE", "WEEK 4", "BASELINE"),
    ADY = c(1, 29, 1, 29, 1),
    TRTP = c("Drug A", "Drug A", "Drug A", "Drug A", "Placebo"),
    ABLFL = c("Y", NA, "Y", NA, "Y"),
    BASE = c(130, 130, 140, 140, 150),
    CHG = c(0, -8, 0, -4, 0),
    stringsAsFactors = FALSE
  )
}

#' Example SDTM LB-like data
#'
#' Creates a small synthetic SDTM LB-like dataset for demos.
#' Part of the bundled SDTM supplemental module.
#'
#' @return A `data.frame` with one or more laboratory records per subject.
#' @export
#'
#' @examples
#' lb <- example_sdtm_lb()
#' head(lb)
example_sdtm_lb <- function() {
  data.frame(
    STUDYID = "STUDY01",
    DOMAIN = "LB",
    USUBJID = c("STUDY01-001", "STUDY01-001", "STUDY01-002", "STUDY01-002", "STUDY01-003"),
    LBSEQ = c(1, 2, 1, 2, 1),
    LBTESTCD = c("HGB", "HGB", "HGB", "HGB", "HGB"),
    LBTEST = "Hemoglobin",
    LBORRES = c("13.0", "13.4", "-1.0", "14.1", "15.0"),
    LBORRESU = "g/dL",
    LBSTRESN = c(13.0, 13.4, -1.0, 14.1, 15.0),
    LBDTC = c("2024-01-01", "2024-01-29", "2024-01-03", "2024-01-31", "2024-01-04"),
    VISITNUM = c(1, 2, 1, 2, 1),
    SITEID = c("SITE01", "SITE01", "SITE01", "SITE01", "SITE02"),
    stringsAsFactors = FALSE
  )
}
