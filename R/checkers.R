#' Check ADSL conformance (basic)
#'
#' Performs lightweight ADSL-oriented checks aligned with key ADaMIG concepts:
#' required columns, uniqueness of `USUBJID`, and population flag quality.
#'
#' This is a **bundled supplemental** checker — part of the ADaM module that
#' will eventually live in a companion `aRtifacts.adam` package.
#'
#' @param adsl A subject-level analysis dataset as a `data.frame`.
#'
#' @return A `data.frame` of issues with columns `check`, `severity`,
#'   `message`, and `n`.
#' @export
#'
#' @examples
#' check_adsl(example_adsl())
check_adsl <- function(adsl) {
  if (!is.data.frame(adsl)) {
    stop("`adsl` must be a data.frame.", call. = FALSE)
  }

  issues <- list()
  i <- 0L
  add_issue <- function(check, severity, message, n = NA_integer_) {
    i <<- i + 1L
    issues[[i]] <<- .make_issue(check, severity, message, n)
  }

  required <- c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "AGEU", "SEX", "RACE", "ARM", "TRT01P")
  missing_required <- setdiff(required, names(adsl))
  if (length(missing_required)) {
    add_issue(
      "ADSL_REQUIRED",
      "error",
      paste0("Missing required ADSL variables: ", paste(missing_required, collapse = ", "), "."),
      length(missing_required)
    )
  }

  if ("USUBJID" %in% names(adsl)) {
    dupe <- sum(duplicated(adsl$USUBJID))
    if (dupe > 0L) {
      add_issue("ADSL_USUBJID_UNIQUE", "error", "USUBJID must be unique in ADSL.", dupe)
    }
  }

  pop_flags <- intersect(
    c("FASFL", "SAFFL", "ITTFL", "PPROTFL", "COMPLFL", "RANDFL", "ENRLFL"),
    names(adsl)
  )

  if (!length(pop_flags)) {
    add_issue("ADSL_POPFLAG", "warning", "At least one subject-level population flag (*FL) is expected.")
  } else {
    for (flag in pop_flags) {
      vals <- adsl[[flag]]
      bad <- is.na(vals) | !(vals %in% c("Y", "N"))
      if (any(bad)) {
        add_issue(
          "ADSL_POPFLAG_VALUES",
          "error",
          paste0(flag, " must be populated with Y/N values only."),
          sum(bad)
        )
      }
    }
  }

  .bind_issues(issues)
}

#' Check BDS conformance (basic)
#'
#' Performs lightweight BDS-oriented checks aligned with key ADaMIG concepts,
#' including required columns, treatment presence, `PARAM`/`PARAMCD` mapping,
#' and selected baseline/timing rules.
#'
#' This is a **bundled supplemental** checker — part of the ADaM module that
#' will eventually live in a companion `aRtifacts.adam` package.
#'
#' @param bds A basic data structure dataset as a `data.frame`.
#'
#' @return A `data.frame` of issues with columns `check`, `severity`,
#'   `message`, and `n`.
#' @export
#'
#' @examples
#' check_bds(example_advs())
check_bds <- function(bds) {
  if (!is.data.frame(bds)) {
    stop("`bds` must be a data.frame.", call. = FALSE)
  }

  issues <- list()
  i <- 0L
  add_issue <- function(check, severity, message, n = NA_integer_) {
    i <<- i + 1L
    issues[[i]] <<- .make_issue(check, severity, message, n)
  }

  required <- c("STUDYID", "USUBJID", "PARAM", "PARAMCD")
  missing_required <- setdiff(required, names(bds))
  if (length(missing_required)) {
    add_issue(
      "BDS_REQUIRED",
      "error",
      paste0("Missing required BDS variables: ", paste(missing_required, collapse = ", "), "."),
      length(missing_required)
    )
  }

  if (!("AVAL" %in% names(bds) || "AVALC" %in% names(bds))) {
    add_issue(
      "BDS_AVAL_OR_AVALC",
      "error",
      "BDS must include at least one analysis value variable: AVAL or AVALC."
    )
  }

  treat_vars <- grep("^TRT\\d{2}P$|^TRTP$|^TRTA$", names(bds), value = TRUE, perl = TRUE)
  if (!length(treat_vars)) {
    add_issue(
      "BDS_TREATMENT",
      "error",
      "At least one treatment variable is expected (e.g., TRT01P, TRTP, TRTA)."
    )
  }

  if (all(c("PARAM", "PARAMCD") %in% names(bds))) {
    if (!.is_one_to_one(bds, "PARAM", "PARAMCD")) {
      add_issue("BDS_PARAM_MAP", "error", "PARAM and PARAMCD must have a one-to-one mapping within dataset.")
    }

    bad_paramcd <- !grepl("^[A-Za-z][A-Za-z0-9_]{0,7}$", bds$PARAMCD)
    if (any(bad_paramcd, na.rm = TRUE)) {
      add_issue(
        "BDS_PARAMCD_FORMAT",
        "error",
        "PARAMCD values should be <=8 chars, start with a letter, and contain only A-Z, 0-9, _.",
        sum(bad_paramcd, na.rm = TRUE)
      )
    }
  }

  if (all(c("USUBJID", "PARAM") %in% names(bds))) {
    grp <- stats::aggregate(
      rep(1L, nrow(bds)),
      by = list(USUBJID = bds$USUBJID, PARAM = bds$PARAM),
      FUN = sum
    )
    has_multi <- any(grp$x > 1L)
    if (has_multi) {
      has_rel_timing <- any(c("ADY", "ASTDY", "AENDY", "AVISIT", "ATPT") %in% names(bds))
      if (!has_rel_timing) {
        add_issue(
          "BDS_REL_TIMING",
          "warning",
          "When multiple records exist per subject/parameter, include a relative timing variable (e.g., ADY or AVISIT)."
        )
      }
    }
  }

  if ("BASE" %in% names(bds) && any(!is.na(bds$BASE))) {
    if (!("ABLFL" %in% names(bds))) {
      add_issue("BDS_BASE_ABLFL", "error", "ABLFL is required when BASE is populated.")
    } else if (all(c("USUBJID", "PARAM", "ABLFL") %in% names(bds))) {
      subset_base <- bds[!is.na(bds$BASE), c("USUBJID", "PARAM"), drop = FALSE]
      key_base <- unique(subset_base)

      flagged <- bds[bds$ABLFL == "Y", c("USUBJID", "PARAM"), drop = FALSE]
      key_flag <- unique(flagged)

      missing_baseline <- nrow(merge(key_base, key_flag, by = c("USUBJID", "PARAM"), all.x = TRUE)) -
        nrow(merge(key_base, key_flag, by = c("USUBJID", "PARAM"), all = FALSE))

      if (missing_baseline > 0L) {
        add_issue(
          "BDS_BASELINE_RECORD",
          "error",
          "Each subject/parameter with non-null BASE should have at least one ABLFL='Y' record.",
          missing_baseline
        )
      }
    }
  }

  .bind_issues(issues)
}

#' Validate ADaM inputs across ADSL and BDS datasets
#'
#' Convenience wrapper that runs `check_adsl()` and `check_bds()` in one call.
#'
#' Bundled supplemental — future `aRtifacts.adam` companion package.
#'
#' @param adsl Optional ADSL `data.frame`.
#' @param bds Either a single BDS `data.frame` or a named list of BDS
#'   `data.frame` objects.
#'
#' @return A list with elements `adsl` and `bds` containing issue tables.
#' @export
#'
#' @examples
#' validate_adam(adsl = example_adsl(), bds = list(ADVS = example_advs()))
validate_adam <- function(adsl = NULL, bds = list()) {
  out <- list()

  if (!is.null(adsl)) {
    out$adsl <- check_adsl(adsl)
  }

  if (is.data.frame(bds)) {
    bds <- list(BDS = bds)
  }

  if (is.list(bds) && length(bds)) {
    out$bds <- lapply(bds, check_bds)
  } else {
    out$bds <- list()
  }

  out
}
