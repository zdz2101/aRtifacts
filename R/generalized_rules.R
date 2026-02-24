#' Built-in core observability rule catalog
#'
#' Returns the initial high-ROI rule catalog used to structure generalized
#' dataset observability checks. The catalog is domain- and schema-agnostic;
#' rules describe structural patterns (keys, dates, missingness, outliers)
#' applicable to any keyed tabular data.
#'
#' @return A `data.frame` with columns `rule_id`, `rule_name`, `scope`, and
#'   `category`.
#' @export
#'
#' @examples
#' rules <- rule_catalog_core()
#' head(rules)
rule_catalog_core <- function() {
  rule_catalog_core_data
}

#' Generalized tabulation checks
#'
#' Runs generalized structural and temporal checks that map to a subset of the
#' core observability rule catalog.
#'
#' @param data A domain-level `data.frame`.
#' @param domain Optional domain name used in issue messages.
#' @param key_vars Character vector of key variables.
#' @param required_vars Character vector of required variables.
#' @param start_end_pairs Named list of start/end pairs, for example
#'   `list(c("AESTDT", "AEENDT"))`.
#' @param non_negative_vars Variables expected to be non-negative.
#' @param date_vars Variables expected to contain full dates in `YYYY-MM-DD`
#'   format.
#' @param visit_order_var Optional variable used for within-subject ordering.
#' @param subject_var Subject identifier variable.
#'
#' @return A `data.frame` with columns `check`, `severity`, `message`, and `n`.
#' @export
#'
#' @examples
#' check_tabulation_domain(
#'   data = example_sdtm_lb(),
#'   domain = "LB",
#'   key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
#'   required_vars = c("STUDYID", "USUBJID", "LBSEQ", "LBTESTCD")
#' )
check_tabulation_domain <- function(data,
                                    domain = NULL,
                                    key_vars,
                                    required_vars = character(),
                                    start_end_pairs = list(),
                                    non_negative_vars = character(),
                                    date_vars = character(),
                                    visit_order_var = NULL,
                                    subject_var = "USUBJID") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }

  issues <- list()
  i <- 0L
  add_issue <- function(check, severity, message, n = NA_integer_) {
    i <<- i + 1L
    issues[[i]] <<- .make_issue(check, severity, message, n)
  }

  missing_required <- setdiff(required_vars, names(data))
  if (length(missing_required)) {
    add_issue(
      "R024_REQUIRED_MISSING",
      "error",
      paste0("Missing required variables", if (!is.null(domain)) paste0(" in ", domain), ": ", paste(missing_required, collapse = ", "), "."),
      length(missing_required)
    )
  }

  missing_keys <- setdiff(key_vars, names(data))
  if (length(missing_keys)) {
    add_issue(
      "R001_KEY_MISSING",
      "error",
      paste0("Missing key variables: ", paste(missing_keys, collapse = ", "), "."),
      length(missing_keys)
    )
  } else {
    key_missing_rows <- rowSums(is.na(data[, key_vars, drop = FALSE]) | data[, key_vars, drop = FALSE] == "") > 0
    if (any(key_missing_rows)) {
      add_issue(
        "R001_KEY_MISSING",
        "error",
        "One or more records have missing key field values.",
        sum(key_missing_rows)
      )
    }

    dupes <- sum(duplicated(data[, key_vars, drop = FALSE]))
    if (dupes > 0) {
      add_issue(
        "R002_DUPLICATE_KEYS",
        "error",
        "Duplicate key combinations were detected.",
        dupes
      )
    }
  }

  for (pair in start_end_pairs) {
    if (length(pair) != 2) next
    start_var <- pair[[1]]
    end_var <- pair[[2]]
    if (!(start_var %in% names(data) && end_var %in% names(data))) next

    start_date <- as.Date(data[[start_var]])
    end_date <- as.Date(data[[end_var]])

    bad_order <- !is.na(start_date) & !is.na(end_date) & start_date > end_date
    if (any(bad_order)) {
      add_issue(
        "R011_START_AFTER_END",
        "error",
        paste0(start_var, " occurs after ", end_var, " on one or more records."),
        sum(bad_order)
      )
    }

    end_without_start <- is.na(start_date) & !is.na(end_date)
    if (any(end_without_start)) {
      add_issue(
        "R014_END_WITHOUT_START",
        "warning",
        paste0(end_var, " is present while ", start_var, " is missing on one or more records."),
        sum(end_without_start)
      )
    }
  }

  for (var in date_vars) {
    if (!(var %in% names(data))) next
    vals <- as.character(data[[var]])
    present <- !is.na(vals) & nzchar(vals)
    bad <- present & !grepl("^\\d{4}-\\d{2}-\\d{2}$", vals)
    if (any(bad)) {
      add_issue(
        "R013_DATE_FORMAT",
        "warning",
        paste0(var, " contains values that are not full ISO dates (YYYY-MM-DD)."),
        sum(bad)
      )
    }
  }

  for (var in non_negative_vars) {
    if (!(var %in% names(data))) next
    vals <- suppressWarnings(as.numeric(data[[var]]))
    bad <- !is.na(vals) & vals < 0
    if (any(bad)) {
      add_issue(
        "R033_NEGATIVE_NOT_ALLOWED",
        "warning",
        paste0(var, " contains negative values where non-negative values are expected."),
        sum(bad)
      )
    }
  }

  if (!is.null(visit_order_var) && subject_var %in% names(data) && visit_order_var %in% names(data)) {
    ord <- order(data[[subject_var]], data[[visit_order_var]], na.last = TRUE)
    sorted <- data[ord, , drop = FALSE]
    split_idx <- split(seq_len(nrow(sorted)), sorted[[subject_var]])

    out_of_order <- 0L
    for (idx in split_idx) {
      visits <- sorted[[visit_order_var]][idx]
      if (is.numeric(visits)) {
        out_of_order <- out_of_order + sum(diff(visits) < 0, na.rm = TRUE)
      }
    }

    if (out_of_order > 0L) {
      add_issue(
        "R016_VISIT_ORDER",
        "warning",
        "Visit/timepoint order appears inconsistent within one or more subjects.",
        out_of_order
      )
    }
  }

  .bind_issues(issues)
}

#' Build a practical starter rule pack from the core catalog
#'
#' Thin wrapper that assembles all five core module rule packs into a
#' single list. Arguments are forwarded verbatim to each module builder;
#' unknown arguments are silently ignored by each module via `...`.
#'
#' See the individual module builder functions for full argument documentation:
#' * [make_rule_pack_keys()]
#' * [make_rule_pack_dates()]
#' * [make_rule_pack_missingness()]
#' * [make_rule_pack_plausibility()]
#' * [make_rule_pack_consistency()]
#'
#' @param domain Domain name targeted by the rules.
#' @param key_vars Character vector of key variables.
#' @param ... Additional arguments forwarded to each module builder.
#'
#' @return A list of `FlagRule` objects compatible with `run_flags()`.
#' @export
#'
#' @examples
#' rules <- make_rule_pack_core(
#'   domain = "LB",
#'   key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
#'   required_vars = c("STUDYID", "USUBJID", "LBSEQ", "LBTESTCD")
#' )
#' length(rules)
make_rule_pack_core <- function(domain, key_vars, ...) {
  c(
    make_rule_pack_keys(domain, key_vars, ...),
    make_rule_pack_dates(domain, key_vars, ...),
    make_rule_pack_missingness(domain, key_vars, ...),
    make_rule_pack_plausibility(domain, key_vars, ...),
    make_rule_pack_consistency(domain, key_vars, ...)
  )
}
