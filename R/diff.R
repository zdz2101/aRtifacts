#' Diff key rows between two domain snapshots
#'
#' Compares key combinations between an old and new `DomainSnapshot`.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#'
#' @return A list with two `data.frame` elements: `added` and `removed`.
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = example_advs(),
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' new_data <- example_advs()
#' new_data <- rbind(
#'   new_data,
#'   data.frame(
#'     STUDYID = "STUDY01",
#'     USUBJID = "STUDY01-999",
#'     PARAM = "Systolic Blood Pressure (mmHg)",
#'     PARAMCD = "SYSBP",
#'     AVAL = 121,
#'     AVISIT = "BASELINE",
#'     ADY = 1,
#'     TRTP = "Drug A",
#'     ABLFL = "Y",
#'     BASE = 121,
#'     CHG = 0,
#'     stringsAsFactors = FALSE
#'   )
#' )
#' new_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = new_data,
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' diff_keys(old_dom, new_dom)
diff_keys <- function(old_dom, new_dom) {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  if (!identical(old_dom@key_vars, new_dom@key_vars)) {
    stop("`old_dom` and `new_dom` must share identical key_vars.", call. = FALSE)
  }

  keys <- old_dom@key_vars
  old_keys <- unique(old_dom@data[, keys, drop = FALSE])
  new_keys <- unique(new_dom@data[, keys, drop = FALSE])

  old_id <- .key_id(old_keys, keys)
  new_id <- .key_id(new_keys, keys)

  added <- new_keys[!(new_id %in% old_id), , drop = FALSE]
  removed <- old_keys[!(old_id %in% new_id), , drop = FALSE]

  list(added = added, removed = removed)
}

.safe_rate <- function(numerator, denominator) {
  if (denominator == 0L) {
    if (numerator == 0L) 0 else NA_real_
  } else {
    as.numeric(numerator) / as.numeric(denominator)
  }
}

#' Compare key churn between two domain snapshots
#'
#' Summarizes key-level churn across cuts, including added and removed key
#' counts and rates.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#'
#' @return A one-row `data.frame` with deterministic columns:
#'   `domain`, `old_n_keys`, `new_n_keys`, `added_n`, `removed_n`,
#'   `churn_n`, `added_rate`, `removed_rate`, `churn_rate`,
#'   `net_change_n`, and `net_change_rate`.
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = example_advs(),
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' new_data <- example_advs()
#' new_data <- rbind(
#'   new_data,
#'   data.frame(
#'     STUDYID = "STUDY01",
#'     USUBJID = "STUDY01-999",
#'     PARAM = "Systolic Blood Pressure (mmHg)",
#'     PARAMCD = "SYSBP",
#'     AVAL = 121,
#'     AVISIT = "BASELINE",
#'     ADY = 1,
#'     TRTP = "Drug A",
#'     ABLFL = "Y",
#'     BASE = 121,
#'     CHG = 0,
#'     stringsAsFactors = FALSE
#'   )
#' )
#' new_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = new_data,
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' churn <- compare_key_churn(old_dom, new_dom)
#' churn$churn_rate > 0.10
compare_key_churn <- function(old_dom, new_dom) {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  if (!identical(old_dom@key_vars, new_dom@key_vars)) {
    stop("`old_dom` and `new_dom` must share identical key_vars.", call. = FALSE)
  }

  key_diff <- diff_keys(old_dom, new_dom)
  old_n_keys <- nrow(unique(old_dom@data[, old_dom@key_vars, drop = FALSE]))
  new_n_keys <- nrow(unique(new_dom@data[, new_dom@key_vars, drop = FALSE]))
  added_n <- nrow(key_diff$added)
  removed_n <- nrow(key_diff$removed)
  churn_n <- added_n + removed_n
  net_change_n <- new_n_keys - old_n_keys

  data.frame(
    domain = as.character(old_dom@name),
    old_n_keys = as.integer(old_n_keys),
    new_n_keys = as.integer(new_n_keys),
    added_n = as.integer(added_n),
    removed_n = as.integer(removed_n),
    churn_n = as.integer(churn_n),
    added_rate = .safe_rate(added_n, old_n_keys),
    removed_rate = .safe_rate(removed_n, old_n_keys),
    churn_rate = .safe_rate(churn_n, old_n_keys),
    net_change_n = as.integer(net_change_n),
    net_change_rate = .safe_rate(net_change_n, old_n_keys),
    stringsAsFactors = FALSE
  )
}

#' Compare record volume between two domain snapshots
#'
#' Summarizes record-count changes across cuts, including absolute and
#' relative change.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#'
#' @return A one-row `data.frame` with deterministic columns:
#'   `domain`, `old_n_records`, `new_n_records`, `absolute_change`,
#'   `relative_change`, and `direction` (`"increase"`, `"decrease"`,
#'   `"unchanged"`).
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = example_advs(),
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' new_data <- example_advs()[1:4, , drop = FALSE]
#' new_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = new_data,
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' vol <- compare_record_volume(old_dom, new_dom)
#' vol$relative_change < -0.20
compare_record_volume <- function(old_dom, new_dom) {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  old_n_records <- nrow(old_dom@data)
  new_n_records <- nrow(new_dom@data)
  absolute_change <- new_n_records - old_n_records

  direction <- if (absolute_change > 0L) {
    "increase"
  } else if (absolute_change < 0L) {
    "decrease"
  } else {
    "unchanged"
  }

  data.frame(
    domain = as.character(old_dom@name),
    old_n_records = as.integer(old_n_records),
    new_n_records = as.integer(new_n_records),
    absolute_change = as.integer(absolute_change),
    relative_change = .safe_rate(absolute_change, old_n_records),
    direction = direction,
    stringsAsFactors = FALSE
  )
}

.column_type_string <- function(x) {
  paste(class(x), collapse = "|")
}

#' Diff key variable types between two domain snapshots
#'
#' Compares classes of key variables between an old and new `DomainSnapshot`.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#'
#' @return A `data.frame` with `key_var`, `old_type`, `new_type`, and
#'   `changed`.
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = example_advs(),
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' new_data <- example_advs()
#' new_data$USUBJID <- as.factor(new_data$USUBJID)
#' new_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = new_data,
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' diff_key_types(old_dom, new_dom)
diff_key_types <- function(old_dom, new_dom) {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  key_vars <- sort(unique(c(old_dom@key_vars, new_dom@key_vars)))
  if (!length(key_vars)) {
    return(data.frame(
      key_var = character(0),
      old_type = character(0),
      new_type = character(0),
      changed = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  old_type <- vapply(
    key_vars,
    function(v) {
      if (v %in% names(old_dom@data)) .column_type_string(old_dom@data[[v]]) else NA_character_
    },
    character(1)
  )
  new_type <- vapply(
    key_vars,
    function(v) {
      if (v %in% names(new_dom@data)) .column_type_string(new_dom@data[[v]]) else NA_character_
    },
    character(1)
  )

  data.frame(
    key_var = key_vars,
    old_type = unname(old_type),
    new_type = unname(new_type),
    changed = !(is.na(old_type) & is.na(new_type)) &
      (is.na(old_type) | is.na(new_type) | old_type != new_type),
    stringsAsFactors = FALSE
  )
}

#' Diff column presence between two domain snapshots
#'
#' Compares old vs new dataset column presence and reports columns that are
#' added, removed, or unchanged.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#'
#' @return A `data.frame` with `column`, `old_present`, `new_present`,
#'   `status`, and `changed`.
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = example_advs(),
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' new_data <- example_advs()
#' new_data$NEWCOL <- "X"
#' new_data$CHG <- NULL
#' new_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = new_data,
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' diff_columns(old_dom, new_dom)
diff_columns <- function(old_dom, new_dom) {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  columns <- sort(unique(c(names(old_dom@data), names(new_dom@data))))
  if (!length(columns)) {
    return(data.frame(
      column = character(0),
      old_present = logical(0),
      new_present = logical(0),
      status = character(0),
      changed = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  old_present <- columns %in% names(old_dom@data)
  new_present <- columns %in% names(new_dom@data)
  status <- ifelse(
    old_present & new_present,
    "unchanged",
    ifelse(!old_present & new_present, "added", "removed")
  )

  data.frame(
    column = columns,
    old_present = old_present,
    new_present = new_present,
    status = status,
    changed = status != "unchanged",
    stringsAsFactors = FALSE
  )
}

#' Diff shared column types between two domain snapshots
#'
#' Compares classes for columns that exist in both snapshots.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#'
#' @return A `data.frame` with `column`, `old_type`, `new_type`, and
#'   `changed`.
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = example_advs(),
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' new_data <- example_advs()
#' new_data$AVAL <- as.character(new_data$AVAL)
#' new_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = new_data,
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' diff_column_types(old_dom, new_dom)
diff_column_types <- function(old_dom, new_dom) {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  columns <- sort(intersect(names(old_dom@data), names(new_dom@data)))
  if (!length(columns)) {
    return(data.frame(
      column = character(0),
      old_type = character(0),
      new_type = character(0),
      changed = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  old_type <- vapply(columns, function(v) .column_type_string(old_dom@data[[v]]), character(1))
  new_type <- vapply(columns, function(v) .column_type_string(new_dom@data[[v]]), character(1))

  data.frame(
    column = columns,
    old_type = unname(old_type),
    new_type = unname(new_type),
    changed = old_type != new_type,
    stringsAsFactors = FALSE
  )
}

#' Diff values between two domain snapshots
#'
#' Compares one or more variables on matched keys between two
#' `DomainSnapshot` objects.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#' @param vars Character vector of variables to compare.
#'
#' @return A `data.frame` with key columns, `var`, `old`, and `new` for
#'   changed values.
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = example_advs(),
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' new_data <- example_advs()
#' new_data$AVAL[new_data$USUBJID == "STUDY01-002" & new_data$AVISIT == "WEEK 4"] <- 141
#' new_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = new_data,
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' diff_values(old_dom, new_dom, vars = c("AVAL", "CHG"))
diff_values <- function(old_dom, new_dom, vars = "AVAL") {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  if (!identical(old_dom@key_vars, new_dom@key_vars)) {
    stop("`old_dom` and `new_dom` must share identical key_vars.", call. = FALSE)
  }

  keys <- old_dom@key_vars
  shared_vars <- intersect(vars, intersect(names(old_dom@data), names(new_dom@data)))
  if (!length(shared_vars)) {
    return(data.frame())
  }

  merged <- merge(
    old_dom@data[, c(keys, shared_vars), drop = FALSE],
    new_dom@data[, c(keys, shared_vars), drop = FALSE],
    by = keys,
    suffixes = c(".old", ".new"),
    all = FALSE
  )

  out <- vector("list", length(shared_vars))
  idx <- 0L

  for (var in shared_vars) {
    old_col <- paste0(var, ".old")
    new_col <- paste0(var, ".new")
    changed <- !(is.na(merged[[old_col]]) & is.na(merged[[new_col]])) &
      (is.na(merged[[old_col]]) | is.na(merged[[new_col]]) | merged[[old_col]] != merged[[new_col]])

    if (any(changed)) {
      idx <- idx + 1L
      piece <- merged[changed, keys, drop = FALSE]
      piece$var <- var
      piece$old <- merged[[old_col]][changed]
      piece$new <- merged[[new_col]][changed]
      out[[idx]] <- piece
    }
  }

  if (!idx) {
    return(data.frame())
  }

  do.call(rbind, out[seq_len(idx)])
}

#' Diff artifact metadata
#'
#' Compares top-level metadata fields between two `ArtifactSnapshot` objects.
#'
#' @param old_art Previous `ArtifactSnapshot`.
#' @param new_art Current `ArtifactSnapshot`.
#'
#' @return A `data.frame` with `field`, `old`, `new`, and `changed`.
#' @export
#'
#' @examples
#' dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = example_advs(),
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' old_art <- new_artifact_snapshot(domains = list(ADVS = dom), build_id = "run-001")
#' new_art <- new_artifact_snapshot(domains = list(ADVS = dom), build_id = "run-002")
#' diff_meta_artifact(old_art, new_art)
diff_meta_artifact <- function(old_art, new_art) {
  .assert_artifact(old_art, "old_art")
  .assert_artifact(new_art, "new_art")

  fields <- c("spec_version", "data_cut", "build_id", "code_sha", "inputs_hash")
  old_vals <- vapply(fields, function(x) S7::prop(old_art, x), character(1))
  new_vals <- vapply(fields, function(x) S7::prop(new_art, x), character(1))

  data.frame(
    field = fields,
    old = unname(old_vals),
    new = unname(new_vals),
    changed = !mapply(identical, old_vals, new_vals),
    stringsAsFactors = FALSE
  )
}
