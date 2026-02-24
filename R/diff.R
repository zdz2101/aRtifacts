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
