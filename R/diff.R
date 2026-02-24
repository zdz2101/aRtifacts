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
