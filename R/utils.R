`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

.make_issue <- function(check, severity, message, n = NA_integer_) {
  data.frame(
    check = check,
    severity = severity,
    message = message,
    n = n,
    stringsAsFactors = FALSE
  )
}

.empty_issues <- function() {
  data.frame(
    check = character(),
    severity = character(),
    message = character(),
    n = integer(),
    stringsAsFactors = FALSE
  )
}

.bind_issues <- function(items) {
  items <- Filter(Negate(is.null), items)
  if (!length(items)) return(.empty_issues())
  do.call(rbind, items)
}

.key_id <- function(df, key_vars) {
  if (!length(key_vars) || !nrow(df)) return(character())
  apply(df[, key_vars, drop = FALSE], 1, function(x) paste(x, collapse = "\r"))
}

.has_unique_keys <- function(df, key_vars) {
  if (!length(key_vars) || !nrow(df)) return(TRUE)
  !anyDuplicated(df[, key_vars, drop = FALSE])
}

.is_one_to_one <- function(df, left, right, within = NULL) {
  if (!(left %in% names(df) && right %in% names(df))) return(TRUE)
  temp <- df[, c(within, left, right), drop = FALSE]
  temp <- temp[stats::complete.cases(temp[, c(left, right), drop = FALSE]), , drop = FALSE]
  if (!nrow(temp)) return(TRUE)

  key_lr <- c(within, left)
  key_rl <- c(within, right)

  # Deduplicate before checking key cardinality so that repeated rows
  # (e.g. the same PARAM/PARAMCD pair appearing many times in a BDS dataset)
  # do not produce false positives.
  uniq_lr <- unique(temp[, c(key_lr, right), drop = FALSE])
  uniq_rl <- unique(temp[, c(key_rl, left),  drop = FALSE])

  lr_ok <- !anyDuplicated(uniq_lr[, key_lr, drop = FALSE])
  rl_ok <- !anyDuplicated(uniq_rl[, key_rl, drop = FALSE])

  lr_ok && rl_ok
}

.select_context_columns <- function(df, key_vars, subject_var = NULL, group_var = NULL, extra = character()) {
  cols <- unique(c(key_vars, subject_var, group_var, extra))
  intersect(cols, names(df))
}

# S7 registers class names with a package-namespace prefix when installed
# (e.g. "aRtifacts::DomainSnapshot"), so base-R inherits(x, "DomainSnapshot")
# always returns FALSE outside devtools::load_all(). Use S7::S7_inherits()
# with the class object — never a string — for reliable type guards.
.assert_domain <- function(x, arg = deparse(substitute(x))) {
  if (!S7::S7_inherits(x, DomainSnapshot)) {
    stop(arg, " must be a DomainSnapshot.", call. = FALSE)
  }
}

.assert_artifact <- function(x, arg = deparse(substitute(x))) {
  if (!S7::S7_inherits(x, ArtifactSnapshot)) {
    stop(arg, " must be an ArtifactSnapshot.", call. = FALSE)
  }
}
