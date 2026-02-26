.topology_any_na_or_blank <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  is.na(x) | (is.character(x) & !nzchar(trimws(x)))
}

.topology_row_output <- function(data, bad, fields = character(), reason = "") {
  if (!length(bad) || !any(bad, na.rm = TRUE)) {
    return(data.frame())
  }

  idx <- which(isTRUE(bad) | (!is.na(bad) & bad))
  keep <- intersect(fields, names(data))

  if (length(keep)) {
    out <- data[idx, keep, drop = FALSE]
  } else {
    out <- data.frame(stringsAsFactors = FALSE)
  }

  out$row_id <- idx
  out$reason <- reason
  out
}

.topology_iso_date_bad <- function(x) {
  vals <- as.character(x)
  present <- !is.na(vals) & nzchar(trimws(vals))
  present & !grepl("^\\d{4}-\\d{2}-\\d{2}$", vals)
}
