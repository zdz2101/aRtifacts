.core50_domain_df <- function(domains, name) {
  if (is.null(domains) || !(name %in% names(domains))) {
    return(NULL)
  }
  domains[[name]]
}

.core50_anti_join_rows <- function(x, y, by) {
  by <- intersect(by, intersect(names(x), names(y)))
  if (!length(by)) {
    return(data.frame())
  }

  x_key <- .key_id(x, by)
  y_key <- .key_id(y, by)
  x[!(x_key %in% y_key), , drop = FALSE]
}

.core50_parse_date <- function(x) {
  suppressWarnings(as.Date(as.character(x)))
}

.core50_within_any_window <- function(index_date, start_dates, end_dates, window_days = 7) {
  if (is.na(index_date)) {
    return(FALSE)
  }

  start_dates <- start_dates[!is.na(start_dates)]
  end_dates <- end_dates[!is.na(end_dates)]
  if (!length(start_dates) || !length(end_dates)) {
    return(FALSE)
  }

  min_start <- min(start_dates) - as.numeric(window_days)
  max_end <- max(end_dates) + as.numeric(window_days)
  index_date >= min_start && index_date <= max_end
}
