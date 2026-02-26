.topology_domain_df <- function(domains, name) {
  if (is.null(domains) || !(name %in% names(domains))) {
    return(NULL)
  }
  domains[[name]]
}

.topology_anti_join_rows <- function(x, y, by) {
  by <- intersect(by, intersect(names(x), names(y)))
  if (!length(by)) {
    return(data.frame())
  }

  x_key <- .key_id(x, by)
  y_key <- .key_id(y, by)
  x[!(x_key %in% y_key), , drop = FALSE]
}

.topology_parse_date <- function(x) {
  suppressWarnings(as.Date(as.character(x)))
}

.topology_within_any_window <- function(index_date, start_dates, end_dates, window_days = 0) {
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

.topology_distinct_count <- function(df, var) {
  if (is.null(df) || !(var %in% names(df))) {
    return(NA_integer_)
  }
  as.integer(length(unique(df[[var]][!is.na(df[[var]])])))
}

.topology_count_by <- function(df, by) {
  if (is.null(df) || !all(by %in% names(df))) {
    return(data.frame())
  }

  key <- do.call(paste, c(df[by], sep = "||"))
  n_tab <- table(key)
  key_names <- names(n_tab)
  split_vals <- strsplit(key_names, "\\|\\|", fixed = FALSE)

  out <- as.data.frame(do.call(rbind, split_vals), stringsAsFactors = FALSE)
  names(out) <- by
  out$n <- as.integer(n_tab)
  out
}

.topology_full_join_by <- function(x, y, by, suffix = c("_x", "_y")) {
  if (!all(by %in% names(x)) || !all(by %in% names(y))) {
    return(data.frame())
  }
  merge(x, y, by = by, all = TRUE, suffixes = suffix)
}

.topology_safe_rate <- function(num, den) {
  if (is.na(den) || den == 0) {
    return(NA_real_)
  }
  as.numeric(num) / as.numeric(den)
}

.topology_events_per_entity_by_group <- function(data, group_var, entity_var) {
  if (!(group_var %in% names(data)) || !(entity_var %in% names(data))) {
    return(data.frame())
  }

  evt <- as.data.frame(table(as.character(data[[group_var]])), stringsAsFactors = FALSE)
  names(evt) <- c("group", "n_events")

  uniq <- unique(data[, c(group_var, entity_var), drop = FALSE])
  ent <- as.data.frame(table(as.character(uniq[[group_var]])), stringsAsFactors = FALSE)
  names(ent) <- c("group", "n_entities")

  out <- merge(evt, ent, by = "group", all = TRUE)
  out[is.na(out)] <- 0
  out$rate <- mapply(.topology_safe_rate, out$n_events, out$n_entities)
  out
}

.topology_metric_high_rate_by_group <- function(data, group_var, value_var, high_cutoff) {
  if (!(group_var %in% names(data)) || !(value_var %in% names(data))) {
    return(data.frame())
  }

  grp <- as.character(data[[group_var]])
  val <- suppressWarnings(as.numeric(data[[value_var]]))
  groups <- sort(unique(grp))

  rows <- lapply(groups, function(g) {
    x <- val[grp == g]
    x <- x[!is.na(x)]
    n <- length(x)
    high_n <- if (!n) 0L else sum(x > high_cutoff, na.rm = TRUE)

    data.frame(
      group = g,
      n_obs = as.integer(n),
      high_n = as.integer(high_n),
      high_rate = .topology_safe_rate(high_n, n),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
