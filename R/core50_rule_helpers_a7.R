.core50_events_per_subject_by_group <- function(data, group_var, subject_var) {
  if (!(group_var %in% names(data)) || !(subject_var %in% names(data))) {
    return(data.frame())
  }

  evt <- as.data.frame(table(as.character(data[[group_var]])), stringsAsFactors = FALSE)
  names(evt) <- c("group", "n_events")
  subj <- as.data.frame(table(as.character(unique(data[, c(group_var, subject_var), drop = FALSE])[[group_var]])), stringsAsFactors = FALSE)
  names(subj) <- c("group", "n_subjects")

  out <- merge(evt, subj, by = "group", all = TRUE)
  out[is.na(out)] <- 0
  out$rate <- mapply(.core50_rate, out$n_events, out$n_subjects)
  out
}

.core50_lab_high_rate_by_group <- function(data, group_var, value_var, high_cutoff) {
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
      high_rate = .core50_rate(high_n, n),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
