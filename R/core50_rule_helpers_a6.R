.core50_rate <- function(numerator, denominator) {
  if (is.na(denominator) || denominator == 0) {
    return(NA_real_)
  }
  as.numeric(numerator) / as.numeric(denominator)
}

.core50_missing_rate_by_group <- function(data, group_var, value_var) {
  if (!(group_var %in% names(data)) || !(value_var %in% names(data))) {
    return(data.frame())
  }

  grp <- as.character(data[[group_var]])
  val <- data[[value_var]]
  miss <- is.na(val) | (is.character(val) & !nzchar(trimws(val)))
  groups <- sort(unique(grp))

  out <- lapply(groups, function(g) {
    idx <- grp == g
    n <- sum(idx, na.rm = TRUE)
    n_miss <- sum(miss[idx], na.rm = TRUE)
    data.frame(
      group = g,
      n = as.integer(n),
      n_miss = as.integer(n_miss),
      miss_rate = .core50_rate(n_miss, n),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

.core50_metric_by_group <- function(data, group_var, value_var, fun = mean) {
  if (!(group_var %in% names(data)) || !(value_var %in% names(data))) {
    return(data.frame())
  }

  val <- suppressWarnings(as.numeric(data[[value_var]]))
  grp <- as.character(data[[group_var]])
  groups <- sort(unique(grp))

  rows <- lapply(groups, function(g) {
    x <- val[grp == g]
    x <- x[!is.na(x)]
    metric <- if (!length(x)) NA_real_ else as.numeric(fun(x))
    data.frame(group = g, metric = metric, stringsAsFactors = FALSE)
  })

  do.call(rbind, rows)
}
