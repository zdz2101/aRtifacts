.core50_distinct_count <- function(df, var) {
  if (is.null(df) || !(var %in% names(df))) {
    return(NA_integer_)
  }
  as.integer(length(unique(df[[var]][!is.na(df[[var]])])))
}

.core50_count_by <- function(df, by, value_var = NULL) {
  if (is.null(df) || !all(by %in% names(df))) {
    return(data.frame())
  }

  key <- do.call(paste, c(df[by], sep = "||"))
  n_tab <- table(key)
  n <- as.integer(n_tab)
  key_names <- names(n_tab)
  split_vals <- strsplit(key_names, "\\|\\|", fixed = FALSE)

  out <- as.data.frame(do.call(rbind, split_vals), stringsAsFactors = FALSE)
  names(out) <- by
  out$n <- n

  if (!is.null(value_var) && (value_var %in% names(df))) {
    val_map <- tapply(df[[value_var]], key, function(x) mean(as.numeric(x), na.rm = TRUE))
    out$metric <- as.numeric(val_map[key_names])
  }

  out
}

.core50_full_join_by <- function(x, y, by, suffix = c("_x", "_y")) {
  if (!all(by %in% names(x)) || !all(by %in% names(y))) {
    return(data.frame())
  }
  merge(x, y, by = by, all = TRUE, suffixes = suffix)
}

.core50_safe_rate <- function(num, den) {
  if (is.na(den) || den == 0) {
    return(NA_real_)
  }
  as.numeric(num) / as.numeric(den)
}
