.core50_has_columns <- function(df, cols) {
  all(cols %in% names(df))
}

.core50_changed_mask <- function(prev, cur, numeric_tol = 0) {
  same_na <- is.na(prev) & is.na(cur)
  one_na <- xor(is.na(prev), is.na(cur))

  if (is.numeric(prev) && is.numeric(cur)) {
    diff <- suppressWarnings(abs(prev - cur))
    changed <- one_na | (!same_na & !is.na(diff) & diff > as.numeric(numeric_tol))
    changed[is.na(changed)] <- one_na[is.na(changed)]
    return(changed)
  }

  prev_chr <- as.character(prev)
  cur_chr <- as.character(cur)
  changed <- one_na | (!same_na & prev_chr != cur_chr)
  changed[is.na(changed)] <- one_na[is.na(changed)]
  changed
}

.core50_key_frame <- function(df, keys) {
  if (is.null(df) || !nrow(df) || !length(keys) || !all(keys %in% names(df))) {
    return(data.frame())
  }
  unique(df[, keys, drop = FALSE])
}
