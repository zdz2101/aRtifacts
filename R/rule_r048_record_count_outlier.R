rule_r048_record_count_outlier <- function(domain, subject_var = "USUBJID", record_count_iqr_multiplier = 3.0, owner_team = "Standards", ...) {
  new_flag_rule(
    id         = "R048_RECORD_COUNT_OUTLIER",
    domain     = domain,
    severity   = "warning",
    category   = "ops_anomaly",
    owner_team = owner_team,
    fn = function(df) {
      if (!(subject_var %in% names(df))) return(data.frame())
      counts    <- table(df[[subject_var]])
      count_vec <- as.integer(counts)
      if (length(count_vec) < 4L) return(data.frame())
      q1  <- stats::quantile(count_vec, 0.25)
      q3  <- stats::quantile(count_vec, 0.75)
      iqr <- q3 - q1
      if (iqr == 0) return(data.frame())
      hi  <- q3 + record_count_iqr_multiplier * iqr
      bad_subjs <- names(counts)[count_vec > hi]
      if (!length(bad_subjs)) return(data.frame())
      data.frame(
        USUBJID      = bad_subjs,
        RECORD_COUNT = as.integer(counts[bad_subjs]),
        FENCE_HI     = hi,
        stringsAsFactors = FALSE
      )
    }
  )
}
