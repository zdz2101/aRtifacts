rule_r027_missing_baseline <- function(domain, subject_var = "USUBJID", baseline_flag_var = NULL, owner_team = "DM", ...) {
new_flag_rule(
      id         = "R027_MISSING_BASELINE",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        bfv <- baseline_flag_var
        if (is.null(bfv) || !(bfv %in% names(df)) || !(subject_var %in% names(df))) {
          return(data.frame())
        }
        all_subjs    <- unique(df[[subject_var]])
        bl_subjs     <- unique(df[[subject_var]][!is.na(df[[bfv]]) & df[[bfv]] == "Y"])
        missing_bl   <- setdiff(all_subjs, bl_subjs)
        if (!length(missing_bl)) return(data.frame())
        data.frame(
          USUBJID = missing_bl,
          ISSUE   = "No baseline record found for this subject.",
          stringsAsFactors = FALSE
        )
      }
    )
}
