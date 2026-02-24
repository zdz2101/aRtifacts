rule_r026_no_followup <- function(domain, subject_var = "USUBJID", baseline_flag_var = NULL, owner_team = "DM", ...) {
new_flag_rule(
      id         = "R026_NO_FOLLOWUP",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        bfv <- baseline_flag_var
        if (is.null(bfv) || !(bfv %in% names(df)) || !(subject_var %in% names(df))) {
          return(data.frame())
        }
        bl_subjs    <- unique(df[[subject_var]][!is.na(df[[bfv]]) & df[[bfv]] == "Y"])
        post_subjs  <- unique(df[[subject_var]][is.na(df[[bfv]]) | df[[bfv]] != "Y"])
        no_followup <- setdiff(bl_subjs, post_subjs)
        if (!length(no_followup)) return(data.frame())
        data.frame(
          USUBJID = no_followup,
          ISSUE   = "Baseline record present but no post-baseline records found.",
          stringsAsFactors = FALSE
        )
      }
    )
}
