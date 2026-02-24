rule_r016_visit_order <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", visit_order_var = NULL, owner_team = "DM", ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  if (!is.null(visit_order_var)) {
    new_flag_rule(
      id         = "R016_VISIT_ORDER",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        if (!(subject_var %in% names(df) && visit_order_var %in% names(df))) {
          return(data.frame())
        }
        grps <- split(seq_len(nrow(df)), df[[subject_var]])
        bad_idx <- integer(0)
        for (idx in grps) {
          visits <- df[[visit_order_var]][idx]
          if (!is.numeric(visits)) next
          diffs <- diff(visits)
          bad_positions <- which(diffs < 0)
          if (length(bad_positions)) {
            bad_idx <- c(bad_idx, idx[bad_positions + 1L])
          }
        }
        if (!length(bad_idx)) return(data.frame())
        df[bad_idx, .ctx(df, visit_order_var), drop = FALSE]
      }
    )
  }
}
