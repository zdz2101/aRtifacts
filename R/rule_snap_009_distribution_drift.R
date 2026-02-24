#' SNAP-009: Distribution drift
#'
#' @param domain Domain name.
#' @param value_var Numeric variable for distribution comparison.
#' @param ks_threshold KS-statistic threshold.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_snap_009_distribution_drift <- function(domain = "ADSL",
                                             value_var = "AGE",
                                             ks_threshold = 0.2,
                                             owner_team = "Clinical Ops") {
  new_rule(
    id = "SNAP-009",
    title = "Distribution drift",
    topology = "A6",
    scope = "snapshot",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      if (!(value_var %in% names(previous)) || !(value_var %in% names(current))) {
        return(data.frame())
      }

      prev_df <- as.data.frame(previous, stringsAsFactors = FALSE)
      cur_df <- as.data.frame(current, stringsAsFactors = FALSE)
      prev_df$.ALL <- "ALL"
      cur_df$.ALL <- "ALL"
      prev_df$.ROWID <- seq_len(nrow(prev_df))
      cur_df$.ROWID <- seq_len(nrow(cur_df))

      prev_dom <- new_domain_snapshot(name = domain, data = prev_df, key_vars = ".ROWID")
      cur_dom <- new_domain_snapshot(name = domain, data = cur_df, key_vars = ".ROWID")
      drift <- compare_distribution_shift(prev_dom, cur_dom, var = value_var, by = ".ALL")

      if (!nrow(drift)) {
        return(data.frame())
      }

      stat <- as.numeric(drift$ks_stat[[1]])
      if (is.na(stat) || stat <= ks_threshold) {
        return(data.frame())
      }

      data.frame(
        value_var = value_var,
        n_prev = as.integer(drift$n_old[[1]]),
        n_cur = as.integer(drift$n_new[[1]]),
        ks_stat = stat,
        ks_p_value = as.numeric(drift$ks_p_value[[1]]),
        threshold = as.numeric(ks_threshold),
        reason = "Distribution drift exceeds KS threshold",
        stringsAsFactors = FALSE
      )
    }
  )
}
