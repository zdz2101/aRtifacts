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

      prev_x <- suppressWarnings(as.numeric(previous[[value_var]]))
      cur_x <- suppressWarnings(as.numeric(current[[value_var]]))
      prev_x <- prev_x[!is.na(prev_x)]
      cur_x <- cur_x[!is.na(cur_x)]
      if (length(prev_x) < 2L || length(cur_x) < 2L) {
        return(data.frame())
      }

      ks <- suppressWarnings(stats::ks.test(prev_x, cur_x))
      stat <- as.numeric(ks$statistic)
      if (is.na(stat) || stat <= ks_threshold) {
        return(data.frame())
      }

      data.frame(
        value_var = value_var,
        n_prev = as.integer(length(prev_x)),
        n_cur = as.integer(length(cur_x)),
        ks_stat = stat,
        ks_p_value = as.numeric(ks$p.value),
        threshold = as.numeric(ks_threshold),
        reason = "Distribution drift exceeds KS threshold",
        stringsAsFactors = FALSE
      )
    }
  )
}
