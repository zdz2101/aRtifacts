#' GRP-005: Distribution skew anomaly
#'
#' @param domain Domain name.
#' @param value_var Numeric variable.
#' @param threshold Absolute skewness threshold.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_grp_005_distribution_skew_anomaly <- function(domain,
                                                   value_var,
                                                   threshold = 2,
                                                   owner_team = "Biostats") {
  new_rule(
    id = "GRP-005",
    title = "Distribution skew anomaly",
    topology = "A2",
    scope = "snapshot",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      if (!(value_var %in% names(data))) {
        return(data.frame())
      }

      vals <- suppressWarnings(as.numeric(data[[value_var]]))
      vals <- vals[!is.na(vals)]
      n <- length(vals)
      if (n < 3L) {
        return(data.frame())
      }

      mu <- mean(vals)
      sigma <- stats::sd(vals)
      if (is.na(sigma) || sigma == 0) {
        return(data.frame())
      }

      skew <- sum((vals - mu)^3) / (n * (sigma^3))
      if (abs(skew) <= threshold) {
        return(data.frame())
      }

      data.frame(
        variable = value_var,
        n = n,
        skewness = skew,
        threshold = as.numeric(threshold),
        reason = "Absolute skewness exceeds threshold",
        stringsAsFactors = FALSE
      )
    }
  )
}
