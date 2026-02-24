#' GRP-004: Site-level outlier rate
#'
#' @param domain Domain name.
#' @param site_var Site/group variable.
#' @param event_flag_var Optional binary event variable (`1`/`0`, `Y`/`N`).
#' @param k Robust z-score threshold.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_grp_004_site_level_outlier_rate <- function(domain,
                                                 site_var = "SITEID",
                                                 event_flag_var = NULL,
                                                 k = 3,
                                                 owner_team = "Clinical Ops") {
  new_rule(
    id = "GRP-004",
    title = "Site-level outlier rate",
    topology = "A2",
    scope = "group",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      if (!(site_var %in% names(data))) {
        return(data.frame())
      }

      event_rate <- NULL
      site_vals <- data[[site_var]]

      if (!is.null(event_flag_var) && (event_flag_var %in% names(data))) {
        ev <- data[[event_flag_var]]
        ev_num <- if (is.character(ev)) {
          as.numeric(toupper(ev) %in% c("Y", "YES", "1", "TRUE"))
        } else {
          suppressWarnings(as.numeric(ev))
        }
        event_rate <- tapply(ev_num, site_vals, function(x) mean(x, na.rm = TRUE))
      } else {
        counts <- table(site_vals)
        event_rate <- as.numeric(counts) / sum(counts)
        names(event_rate) <- names(counts)
      }

      med <- stats::median(event_rate, na.rm = TRUE)
      mad_val <- stats::mad(event_rate, center = med, constant = 1, na.rm = TRUE)
      if (is.na(mad_val) || mad_val == 0) {
        return(data.frame())
      }

      rz <- (event_rate - med) / mad_val
      bad <- names(rz)[abs(rz) > k]
      if (!length(bad)) {
        return(data.frame())
      }

      data.frame(
        site_id = bad,
        event_rate = unname(event_rate[bad]),
        robust_z = unname(rz[bad]),
        threshold = as.numeric(k),
        reason = "Site event rate is a robust-z outlier",
        stringsAsFactors = FALSE
      )
    }
  )
}
