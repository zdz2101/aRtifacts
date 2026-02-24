#' XAGG-004: Exposure vs event rate discrepancy
#'
#' @param ae_domain AE domain name.
#' @param ex_domain EX domain name.
#' @param group_var Grouping variable.
#' @param ae_high_threshold AE rate high threshold.
#' @param ex_low_threshold EX completeness low threshold.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_xagg_004_exposure_event_rate_discrepancy <- function(ae_domain = "AE",
                                                          ex_domain = "EX",
                                                          group_var = "SITEID",
                                                          ae_high_threshold = 0.5,
                                                          ex_low_threshold = 0.5,
                                                          owner_team = "Clinical Ops") {
  new_rule(
    id = "XAGG-004",
    title = "Exposure vs event rate discrepancy",
    topology = "A4",
    scope = "group",
    intent = "ops_anomaly",
    domains = c(ae_domain, ex_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      ae <- .core50_domain_df(domains, ae_domain)
      ex <- .core50_domain_df(domains, ex_domain)
      needed <- c(group_var, "USUBJID")
      if (is.null(ae) || is.null(ex) || !all(needed %in% names(ae)) || !all(needed %in% names(ex))) {
        return(data.frame())
      }

      ae_counts <- as.data.frame(table(ae[[group_var]]), stringsAsFactors = FALSE)
      names(ae_counts) <- c(group_var, "ae_n")
      ex_counts <- as.data.frame(table(ex[[group_var]]), stringsAsFactors = FALSE)
      names(ex_counts) <- c(group_var, "ex_n")
      ae_subj <- as.data.frame(table(unique(ae[, c(group_var, "USUBJID")])[[group_var]]), stringsAsFactors = FALSE)
      names(ae_subj) <- c(group_var, "ae_subject_n")
      ex_subj <- as.data.frame(table(unique(ex[, c(group_var, "USUBJID")])[[group_var]]), stringsAsFactors = FALSE)
      names(ex_subj) <- c(group_var, "ex_subject_n")

      merged <- merge(ae_counts, ex_counts, by = group_var, all = TRUE)
      merged <- merge(merged, ae_subj, by = group_var, all = TRUE)
      merged <- merge(merged, ex_subj, by = group_var, all = TRUE)
      merged[is.na(merged)] <- 0

      merged$ae_rate <- mapply(.core50_safe_rate, merged$ae_n, merged$ae_subject_n)
      merged$ex_rate <- mapply(.core50_safe_rate, merged$ex_n, merged$ex_subject_n)

      bad <- !is.na(merged$ae_rate) & !is.na(merged$ex_rate) &
        (merged$ae_rate > ae_high_threshold) & (merged$ex_rate < ex_low_threshold)
      if (!any(bad)) {
        return(data.frame())
      }

      out <- merged[bad, c(group_var, "ae_rate", "ex_rate", "ae_n", "ex_n"), drop = FALSE]
      out$ae_high_threshold <- as.numeric(ae_high_threshold)
      out$ex_low_threshold <- as.numeric(ex_low_threshold)
      out$reason <- "High AE rate with low EX completeness"
      out
    }
  )
}
