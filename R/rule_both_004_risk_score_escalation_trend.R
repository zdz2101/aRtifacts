#' BOTH-004: Risk score escalation trend
#'
#' @param domains_set Domains used to compute risk score.
#' @param group_var Grouping variable.
#' @param subject_var Subject identifier variable.
#' @param lb_var Numeric LB variable.
#' @param lb_high_cutoff LB high threshold.
#' @param risk_delta_threshold Minimum increase in composite risk score.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_both_004_risk_score_escalation_trend <- function(domains_set = c("AE", "EX", "LB"),
                                                       group_var = "SITEID",
                                                       subject_var = "USUBJID",
                                                       lb_var = "LBSTRESN",
                                                       lb_high_cutoff = 200,
                                                       risk_delta_threshold = 0.5,
                                                       owner_team = "Clinical Ops") {
  new_rule(
    id = "BOTH-004",
    title = "Risk score escalation trend",
    topology = "A7",
    scope = "group",
    intent = "ops_anomaly",
    domains = domains_set,
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      f_events_per_subject <- get(".core50_events_per_subject_by_group", mode = "function", inherits = TRUE)
      f_lab_high_rate <- get(".core50_lab_high_rate_by_group", mode = "function", inherits = TRUE)

      ae_prev <- .core50_domain_df(previous, "AE")
      ae_cur <- .core50_domain_df(current, "AE")
      ex_prev <- .core50_domain_df(previous, "EX")
      ex_cur <- .core50_domain_df(current, "EX")
      lb_prev <- .core50_domain_df(previous, "LB")
      lb_cur <- .core50_domain_df(current, "LB")

      if (is.null(ae_prev) || is.null(ae_cur) || is.null(ex_prev) || is.null(ex_cur) || is.null(lb_prev) || is.null(lb_cur)) {
        return(data.frame())
      }

      ae_prev_m <- f_events_per_subject(ae_prev, group_var = group_var, subject_var = subject_var)
      ae_cur_m <- f_events_per_subject(ae_cur, group_var = group_var, subject_var = subject_var)
      ex_prev_m <- f_events_per_subject(ex_prev, group_var = group_var, subject_var = subject_var)
      ex_cur_m <- f_events_per_subject(ex_cur, group_var = group_var, subject_var = subject_var)
      lb_prev_m <- f_lab_high_rate(lb_prev, group_var = group_var, value_var = lb_var, high_cutoff = lb_high_cutoff)
      lb_cur_m <- f_lab_high_rate(lb_cur, group_var = group_var, value_var = lb_var, high_cutoff = lb_high_cutoff)

      if (!nrow(ae_prev_m) || !nrow(ae_cur_m) || !nrow(ex_prev_m) || !nrow(ex_cur_m) || !nrow(lb_prev_m) || !nrow(lb_cur_m)) {
        return(data.frame())
      }

      p <- merge(ae_prev_m[, c("group", "rate")], ex_prev_m[, c("group", "rate")], by = "group", all = TRUE)
      names(p) <- c("group", "ae_rate_prev", "ex_rate_prev")
      p <- merge(p, lb_prev_m[, c("group", "high_rate")], by = "group", all = TRUE)
      names(p)[names(p) == "high_rate"] <- "lb_high_rate_prev"

      c <- merge(ae_cur_m[, c("group", "rate")], ex_cur_m[, c("group", "rate")], by = "group", all = TRUE)
      names(c) <- c("group", "ae_rate_cur", "ex_rate_cur")
      c <- merge(c, lb_cur_m[, c("group", "high_rate")], by = "group", all = TRUE)
      names(c)[names(c) == "high_rate"] <- "lb_high_rate_cur"

      merged <- merge(p, c, by = "group", all = TRUE)
      merged[is.na(merged)] <- 0

      merged$risk_prev <- merged$ae_rate_prev + merged$ex_rate_prev + merged$lb_high_rate_prev
      merged$risk_cur <- merged$ae_rate_cur + merged$ex_rate_cur + merged$lb_high_rate_cur
      merged$risk_delta <- merged$risk_cur - merged$risk_prev

      bad <- merged$risk_delta > risk_delta_threshold
      if (!any(bad)) {
        return(data.frame())
      }

      out <- merged[bad, c("group", "risk_prev", "risk_cur", "risk_delta", "ae_rate_prev", "ae_rate_cur", "lb_high_rate_prev", "lb_high_rate_cur"), drop = FALSE]
      names(out)[names(out) == "group"] <- group_var
      out$threshold <- as.numeric(risk_delta_threshold)
      out$reason <- "Composite risk score increased beyond threshold"
      out
    }
  )
}
