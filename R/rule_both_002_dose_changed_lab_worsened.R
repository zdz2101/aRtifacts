#' BOTH-002: Dose changed and lab worsened
#'
#' @param ex_domain EX domain name.
#' @param lb_domain LB domain name.
#' @param subject_var Subject identifier variable.
#' @param dose_var Dose variable in EX.
#' @param lab_var Lab numeric variable in LB.
#' @param dose_delta_min Minimum dose increase.
#' @param lab_delta_min Minimum lab worsening.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_both_002_dose_changed_lab_worsened <- function(ex_domain = "EX",
                                                     lb_domain = "LB",
                                                     subject_var = "USUBJID",
                                                     dose_var = "EXDOSE",
                                                     lab_var = "LBSTRESN",
                                                     dose_delta_min = 0,
                                                     lab_delta_min = 0,
                                                     owner_team = "Clinical Review") {
  new_rule(
    id = "BOTH-002",
    title = "Dose changed + lab worsened",
    topology = "A7",
    scope = "group",
    intent = "clinical_review",
    domains = c(ex_domain, lb_domain),
    owner_team = owner_team,
    identity_spec = new_identity_spec(keys = c(subject_var), method = "exact"),
    fn = function(previous, current, context, rule) {
      ex_prev <- .core50_domain_df(previous, ex_domain)
      ex_cur <- .core50_domain_df(current, ex_domain)
      lb_prev <- .core50_domain_df(previous, lb_domain)
      lb_cur <- .core50_domain_df(current, lb_domain)

      need_ex <- c(subject_var, dose_var)
      need_lb <- c(subject_var, lab_var)
      if (is.null(ex_prev) || is.null(ex_cur) || is.null(lb_prev) || is.null(lb_cur) ||
          !all(need_ex %in% names(ex_prev)) || !all(need_ex %in% names(ex_cur)) ||
          !all(need_lb %in% names(lb_prev)) || !all(need_lb %in% names(lb_cur))) {
        return(data.frame())
      }

      dose_prev <- stats::aggregate(suppressWarnings(as.numeric(ex_prev[[dose_var]])), by = list(ex_prev[[subject_var]]), FUN = mean, na.rm = TRUE)
      names(dose_prev) <- c(subject_var, "dose_prev")
      dose_cur <- stats::aggregate(suppressWarnings(as.numeric(ex_cur[[dose_var]])), by = list(ex_cur[[subject_var]]), FUN = mean, na.rm = TRUE)
      names(dose_cur) <- c(subject_var, "dose_cur")

      lab_prev_max <- stats::aggregate(suppressWarnings(as.numeric(lb_prev[[lab_var]])), by = list(lb_prev[[subject_var]]), FUN = max, na.rm = TRUE)
      names(lab_prev_max) <- c(subject_var, "lab_prev")
      lab_cur_max <- stats::aggregate(suppressWarnings(as.numeric(lb_cur[[lab_var]])), by = list(lb_cur[[subject_var]]), FUN = max, na.rm = TRUE)
      names(lab_cur_max) <- c(subject_var, "lab_cur")

      merged <- merge(dose_prev, dose_cur, by = subject_var, all = FALSE)
      merged <- merge(merged, lab_prev_max, by = subject_var, all = FALSE)
      merged <- merge(merged, lab_cur_max, by = subject_var, all = FALSE)
      if (!nrow(merged)) {
        return(data.frame())
      }

      merged$dose_delta <- merged$dose_cur - merged$dose_prev
      merged$lab_delta <- merged$lab_cur - merged$lab_prev
      bad <- merged$dose_delta > dose_delta_min & merged$lab_delta > lab_delta_min
      if (!any(bad, na.rm = TRUE)) {
        return(data.frame())
      }

      out <- merged[bad, c(subject_var, "dose_prev", "dose_cur", "dose_delta", "lab_prev", "lab_cur", "lab_delta"), drop = FALSE]
      out$reason <- "Dose increased while lab worsened"
      out
    }
  )
}
