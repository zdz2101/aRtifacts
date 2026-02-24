#' BOTH-001: New AE without new EX
#'
#' @param ae_domain AE domain name.
#' @param ex_domain EX domain name.
#' @param ae_keys AE identity keys.
#' @param match_keys Keys used to match AE to EX.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_both_001_new_ae_without_new_ex <- function(ae_domain = "AE",
                                                 ex_domain = "EX",
                                                 ae_keys = c("USUBJID", "AESEQ"),
                                                 match_keys = c("USUBJID"),
                                                 owner_team = "Clinical Review") {
  new_rule(
    id = "BOTH-001",
    title = "New AE without new EX",
    topology = "A7",
    scope = "record",
    intent = "clinical_review",
    domains = c(ae_domain, ex_domain),
    owner_team = owner_team,
    identity_spec = new_identity_spec(keys = ae_keys, method = "exact"),
    match_spec = new_match_spec(by = match_keys, join_type = "anti"),
    fn = function(previous, current, context, rule) {
      ae_prev <- .core50_domain_df(previous, ae_domain)
      ae_cur <- .core50_domain_df(current, ae_domain)
      ex_cur <- .core50_domain_df(current, ex_domain)
      if (is.null(ae_prev) || is.null(ae_cur) || is.null(ex_cur)) {
        return(data.frame())
      }

      by_ae <- intersect(ae_keys, intersect(names(ae_prev), names(ae_cur)))
      by_match <- intersect(match_keys, intersect(names(ae_cur), names(ex_cur)))
      if (!length(by_ae) || !length(by_match)) {
        return(data.frame())
      }

      new_ae <- .core50_anti_join_rows(ae_cur, ae_prev, by = by_ae)
      if (!nrow(new_ae)) {
        return(data.frame())
      }

      bad <- .core50_anti_join_rows(new_ae, ex_cur, by = by_match)
      if (!nrow(bad)) {
        return(data.frame())
      }

      keep <- unique(c(by_ae, by_match))
      out <- bad[, intersect(keep, names(bad)), drop = FALSE]
      out$reason <- "New AE has no matching EX in current cut"
      out
    }
  )
}
