#' XDOM-004: CM outside AE window
#'
#' @param window_days Allowed window extension in days.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_xdom_004_cm_outside_ae_window <- function(window_days = 7,
                                               owner_team = "Medical Monitor") {
  new_rule(
    id = "XDOM-004",
    title = "CM outside AE window",
    topology = "A3",
    scope = "record",
    intent = "clinical_review",
    domains = c("CM", "AE"),
    owner_team = owner_team,
    match_spec = new_match_spec(by = "USUBJID", join_type = "inner", window_days = window_days),
    fn = function(domains, context, rule) {
      cm <- .core50_domain_df(domains, "CM")
      ae <- .core50_domain_df(domains, "AE")
      needed_cm <- c("USUBJID", "CMSTDTC")
      needed_ae <- c("USUBJID", "AESTDTC", "AEENDTC")
      if (is.null(cm) || is.null(ae) || !all(needed_cm %in% names(cm)) || !all(needed_ae %in% names(ae))) {
        return(data.frame())
      }

      cm_dt <- .core50_parse_date(cm$CMSTDTC)
      ae_start <- .core50_parse_date(ae$AESTDTC)
      ae_end <- .core50_parse_date(ae$AEENDTC)

      bad <- logical(nrow(cm))
      for (i in seq_len(nrow(cm))) {
        sid <- cm$USUBJID[[i]]
        rel <- which(ae$USUBJID == sid)
        if (!length(rel)) {
          bad[[i]] <- TRUE
          next
        }
        ok <- .core50_within_any_window(cm_dt[[i]], ae_start[rel], ae_end[rel], window_days = window_days)
        bad[[i]] <- !ok
      }

      if (!any(bad)) {
        return(data.frame())
      }

      out <- cm[bad, intersect(c("USUBJID", "CMSEQ", "CMSTDTC"), names(cm)), drop = FALSE]
      out$reason <- "CM start date outside AE window"
      out
    }
  )
}
