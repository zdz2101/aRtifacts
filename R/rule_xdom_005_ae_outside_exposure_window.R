#' XDOM-005: AE outside exposure window
#'
#' @param window_days Allowed window extension in days.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_xdom_005_ae_outside_exposure_window <- function(window_days = 0,
                                                     owner_team = "Medical Monitor") {
  new_rule(
    id = "XDOM-005",
    title = "AE outside exposure window",
    topology = "A3",
    scope = "record",
    intent = "clinical_review",
    domains = c("AE", "EX"),
    owner_team = owner_team,
    match_spec = new_match_spec(by = "USUBJID", join_type = "inner", window_days = window_days),
    fn = function(domains, context, rule) {
      ae <- .core50_domain_df(domains, "AE")
      ex <- .core50_domain_df(domains, "EX")
      needed_ae <- c("USUBJID", "AESTDTC")
      needed_ex <- c("USUBJID", "EXSTDTC", "EXENDTC")
      if (is.null(ae) || is.null(ex) || !all(needed_ae %in% names(ae)) || !all(needed_ex %in% names(ex))) {
        return(data.frame())
      }

      ae_dt <- .core50_parse_date(ae$AESTDTC)
      ex_start <- .core50_parse_date(ex$EXSTDTC)
      ex_end <- .core50_parse_date(ex$EXENDTC)

      bad <- logical(nrow(ae))
      for (i in seq_len(nrow(ae))) {
        sid <- ae$USUBJID[[i]]
        rel <- which(ex$USUBJID == sid)
        if (!length(rel)) {
          bad[[i]] <- TRUE
          next
        }
        ok <- .core50_within_any_window(ae_dt[[i]], ex_start[rel], ex_end[rel], window_days = window_days)
        bad[[i]] <- !ok
      }

      if (!any(bad)) {
        return(data.frame())
      }

      out <- ae[bad, intersect(c("USUBJID", "AESEQ", "AESTDTC"), names(ae)), drop = FALSE]
      out$reason <- "AE date outside exposure window"
      out
    }
  )
}
