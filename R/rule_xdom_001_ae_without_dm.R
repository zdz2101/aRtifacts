#' XDOM-001: AE without DM
#'
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_xdom_001_ae_without_dm <- function(owner_team = "DM") {
  new_rule(
    id = "XDOM-001",
    title = "AE without DM",
    topology = "A3",
    scope = "record",
    intent = "core_integrity",
    domains = c("AE", "DM"),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      anti_join_rows <- get(".core50_anti_join_rows", mode = "function", inherits = TRUE)

      ae <- .core50_domain_df(domains, "AE")
      dm <- .core50_domain_df(domains, "DM")
      if (is.null(ae) || is.null(dm) || !("USUBJID" %in% names(ae)) || !("USUBJID" %in% names(dm))) {
        return(data.frame())
      }

      bad <- anti_join_rows(ae, dm, by = "USUBJID")
      if (!nrow(bad)) {
        return(data.frame())
      }

      out <- bad[, intersect(c("USUBJID", "AESEQ", "AETERM"), names(bad)), drop = FALSE]
      out$reason <- "AE record has no matching DM subject"
      out
    }
  )
}
