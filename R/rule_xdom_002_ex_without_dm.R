#' XDOM-002: EX without DM
#'
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_xdom_002_ex_without_dm <- function(owner_team = "DM") {
  new_rule(
    id = "XDOM-002",
    title = "EX without DM",
    topology = "A3",
    scope = "record",
    intent = "core_integrity",
    domains = c("EX", "DM"),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      anti_join_rows <- get(".core50_anti_join_rows", mode = "function", inherits = TRUE)

      ex <- .core50_domain_df(domains, "EX")
      dm <- .core50_domain_df(domains, "DM")
      if (is.null(ex) || is.null(dm) || !("USUBJID" %in% names(ex)) || !("USUBJID" %in% names(dm))) {
        return(data.frame())
      }

      bad <- anti_join_rows(ex, dm, by = "USUBJID")
      if (!nrow(bad)) {
        return(data.frame())
      }

      out <- bad[, intersect(c("USUBJID", "EXSEQ", "EXTRT"), names(bad)), drop = FALSE]
      out$reason <- "EX record has no matching DM subject"
      out
    }
  )
}
