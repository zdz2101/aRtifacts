#' XDOM-003: LB without expected visit
#'
#' @param lb_visit_var Visit variable in LB.
#' @param vs_visit_var Visit variable in VS.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_xdom_003_lb_without_expected_visit <- function(lb_visit_var = "VISIT",
                                                    vs_visit_var = "VISIT",
                                                    owner_team = "Standards") {
  new_rule(
    id = "XDOM-003",
    title = "LB without expected visit",
    topology = "A3",
    scope = "record",
    intent = "programming_etl",
    domains = c("LB", "VS"),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      anti_join_rows <- get(".core50_anti_join_rows", mode = "function", inherits = TRUE)

      lb <- .core50_domain_df(domains, "LB")
      vs <- .core50_domain_df(domains, "VS")
      needed_lb <- c("USUBJID", lb_visit_var)
      needed_vs <- c("USUBJID", vs_visit_var)
      if (is.null(lb) || is.null(vs) || !all(needed_lb %in% names(lb)) || !all(needed_vs %in% names(vs))) {
        return(data.frame())
      }

      lb_key <- data.frame(USUBJID = lb[["USUBJID"]], VISIT = lb[[lb_visit_var]], stringsAsFactors = FALSE)
      vs_key <- data.frame(USUBJID = vs[["USUBJID"]], VISIT = vs[[vs_visit_var]], stringsAsFactors = FALSE)
      bad_key <- anti_join_rows(lb_key, vs_key, by = c("USUBJID", "VISIT"))
      if (!nrow(bad_key)) {
        return(data.frame())
      }

      out <- bad_key
      out$reason <- "LB subject/visit not found in VS"
      out
    }
  )
}
