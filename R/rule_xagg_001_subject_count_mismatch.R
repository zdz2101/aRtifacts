#' XAGG-001: Subject count mismatch
#'
#' @param left_domain First domain name.
#' @param right_domain Second domain name.
#' @param subject_var Subject identifier variable.
#' @param tolerance Allowed absolute count difference.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_xagg_001_subject_count_mismatch <- function(left_domain = "DM",
                                                 right_domain = "AE",
                                                 subject_var = "USUBJID",
                                                 tolerance = 0,
                                                 owner_team = "Standards") {
  new_rule(
    id = "XAGG-001",
    title = "Subject count mismatch",
    topology = "A4",
    scope = "snapshot",
    intent = "programming_etl",
    domains = c(left_domain, right_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      left <- .core50_domain_df(domains, left_domain)
      right <- .core50_domain_df(domains, right_domain)
      if (is.null(left) || is.null(right)) {
        return(data.frame())
      }

      n_left <- .core50_distinct_count(left, subject_var)
      n_right <- .core50_distinct_count(right, subject_var)
      if (is.na(n_left) || is.na(n_right)) {
        return(data.frame())
      }

      delta <- abs(n_left - n_right)
      if (delta <= tolerance) {
        return(data.frame())
      }

      data.frame(
        left_domain = left_domain,
        right_domain = right_domain,
        n_left = n_left,
        n_right = n_right,
        diff_n = as.integer(n_left - n_right),
        tolerance = as.integer(tolerance),
        reason = "Distinct subject count mismatch",
        stringsAsFactors = FALSE
      )
    }
  )
}
