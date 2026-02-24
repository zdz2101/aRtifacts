#' XDOM-006: Visit mismatch across domains
#'
#' @param left_domain First domain name.
#' @param right_domain Second domain name.
#' @param subject_var Subject variable.
#' @param left_visit_var Visit variable in first domain.
#' @param right_visit_var Visit variable in second domain.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_xdom_006_visit_mismatch_across_domains <- function(left_domain = "VS",
                                                        right_domain = "LB",
                                                        subject_var = "USUBJID",
                                                        left_visit_var = "VISIT",
                                                        right_visit_var = "VISIT",
                                                        owner_team = "Standards") {
  new_rule(
    id = "XDOM-006",
    title = "Visit mismatch across domains",
    topology = "A3",
    scope = "group",
    intent = "programming_etl",
    domains = c(left_domain, right_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      left <- .core50_domain_df(domains, left_domain)
      right <- .core50_domain_df(domains, right_domain)
      needed_left <- c(subject_var, left_visit_var)
      needed_right <- c(subject_var, right_visit_var)
      if (is.null(left) || is.null(right) || !all(needed_left %in% names(left)) || !all(needed_right %in% names(right))) {
        return(data.frame())
      }

      left_key <- data.frame(
        SUBJECT = left[[subject_var]],
        VISIT = left[[left_visit_var]],
        stringsAsFactors = FALSE
      )
      right_key <- data.frame(
        SUBJECT = right[[subject_var]],
        VISIT = right[[right_visit_var]],
        stringsAsFactors = FALSE
      )

      only_left <- .core50_anti_join_rows(left_key, right_key, by = c("SUBJECT", "VISIT"))
      only_right <- .core50_anti_join_rows(right_key, left_key, by = c("SUBJECT", "VISIT"))
      if (!nrow(only_left) && !nrow(only_right)) {
        return(data.frame())
      }

      out <- data.frame(stringsAsFactors = FALSE)
      if (nrow(only_left)) {
        tmp <- only_left
        tmp$side <- paste0(left_domain, "_only")
        out <- rbind(out, tmp)
      }
      if (nrow(only_right)) {
        tmp <- only_right
        tmp$side <- paste0(right_domain, "_only")
        out <- rbind(out, tmp)
      }

      out$reason <- "Subject/visit pair mismatch across domains"
      out
    }
  )
}
