#' BOTH-003: Subject removed from DM but present elsewhere
#'
#' @param dm_domain DM domain name.
#' @param other_domain Other domain to check current presence.
#' @param subject_var Subject identifier variable.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_both_003_subject_removed_dm_present_elsewhere <- function(dm_domain = "DM",
                                                                other_domain = "AE",
                                                                subject_var = "USUBJID",
                                                                owner_team = "Standards") {
  new_rule(
    id = "BOTH-003",
    title = "Subject removed from DM but present elsewhere",
    topology = "A7",
    scope = "record",
    intent = "programming_etl",
    domains = c(dm_domain, other_domain),
    owner_team = owner_team,
    identity_spec = new_identity_spec(keys = c(subject_var), method = "exact"),
    match_spec = new_match_spec(by = c(subject_var), join_type = "semi"),
    fn = function(previous, current, context, rule) {
      dm_prev <- .core50_domain_df(previous, dm_domain)
      dm_cur <- .core50_domain_df(current, dm_domain)
      oth_cur <- .core50_domain_df(current, other_domain)
      if (is.null(dm_prev) || is.null(dm_cur) || is.null(oth_cur) ||
          !(subject_var %in% names(dm_prev)) || !(subject_var %in% names(dm_cur)) || !(subject_var %in% names(oth_cur))) {
        return(data.frame())
      }

      removed_dm <- .core50_anti_join_rows(unique(dm_prev[, subject_var, drop = FALSE]), unique(dm_cur[, subject_var, drop = FALSE]), by = subject_var)
      if (!nrow(removed_dm)) {
        return(data.frame())
      }

      oth_ids <- unique(oth_cur[, subject_var, drop = FALSE])
      bad <- merge(removed_dm, oth_ids, by = subject_var, all = FALSE)
      if (!nrow(bad)) {
        return(data.frame())
      }

      bad$reason <- "Subject removed in DM but still present in another domain"
      bad
    }
  )
}
