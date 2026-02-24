#' XDOM-007: Orphan records in child domain
#'
#' @param child_domain Child domain name.
#' @param parent_domain Parent domain name.
#' @param parent_keys Key mapping used to resolve parent records.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_xdom_007_orphan_child_records <- function(child_domain,
                                               parent_domain,
                                               parent_keys,
                                               owner_team = "DM") {
  new_rule(
    id = "XDOM-007",
    title = "Orphan records in child domain",
    topology = "A3",
    scope = "record",
    intent = "core_integrity",
    domains = c(child_domain, parent_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      child <- .core50_domain_df(domains, child_domain)
      parent <- .core50_domain_df(domains, parent_domain)
      keys <- intersect(parent_keys, intersect(names(child), names(parent)))
      if (is.null(child) || is.null(parent) || !length(keys)) {
        return(data.frame())
      }

      bad <- .core50_anti_join_rows(child, parent, by = keys)
      if (!nrow(bad)) {
        return(data.frame())
      }

      out <- bad[, intersect(c(keys, "USUBJID"), names(bad)), drop = FALSE]
      out$reason <- "Child record has no matching parent"
      out
    }
  )
}
