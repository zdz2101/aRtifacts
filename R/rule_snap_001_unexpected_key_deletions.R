#' SNAP-001: Unexpected key deletions
#'
#' @param domain Domain name.
#' @param keys Identity key columns.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_snap_001_unexpected_key_deletions <- function(domain = "ADSL",
                                                   keys = c("USUBJID"),
                                                   owner_team = "Standards") {
  new_rule(
    id = "SNAP-001",
    title = "Unexpected key deletions",
    topology = "A5",
    scope = "record",
    intent = "programming_etl",
    domain = domain,
    owner_team = owner_team,
    identity_spec = new_identity_spec(keys = keys, method = "exact"),
    fn = function(previous, current, context, rule) {
      by <- intersect(keys, intersect(names(previous), names(current)))
      if (!length(by)) {
        return(data.frame())
      }

      removed <- .core50_anti_join_rows(previous, current, by = by)
      if (!nrow(removed)) {
        return(data.frame())
      }

      out <- removed[, by, drop = FALSE]
      out$reason <- "Removed since last cut"
      out
    }
  )
}
