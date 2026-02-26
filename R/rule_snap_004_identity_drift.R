#' SNAP-004: Identity drift
#'
#' @param domain Domain name.
#' @param stable_fields Fields defining a stable event identity.
#' @param identity_fields Fields expected to remain stable for each event.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_snap_004_identity_drift <- function(domain = "PRIMARY",
                                         stable_fields = c("entity_id", "event_type", "event_date"),
                                         identity_fields = c("event_id"),
                                         owner_team = "Standards") {
  new_rule(
    id = "SNAP-004",
    title = "Identity drift",
    topology = "A5",
    scope = "record",
    intent = "programming_etl",
    domain = domain,
    owner_team = owner_team,
    fn = function(previous, current, context, rule) {
      needed <- unique(c(stable_fields, identity_fields))
      if (!.topology_has_columns(previous, needed) || !.topology_has_columns(current, needed)) {
        return(data.frame())
      }

      prev_u <- unique(previous[, needed, drop = FALSE])
      cur_u <- unique(current[, needed, drop = FALSE])
      aligned <- merge(prev_u, cur_u, by = stable_fields, suffixes = c("_prev", "_cur"), all = FALSE)
      if (!nrow(aligned)) {
        return(data.frame())
      }

      bad <- rep(FALSE, nrow(aligned))
      for (fld in identity_fields) {
        prev_col <- aligned[[paste0(fld, "_prev")]]
        cur_col <- aligned[[paste0(fld, "_cur")]]
        bad <- bad | .topology_changed_mask(prev_col, cur_col, numeric_tol = 0)
      }

      if (!any(bad)) {
        return(data.frame())
      }

      out <- aligned[bad, stable_fields, drop = FALSE]
      out$reason <- "Stable identity fields drifted across snapshots"
      out
    }
  )
}
