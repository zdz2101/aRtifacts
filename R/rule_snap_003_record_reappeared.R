#' SNAP-003: Record reappeared
#'
#' Flags keys in current data that are present in historical removed-key ledger.
#'
#' @param domain Domain name.
#' @param keys Identity key columns.
#' @param removed_keys_option Context option name carrying removed keys.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_snap_003_record_reappeared <- function(domain = "ADSL",
                                            keys = c("USUBJID"),
                                            removed_keys_option = "removed_keys_history",
                                            owner_team = "Clinical Ops") {
  new_rule(
    id = "SNAP-003",
    title = "Record reappeared",
    topology = "A5",
    scope = "record",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    identity_spec = new_identity_spec(keys = keys, method = "exact"),
    fn = function(previous, current, context, rule) {
      key_frame <- .core50_key_frame(current, keys)
      if (!nrow(key_frame)) {
        return(data.frame())
      }

      ledger <- context@options[[removed_keys_option]]
      if (is.null(ledger)) {
        return(data.frame())
      }

      if (is.vector(ledger) && is.character(ledger)) {
        ledger_ids <- ledger
      } else if (is.data.frame(ledger) && .core50_has_columns(ledger, keys)) {
        ledger_ids <- .key_id(unique(ledger[, keys, drop = FALSE]), keys)
      } else {
        return(data.frame())
      }

      cur_ids <- .key_id(key_frame, keys)
      bad <- cur_ids %in% ledger_ids
      if (!any(bad)) {
        return(data.frame())
      }

      out <- key_frame[bad, keys, drop = FALSE]
      out$reason <- "Previously removed key appears again"
      out
    }
  )
}
