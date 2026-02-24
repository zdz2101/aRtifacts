#' SNAP-002: Record changed unexpectedly
#'
#' @param domain Domain name.
#' @param keys Identity key columns.
#' @param monitored_fields Fields to compare. If empty, inferred from overlap minus keys.
#' @param numeric_tol Numeric absolute tolerance.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_snap_002_record_changed_unexpectedly <- function(domain = "ADSL",
                                                      keys = c("USUBJID"),
                                                      monitored_fields = character(),
                                                      numeric_tol = 0,
                                                      owner_team = "Standards") {
  new_rule(
    id = "SNAP-002",
    title = "Record changed unexpectedly",
    topology = "A5",
    scope = "record",
    intent = "programming_etl",
    domain = domain,
    owner_team = owner_team,
    identity_spec = new_identity_spec(keys = keys, method = "exact"),
    delta_spec = new_delta_spec(fields = if (length(monitored_fields)) monitored_fields else keys, abs_tol = numeric_tol),
    fn = function(previous, current, context, rule) {
      by <- intersect(keys, intersect(names(previous), names(current)))
      if (!length(by)) {
        return(data.frame())
      }

      fields <- monitored_fields
      if (!length(fields)) {
        fields <- setdiff(intersect(names(previous), names(current)), by)
      }
      if (!length(fields)) {
        return(data.frame())
      }

      aligned <- merge(previous, current, by = by, suffixes = c("_prev", "_cur"), all = FALSE)
      if (!nrow(aligned)) {
        return(data.frame())
      }

      changed_mat <- matrix(FALSE, nrow = nrow(aligned), ncol = length(fields))
      colnames(changed_mat) <- fields

      for (i in seq_along(fields)) {
        fld <- fields[[i]]
        prev_col <- aligned[[paste0(fld, "_prev")]]
        cur_col <- aligned[[paste0(fld, "_cur")]]
        changed_mat[, i] <- .core50_changed_mask(prev_col, cur_col, numeric_tol = numeric_tol)
      }

      bad <- apply(changed_mat, 1, any)
      if (!any(bad)) {
        return(data.frame())
      }

      changed_fields <- apply(changed_mat[bad, , drop = FALSE], 1, function(x) {
        paste(fields[which(isTRUE(x) | (!is.na(x) & x))], collapse = ",")
      })

      out <- aligned[bad, by, drop = FALSE]
      out$changed_fields <- changed_fields
      out$reason <- "Aligned record changed beyond tolerance"
      out
    }
  )
}
