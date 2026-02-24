#' Rule R002 — duplicate keys
#'
#' Builds a single-snapshot `FlagRule` that flags rows participating in
#' duplicated key combinations.
#'
#' @inheritParams make_rule_pack_keys
#' @return A `FlagRule` object.
rule_r002_duplicate_keys <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", owner_team = "DM", ...) {
new_flag_rule(
      id         = "R002_DUPLICATE_KEYS",
      domain     = domain,
      severity   = "critical",
      category   = "programming_etl",
      owner_team = owner_team,
      fn = function(df) {
        present <- intersect(key_vars, names(df))
        if (!length(present)) return(data.frame())
        dup <- duplicated(df[, present, drop = FALSE]) |
          duplicated(df[, present, drop = FALSE], fromLast = TRUE)
        if (!any(dup)) return(data.frame())
        df[dup, .select_context_columns(df, key_vars, subject_var, group_var), drop = FALSE]
      }
    )
}
