rule_r043_placeholder_text <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", placeholder_patterns = NULL, owner_team = "Standards", ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  if (is.null(placeholder_patterns)) {
    placeholder_patterns <- c(
      "^N/?A$", "^TBD$", "^TBC$", "^UNK(NOWN)?$", "^MISSING$",
      "^NONE$", "^NA$", "^\\.$", "^-$", "^999+$", "^9{3,}$",
      "^\\*+$", "^TEST$", "^TEMP$", "^PLACEHOLDER$"
    )
  }

  new_flag_rule(
    id         = "R043_PLACEHOLDER_TEXT",
    domain     = domain,
    severity   = "warning",
    category   = "ops_anomaly",
    owner_team = owner_team,
    fn = function(df) {
      char_vars <- names(df)[vapply(df, is.character, logical(1))]
      out <- list(); k <- 0L
      for (v in char_vars) {
        vals    <- df[[v]]
        present <- !is.na(vals) & nzchar(vals)
        bad     <- present & vapply(
          toupper(trimws(vals)),
          function(x) any(grepl(paste(placeholder_patterns, collapse = "|"), x, perl = TRUE)),
          logical(1)
        )
        if (!any(bad)) next
        k <- k + 1L
        piece <- df[bad, .ctx(df, v), drop = FALSE]
        piece$VARIABLE    <- v
        piece$BAD_VALUE   <- vals[bad]
        out[[k]] <- piece
      }
      if (!k) return(data.frame())
      do.call(rbind, out)
    }
  )
}
