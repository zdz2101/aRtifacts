rule_r047_seq_gaps <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", seq_vars = character(), owner_team = "Standards", ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  if (length(seq_vars)) {
    new_flag_rule(
      id         = "R047_SEQ_GAPS",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        if (!(subject_var %in% names(df))) return(data.frame())
        out <- list(); k <- 0L
        for (sv in seq_vars) {
          if (!(sv %in% names(df))) next
          grps <- split(seq_len(nrow(df)), df[[subject_var]])
          for (subj in names(grps)) {
            idx  <- grps[[subj]]
            seqs <- suppressWarnings(as.integer(df[[sv]][idx]))
            seqs <- sort(seqs[!is.na(seqs)])
            if (length(seqs) < 2L) next
            # duplicate check
            has_dup <- anyDuplicated(seqs) > 0L
            # gap check: expected to be consecutive integers
            has_gap <- any(diff(seqs) > 1L)
            if (!has_dup && !has_gap) next
            k <- k + 1L
            piece <- df[idx, .ctx(df, sv), drop = FALSE]
            piece$SEQ_VAR <- sv
            piece$ISSUE   <- if (has_dup) "Duplicate sequence" else "Gap in sequence"
            out[[k]] <- piece
          }
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }
}
