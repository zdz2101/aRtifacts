# ---------------------------------------------------------------------------
# Module 5 — Cross-Field & Relational Consistency (R040–R048)
#
# R042 (derived flag inconsistent with source) requires user-supplied
# validation logic that is too domain-specific to generalize here — callers
# should implement it directly as a custom FlagRule.
# R044 (controlled terminology drift) requires an external CT reference
# table and is not implementable without it.
# R046 (child record without parent) requires cross-domain data that is not
# available inside a single-domain rule pack.
# R049 (enrollment vs assessment mismatch) and R050 (record deletions) are
# Snapshot-scope and belong in the diff engine.
# All are noted in the catalog but not implemented here.
# ---------------------------------------------------------------------------

#' Rule pack — cross-field and relational consistency
#'
#' Builds executable `FlagRule` objects for the cross-field and relational
#' consistency checks from the core observability rule catalog.
#'
#' | Rule ID | Name | Scope |
#' |---|---|---|
#' | R040 | Impossible categorical combinations | Record |
#' | R041 | Inconsistent categorical pairs | Record |
#' | R043 | Placeholder text prevalence spike | Record |
#' | R045 | Unexpected explosion in distinct categorical values | Snapshot |
#' | R047 | Sequence gaps or duplicate sequence numbers | Group |
#' | R048 | Record count per subject outlier | Group |
#'
#' @param domain Domain name targeted by the rules.
#' @param key_vars Character vector of key variables.
#' @param subject_var Subject identifier variable. Default `"USUBJID"`.
#' @param site_var Site identifier variable. Default `"SITEID"`.
#' @param impossible_combos List of named character vectors that define
#'   forbidden combinations. Each element should be a named vector where
#'   names are column names and values are the forbidden values that must
#'   not appear together, e.g.
#'   `list(c(SEX = "M", PREGNANT = "Y"))`.
#' @param inconsistent_pairs List of named character vectors specifying
#'   values that are mutually exclusive within the same record, e.g.
#'   `list(c(AESER = "Y", AESDTH = "N", AESLIFE = "N", AESHOSP = "N",
#'            AESDISAB = "N", AESCONG = "N", AESMIE = "N"))`.
#'   A record is flagged when *all* listed values match simultaneously.
#' @param categorical_vars Character variables to inspect for unusual
#'   cardinality (R045).
#' @param seq_vars Character vector of integer sequence variables to check
#'   for gaps or duplicates within each subject (R047).
#' @param placeholder_patterns Character vector of regex patterns that
#'   identify placeholder text (e.g. `"^N/A$"`, `"^TBD$"`). Defaults to a
#'   built-in set covering common data-entry placeholders.
#' @param max_distinct_values Maximum number of distinct values allowed per
#'   categorical variable before R045 fires. Default `50L`.
#' @param record_count_iqr_multiplier IQR multiplier for the per-subject
#'   record count outlier check (R048). Default `3.0`.
#' @param owner_team Team assigned to follow up on flagged records.
#'
#' @return A list of `FlagRule` objects compatible with `run_flags()`.
#' @export
#'
#' @examples
#' pack <- make_rule_pack_consistency(
#'   domain            = "ADVS",
#'   key_vars          = c("USUBJID", "PARAMCD", "AVISIT"),
#'   categorical_vars  = c("PARAMCD", "AVISIT"),
#'   seq_vars          = character()
#' )
#' length(pack)
make_rule_pack_consistency <- function(domain,
                                       key_vars,
                                       subject_var              = "USUBJID",
                                       site_var                 = "SITEID",
                                       impossible_combos        = list(),
                                       inconsistent_pairs       = list(),
                                       categorical_vars         = character(),
                                       seq_vars                 = character(),
                                       placeholder_patterns     = NULL,
                                       max_distinct_values      = 50L,
                                       record_count_iqr_multiplier = 3.0,
                                       owner_team               = "Standards",
                                       ...) {
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

  rules <- list()

  # R040 — Impossible categorical combinations --------------------------------
  if (length(impossible_combos)) {
    rules[["R040"]] <- new_flag_rule(
      id         = "R040_IMPOSSIBLE_COMBOS",
      domain     = domain,
      severity   = "error",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (combo in impossible_combos) {
          vars <- names(combo)
          if (!all(vars %in% names(df))) next
          match_vec <- vapply(vars, function(v) df[[v]] == combo[[v]], logical(nrow(df)))
          if (is.vector(match_vec)) {
            bad <- match_vec
          } else {
            bad <- rowSums(match_vec) == length(vars)
          }
          if (!any(bad, na.rm = TRUE)) next
          k <- k + 1L
          piece <- df[which(bad), .ctx(df, vars), drop = FALSE]
          piece$COMBO <- paste(paste(vars, combo, sep = "="), collapse = " & ")
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R041 — Inconsistent categorical pairs ------------------------------------
  if (length(inconsistent_pairs)) {
    rules[["R041"]] <- new_flag_rule(
      id         = "R041_INCONSISTENT_PAIRS",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (pair in inconsistent_pairs) {
          vars <- names(pair)
          if (!all(vars %in% names(df))) next
          match_vec <- vapply(vars, function(v) !is.na(df[[v]]) & df[[v]] == pair[[v]], logical(nrow(df)))
          if (is.vector(match_vec)) {
            bad <- match_vec
          } else {
            bad <- rowSums(match_vec) == length(vars)
          }
          if (!any(bad, na.rm = TRUE)) next
          k <- k + 1L
          piece <- df[which(bad), .ctx(df, vars), drop = FALSE]
          piece$INCONSISTENCY <- paste(paste(vars, pair, sep = "="), collapse = " & ")
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R043 — Placeholder text prevalence spike ---------------------------------
  rules[["R043"]] <- new_flag_rule(
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

  # R045 — Unexpected explosion in distinct categorical values ---------------
  if (length(categorical_vars)) {
    rules[["R045"]] <- new_flag_rule(
      id         = "R045_DISTINCT_VALUES_EXPLOSION",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (v in categorical_vars) {
          if (!(v %in% names(df))) next
          n_distinct <- length(unique(df[[v]][!is.na(df[[v]])]))
          if (n_distinct <= max_distinct_values) next
          k <- k + 1L
          out[[k]] <- data.frame(
            VARIABLE   = v,
            N_DISTINCT = n_distinct,
            THRESHOLD  = max_distinct_values,
            stringsAsFactors = FALSE
          )
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R047 — Sequence gaps or duplicate sequence numbers ----------------------
  if (length(seq_vars)) {
    rules[["R047"]] <- new_flag_rule(
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

  # R048 — Record count per subject outlier ----------------------------------
  rules[["R048"]] <- new_flag_rule(
    id         = "R048_RECORD_COUNT_OUTLIER",
    domain     = domain,
    severity   = "warning",
    category   = "ops_anomaly",
    owner_team = owner_team,
    fn = function(df) {
      if (!(subject_var %in% names(df))) return(data.frame())
      counts    <- table(df[[subject_var]])
      count_vec <- as.integer(counts)
      if (length(count_vec) < 4L) return(data.frame())
      q1  <- stats::quantile(count_vec, 0.25)
      q3  <- stats::quantile(count_vec, 0.75)
      iqr <- q3 - q1
      if (iqr == 0) return(data.frame())
      hi  <- q3 + record_count_iqr_multiplier * iqr
      bad_subjs <- names(counts)[count_vec > hi]
      if (!length(bad_subjs)) return(data.frame())
      data.frame(
        USUBJID      = bad_subjs,
        RECORD_COUNT = as.integer(counts[bad_subjs]),
        FENCE_HI     = hi,
        stringsAsFactors = FALSE
      )
    }
  )

  rules
}
