# ---------------------------------------------------------------------------
# Module 2 — Date & Time Consistency (R011–R018)
#
# R019 (large date gaps for scheduled visits) requires a visit schedule
# reference that is outside the scope of a single-domain rule pack.
# R020 (timezone inconsistency) requires ISO 8601 datetime strings with
# offset information — not generalizable without additional assumptions.
# Both are noted in the catalog but not implemented here.
# ---------------------------------------------------------------------------

#' Rule pack — date and time consistency
#'
#' Builds executable `FlagRule` objects for the date and time integrity
#' checks from the core observability rule catalog.
#'
#' | Rule ID | Name | Scope |
#' |---|---|---|
#' | R011 | Start date after end date | Record |
#' | R012 | Event date outside study period | Record |
#' | R013 | Invalid or partial date format | Record |
#' | R014 | End date present but start missing | Record |
#' | R015 | Time present without date | Record |
#' | R016 | Visit dates out of order within subject | Group |
#' | R017 | Identical timestamps across multiple records | Group |
#' | R018 | Future-dated events | Record |
#'
#' @param domain Domain name targeted by the rules.
#' @param key_vars Character vector of key variables.
#' @param subject_var Subject identifier variable. Default `"USUBJID"`.
#' @param site_var Site identifier variable. Default `"SITEID"`.
#' @param start_end_pairs List of two-element character vectors naming a
#'   start/end date pair, e.g. `list(c("AESTDTC", "AEENDTC"))`.
#' @param date_vars Character vector of variables expected to hold full
#'   ISO 8601 dates (`YYYY-MM-DD`).
#' @param time_date_pairs List of two-element character vectors `c(time_var,
#'   date_var)` where the time variable should not be present when the
#'   corresponding date variable is missing.
#' @param visit_order_var Optional numeric variable used to order visits
#'   within a subject (e.g. `"VISITNUM"` or `"ADY"`).
#' @param study_start Optional `Date` or ISO string. Events before this date
#'   are flagged by R012.
#' @param study_end Optional `Date` or ISO string. Events after this date are
#'   flagged by R012.
#' @param cutoff_date Date used as "today" for the future-dating check
#'   (R018). Defaults to `Sys.Date()`.
#' @param owner_team Team assigned to follow up on flagged records.
#' @param ... Reserved for future extensions; currently ignored.
#'
#' @return A list of `FlagRule` objects compatible with `run_flags()`.
#' @export
#'
#' @examples
#' pack <- make_rule_pack_dates(
#'   domain          = "LB",
#'   key_vars        = c("STUDYID", "USUBJID", "LBSEQ"),
#'   date_vars       = "LBDTC",
#'   visit_order_var = "VISITNUM"
#' )
#' length(pack)
make_rule_pack_dates <- function(domain,
                                 key_vars,
                                 subject_var     = "USUBJID",
                                 site_var        = "SITEID",
                                 start_end_pairs = list(),
                                 date_vars       = character(),
                                 time_date_pairs = list(),
                                 visit_order_var = NULL,
                                 study_start     = NULL,
                                 study_end       = NULL,
                                 cutoff_date     = Sys.Date(),
                                 owner_team      = "DM",
                                 ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  rules <- list()

  # R011 — Start date after end date ----------------------------------------
  if (length(start_end_pairs)) {
    rules[["R011"]] <- new_flag_rule(
      id         = "R011_START_AFTER_END",
      domain     = domain,
      severity   = "error",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (pair in start_end_pairs) {
          if (length(pair) != 2) next
          s <- pair[[1]]; e <- pair[[2]]
          if (!(s %in% names(df) && e %in% names(df))) next
          sd <- suppressWarnings(as.Date(df[[s]]))
          ed <- suppressWarnings(as.Date(df[[e]]))
          bad <- !is.na(sd) & !is.na(ed) & sd > ed
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .ctx(df, c(s, e)), drop = FALSE]
          piece$START_VAR <- s; piece$END_VAR <- e
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R012 — Event date outside study period ----------------------------------
  if (!is.null(study_start) || !is.null(study_end)) {
    s_start <- if (!is.null(study_start)) as.Date(study_start) else NULL
    s_end   <- if (!is.null(study_end))   as.Date(study_end)   else NULL
    rules[["R012"]] <- new_flag_rule(
      id         = "R012_DATE_OUTSIDE_STUDY",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (dv in date_vars) {
          if (!(dv %in% names(df))) next
          d <- suppressWarnings(as.Date(df[[dv]]))
          bad <- rep(FALSE, length(d))
          if (!is.null(s_start)) bad <- bad | (!is.na(d) & d < s_start)
          if (!is.null(s_end))   bad <- bad | (!is.na(d) & d > s_end)
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .ctx(df, dv), drop = FALSE]
          piece$DATE_VAR <- dv
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R013 — Invalid or partial date format -----------------------------------
  if (length(date_vars)) {
    rules[["R013"]] <- new_flag_rule(
      id         = "R013_DATE_FORMAT",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (dv in date_vars) {
          if (!(dv %in% names(df))) next
          vals    <- as.character(df[[dv]])
          present <- !is.na(vals) & nzchar(vals)
          bad     <- present & !grepl("^\\d{4}-\\d{2}-\\d{2}", vals)
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .ctx(df, dv), drop = FALSE]
          piece$DATE_VAR   <- dv
          piece$BAD_VALUE  <- vals[bad]
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R014 — End date present but start missing --------------------------------
  if (length(start_end_pairs)) {
    rules[["R014"]] <- new_flag_rule(
      id         = "R014_END_WITHOUT_START",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (pair in start_end_pairs) {
          if (length(pair) != 2) next
          s <- pair[[1]]; e <- pair[[2]]
          if (!(s %in% names(df) && e %in% names(df))) next
          sd  <- suppressWarnings(as.Date(df[[s]]))
          ed  <- suppressWarnings(as.Date(df[[e]]))
          bad <- is.na(sd) & !is.na(ed)
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .ctx(df, c(s, e)), drop = FALSE]
          piece$START_VAR <- s; piece$END_VAR <- e
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R015 — Time present without date -----------------------------------------
  if (length(time_date_pairs)) {
    rules[["R015"]] <- new_flag_rule(
      id         = "R015_TIME_WITHOUT_DATE",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (pair in time_date_pairs) {
          if (length(pair) != 2) next
          tv <- pair[[1]]; dv <- pair[[2]]
          if (!(tv %in% names(df) && dv %in% names(df))) next
          t_present <- !is.na(df[[tv]]) & nzchar(as.character(df[[tv]]))
          d_missing <- is.na(df[[dv]])  | !nzchar(as.character(df[[dv]]))
          bad <- t_present & d_missing
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .ctx(df, c(tv, dv)), drop = FALSE]
          piece$TIME_VAR <- tv; piece$DATE_VAR <- dv
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R016 — Visit dates out of order within subject ---------------------------
  if (!is.null(visit_order_var)) {
    rules[["R016"]] <- new_flag_rule(
      id         = "R016_VISIT_ORDER",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        if (!(subject_var %in% names(df) && visit_order_var %in% names(df))) {
          return(data.frame())
        }
        ord    <- order(df[[subject_var]], df[[visit_order_var]], na.last = TRUE)
        sorted <- df[ord, , drop = FALSE]
        grps   <- split(seq_len(nrow(sorted)), sorted[[subject_var]])
        bad_idx <- integer(0)
        for (idx in grps) {
          visits <- sorted[[visit_order_var]][idx]
          if (!is.numeric(visits)) next
          diffs <- diff(visits)
          bad_positions <- which(diffs < 0)
          if (length(bad_positions)) {
            bad_idx <- c(bad_idx, idx[bad_positions + 1L])
          }
        }
        if (!length(bad_idx)) return(data.frame())
        sorted[bad_idx, .ctx(sorted, visit_order_var), drop = FALSE]
      }
    )
  }

  # R017 — Identical timestamps across multiple records ----------------------
  if (length(date_vars)) {
    rules[["R017"]] <- new_flag_rule(
      id         = "R017_IDENTICAL_TIMESTAMPS",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (dv in date_vars) {
          if (!(dv %in% names(df))) next
          dup <- duplicated(df[[dv]]) | duplicated(df[[dv]], fromLast = TRUE)
          dup <- dup & !is.na(df[[dv]])
          if (!any(dup)) next
          k <- k + 1L
          piece <- df[dup, .ctx(df, dv), drop = FALSE]
          piece$DATE_VAR <- dv
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R018 — Future-dated events -----------------------------------------------
  if (length(date_vars)) {
    rules[["R018"]] <- new_flag_rule(
      id         = "R018_FUTURE_DATED",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        cutoff <- as.Date(cutoff_date)
        for (dv in date_vars) {
          if (!(dv %in% names(df))) next
          d   <- suppressWarnings(as.Date(df[[dv]]))
          bad <- !is.na(d) & d > cutoff
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .ctx(df, dv), drop = FALSE]
          piece$DATE_VAR   <- dv
          piece$DATE_VALUE <- as.character(d[bad])
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  rules
}
