# ---------------------------------------------------------------------------
# Module 4 — Plausibility & Range Checks (R031–R039)
#
# R032 (unit mismatch heuristic) requires a controlled terminology reference
# mapping units to parameters — outside the scope of a single-domain rule
# pack.
# R036 (site-level distribution shift) requires a historical baseline
# distribution and is Snapshot-scope across two data cuts.
# Both are noted in the catalog but not implemented here.
# ---------------------------------------------------------------------------

#' Rule pack — plausibility and range checks
#'
#' Builds executable `FlagRule` objects for the plausibility and value-range
#' checks from the core observability rule catalog.
#'
#' | Rule ID | Name | Scope |
#' |---|---|---|
#' | R031 | Physiologically impossible values | Record |
#' | R033 | Negative values where not allowed | Record |
#' | R034 | Suspicious zero values | Group |
#' | R035 | Extreme statistical outliers (IQR) | Record |
#' | R037 | Within-subject implausible change | Group |
#' | R038 | Too-perfect data (low variance / CV) | Group |
#' | R039 | Rounding pattern anomaly | Group |
#'
#' @param domain Domain name targeted by the rules.
#' @param key_vars Character vector of key variables.
#' @param subject_var Subject identifier variable. Default `"USUBJID"`.
#' @param site_var Site identifier variable. Default `"SITEID"`.
#' @param numeric_vars Character vector of numeric variables to include in
#'   the statistical checks (R034, R035, R037, R038, R039).
#' @param non_negative_vars Variables that must not contain negative values
#'   (R033).
#' @param value_ranges Named list of two-element numeric vectors specifying
#'   hard `c(min, max)` bounds for R031 (e.g.
#'   `list(AGE = c(0, 120), WEIGHT = c(1, 600))`).
#' @param visit_order_var Numeric variable used to order records within a
#'   subject for R037 (e.g. `"ADY"` or `"VISITNUM"`).
#' @param max_within_change Named list of maximum permitted consecutive
#'   change per numeric variable for R037 (e.g. `list(AVAL = 100)`).
#'   Variables absent from this list are skipped.
#' @param iqr_multiplier Multiplier applied to the IQR to define the outer
#'   fence for R035. Default `3.0` (Tukey far-out rule).
#' @param zero_rate_threshold Fraction of non-missing values that are zero
#'   above which R034 fires. Default `0.5`.
#' @param low_cv_threshold Coefficient of variation below which R038 flags
#'   suspiciously low variance. Default `0.01`.
#' @param round_rate_threshold Fraction of values divisible by 5 above
#'   which R039 fires. Default `0.9`.
#' @param owner_team Team assigned to follow up on flagged records.
#'
#' @return A list of `FlagRule` objects compatible with `run_flags()`.
#' @export
#'
#' @examples
#' pack <- make_rule_pack_plausibility(
#'   domain           = "ADVS",
#'   key_vars         = c("USUBJID", "PARAMCD", "AVISIT"),
#'   numeric_vars     = "AVAL",
#'   non_negative_vars = "AVAL",
#'   value_ranges     = list(AVAL = c(40, 300))
#' )
#' length(pack)
make_rule_pack_plausibility <- function(domain,
                                        key_vars,
                                        subject_var        = "USUBJID",
                                        site_var           = "SITEID",
                                        numeric_vars       = character(),
                                        non_negative_vars  = character(),
                                        value_ranges       = list(),
                                        visit_order_var    = NULL,
                                        max_within_change  = list(),
                                        iqr_multiplier     = 3.0,
                                        zero_rate_threshold = 0.5,
                                        low_cv_threshold   = 0.01,
                                        round_rate_threshold = 0.9,
                                        owner_team         = "Biostats",
                                        ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  rules <- list()

  # R031 — Physiologically impossible values ----------------------------------
  if (length(value_ranges)) {
    rules[["R031"]] <- new_flag_rule(
      id         = "R031_IMPOSSIBLE_VALUES",
      domain     = domain,
      severity   = "error",
      category   = "clinical_review",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (v in names(value_ranges)) {
          if (!(v %in% names(df))) next
          bounds <- value_ranges[[v]]
          vals   <- suppressWarnings(as.numeric(df[[v]]))
          bad    <- !is.na(vals) & (vals < bounds[1] | vals > bounds[2])
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .ctx(df, v), drop = FALSE]
          piece$VARIABLE  <- v
          piece$VALUE     <- vals[bad]
          piece$RANGE_MIN <- bounds[1]
          piece$RANGE_MAX <- bounds[2]
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R033 — Negative values where not allowed -----------------------------------
  if (length(non_negative_vars)) {
    rules[["R033"]] <- new_flag_rule(
      id         = "R033_NEGATIVE_NOT_ALLOWED",
      domain     = domain,
      severity   = "warning",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (v in non_negative_vars) {
          if (!(v %in% names(df))) next
          vals <- suppressWarnings(as.numeric(df[[v]]))
          bad  <- !is.na(vals) & vals < 0
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .ctx(df, v), drop = FALSE]
          piece$VARIABLE <- v
          piece$VALUE    <- vals[bad]
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R034 — Suspicious zero values ----------------------------------------------
  if (length(numeric_vars)) {
    rules[["R034"]] <- new_flag_rule(
      id         = "R034_SUSPICIOUS_ZEROS",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (v in numeric_vars) {
          if (!(v %in% names(df))) next
          vals     <- suppressWarnings(as.numeric(df[[v]]))
          nonmiss  <- !is.na(vals)
          if (!any(nonmiss)) next
          zero_rate <- mean(vals[nonmiss] == 0, na.rm = TRUE)
          if (zero_rate <= zero_rate_threshold) next
          k <- k + 1L
          out[[k]] <- data.frame(
            VARIABLE   = v,
            ZERO_RATE  = zero_rate,
            N_ZERO     = sum(vals == 0, na.rm = TRUE),
            N_NONMISS  = sum(nonmiss),
            stringsAsFactors = FALSE
          )
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R035 — Extreme statistical outliers (IQR) ---------------------------------
  if (length(numeric_vars)) {
    rules[["R035"]] <- new_flag_rule(
      id         = "R035_STATISTICAL_OUTLIERS",
      domain     = domain,
      severity   = "warning",
      category   = "clinical_review",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (v in numeric_vars) {
          if (!(v %in% names(df))) next
          vals <- suppressWarnings(as.numeric(df[[v]]))
          nonmiss <- !is.na(vals)
          if (sum(nonmiss) < 4L) next
          q1  <- stats::quantile(vals[nonmiss], 0.25)
          q3  <- stats::quantile(vals[nonmiss], 0.75)
          iqr <- q3 - q1
          if (iqr == 0) next
          lo  <- q1 - iqr_multiplier * iqr
          hi  <- q3 + iqr_multiplier * iqr
          bad <- nonmiss & (vals < lo | vals > hi)
          if (!any(bad)) next
          k <- k + 1L
          piece <- df[bad, .ctx(df, v), drop = FALSE]
          piece$VARIABLE <- v
          piece$VALUE    <- vals[bad]
          piece$FENCE_LO <- lo
          piece$FENCE_HI <- hi
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R037 — Within-subject implausible change ----------------------------------
  if (!is.null(visit_order_var) && length(max_within_change)) {
    rules[["R037"]] <- new_flag_rule(
      id         = "R037_IMPLAUSIBLE_CHANGE",
      domain     = domain,
      severity   = "warning",
      category   = "clinical_review",
      owner_team = owner_team,
      fn = function(df) {
        if (!(subject_var %in% names(df) && visit_order_var %in% names(df))) {
          return(data.frame())
        }
        ord    <- order(df[[subject_var]], df[[visit_order_var]], na.last = TRUE)
        sorted <- df[ord, , drop = FALSE]
        out <- list(); k <- 0L
        for (v in names(max_within_change)) {
          if (!(v %in% names(sorted))) next
          threshold <- max_within_change[[v]]
          grps      <- split(seq_len(nrow(sorted)), sorted[[subject_var]])
          bad_idx   <- integer(0)
          for (idx in grps) {
            vals  <- suppressWarnings(as.numeric(sorted[[v]][idx]))
            chg   <- abs(diff(vals))
            flags <- which(!is.na(chg) & chg > threshold)
            if (length(flags)) bad_idx <- c(bad_idx, idx[flags + 1L])
          }
          if (!length(bad_idx)) next
          k <- k + 1L
          piece <- sorted[bad_idx, .ctx(sorted, c(v, visit_order_var)), drop = FALSE]
          piece$VARIABLE  <- v
          piece$VALUE     <- suppressWarnings(as.numeric(sorted[[v]][bad_idx]))
          piece$THRESHOLD <- threshold
          out[[k]] <- piece
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R038 — Too-perfect data (low variance / CV) --------------------------------
  if (length(numeric_vars)) {
    rules[["R038"]] <- new_flag_rule(
      id         = "R038_LOW_VARIANCE",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (v in numeric_vars) {
          if (!(v %in% names(df))) next
          vals    <- suppressWarnings(as.numeric(df[[v]]))
          nonmiss <- vals[!is.na(vals)]
          if (length(nonmiss) < 2L) next
          mn  <- mean(nonmiss)
          cv  <- if (mn != 0) stats::sd(nonmiss) / abs(mn) else stats::sd(nonmiss)
          if (cv > low_cv_threshold) next
          k <- k + 1L
          out[[k]] <- data.frame(
            VARIABLE  = v,
            CV        = cv,
            MEAN      = mn,
            SD        = stats::sd(nonmiss),
            N         = length(nonmiss),
            stringsAsFactors = FALSE
          )
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  # R039 — Rounding pattern anomaly ------------------------------------------
  if (length(numeric_vars)) {
    rules[["R039"]] <- new_flag_rule(
      id         = "R039_ROUNDING_ANOMALY",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        out <- list(); k <- 0L
        for (v in numeric_vars) {
          if (!(v %in% names(df))) next
          vals    <- suppressWarnings(as.numeric(df[[v]]))
          nonmiss <- vals[!is.na(vals)]
          if (length(nonmiss) < 5L) next
          round_rate <- mean(nonmiss %% 5 == 0, na.rm = TRUE)
          if (round_rate <= round_rate_threshold) next
          k <- k + 1L
          out[[k]] <- data.frame(
            VARIABLE   = v,
            ROUND_RATE = round_rate,
            N          = length(nonmiss),
            stringsAsFactors = FALSE
          )
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    )
  }

  rules
}
