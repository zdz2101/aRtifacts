# ---------------------------------------------------------------------------
# Module 3 — Missingness & Completeness Drift (R021–R027)
#
# R028 (late-arriving data volume spike), R029 (unexpected data plateau),
# and R030 (form completion rate outlier) require cross-snapshot comparison
# or an external completion log and are not implementable in a single-domain
# rule pack.
# ---------------------------------------------------------------------------

#' Rule pack — missingness and completeness
#'
#' Builds executable `FlagRule` objects for the missingness and completeness
#' checks from the core observability rule catalog.
#'
#' | Rule ID | Name | Scope |
#' |---|---|---|
#' | R021 | Overall missingness spike | Snapshot |
#' | R022 | Missingness spike by site | Group |
#' | R023 | Missingness spike by visit/timepoint | Group |
#' | R024 | Required variable missingness > 0 | Snapshot |
#' | R025 | Expected domain empty | Snapshot |
#' | R026 | Subjects without follow-up after baseline | Group |
#' | R027 | New subjects missing baseline assessments | Group |
#'
#' @param domain Domain name targeted by the rules.
#' @param key_vars Character vector of key variables.
#' @param required_vars Character vector of variables that must be present
#'   and non-missing in every record (R024).
#' @param subject_var Subject identifier variable. Default `"USUBJID"`.
#' @param site_var Site identifier variable. Default `"SITEID"`.
#' @param visit_var Optional variable that identifies visit or timepoint
#'   (used by R023).
#' @param baseline_flag_var Variable whose value `"Y"` marks the baseline
#'   record for a subject (used by R026 and R027). Typically `"ABLFL"` or
#'   `"DTYPE"`.
#' @param missingness_threshold Numeric \[0, 1\]. Variables with a missing
#'   rate above this threshold are flagged by R021–R023. Default `0.1`.
#' @param owner_team Team assigned to follow up on flagged records.
#' @param ... Reserved for future extensions; currently ignored.
#'
#' @return A list of `FlagRule` objects compatible with `run_flags()`.
#' @export
#'
#' @examples
#' pack <- make_rule_pack_missingness(
#'   domain        = "ADVS",
#'   key_vars      = c("USUBJID", "PARAMCD", "AVISIT"),
#'   required_vars = c("USUBJID", "PARAMCD", "AVAL"),
#'   baseline_flag_var = "ABLFL"
#' )
#' length(pack)
make_rule_pack_missingness <- function(domain,
                                       key_vars,
                                       required_vars         = character(),
                                       subject_var           = "USUBJID",
                                       site_var              = "SITEID",
                                       visit_var             = NULL,
                                       baseline_flag_var     = NULL,
                                       missingness_threshold = 0.1,
                                       owner_team            = "DM",
                                       ...) {
  .ctx <- function(df, extra = character()) {
    intersect(c(key_vars, subject_var, site_var, extra), names(df))
  }

  list(

    # R021 — Overall missingness spike ----------------------------------------
    new_flag_rule(
      id         = "R021_MISSINGNESS_OVERALL",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        if (!nrow(df)) return(data.frame())
        rates <- colMeans(is.na(df))
        bad   <- rates[rates > missingness_threshold]
        if (!length(bad)) return(data.frame())
        data.frame(
          VARIABLE     = names(bad),
          MISS_RATE    = unname(bad),
          N_MISSING    = as.integer(unname(bad) * nrow(df)),
          stringsAsFactors = FALSE
        )
      }
    ),

    # R022 — Missingness spike by site ----------------------------------------
    new_flag_rule(
      id         = "R022_MISSINGNESS_BY_SITE",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        if (!nrow(df) || !(site_var %in% names(df))) return(data.frame())
        check_vars <- setdiff(names(df), c(key_vars, site_var, subject_var))
        if (!length(check_vars)) return(data.frame())
        sites <- unique(df[[site_var]])
        out <- list(); k <- 0L
        for (s in sites) {
          sub   <- df[df[[site_var]] == s, check_vars, drop = FALSE]
          if (!nrow(sub)) next
          rates <- colMeans(is.na(sub))
          bad   <- rates[rates > missingness_threshold]
          if (!length(bad)) next
          k <- k + 1L
          out[[k]] <- data.frame(
            SITE_ID   = s,
            VARIABLE  = names(bad),
            MISS_RATE = unname(bad),
            stringsAsFactors = FALSE
          )
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    ),

    # R023 — Missingness spike by visit/timepoint -----------------------------
    new_flag_rule(
      id         = "R023_MISSINGNESS_BY_VISIT",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        vv <- visit_var
        if (is.null(vv) || !(vv %in% names(df)) || !nrow(df)) return(data.frame())
        check_vars <- setdiff(names(df), c(key_vars, site_var, subject_var, vv))
        if (!length(check_vars)) return(data.frame())
        visits <- unique(df[[vv]])
        out <- list(); k <- 0L
        for (v in visits) {
          sub   <- df[!is.na(df[[vv]]) & df[[vv]] == v, check_vars, drop = FALSE]
          if (!nrow(sub)) next
          rates <- colMeans(is.na(sub))
          bad   <- rates[rates > missingness_threshold]
          if (!length(bad)) next
          k <- k + 1L
          out[[k]] <- data.frame(
            VISIT     = as.character(v),
            VARIABLE  = names(bad),
            MISS_RATE = unname(bad),
            stringsAsFactors = FALSE
          )
        }
        if (!k) return(data.frame())
        do.call(rbind, out)
      }
    ),

    # R024 — Required variable missingness > 0 --------------------------------
    new_flag_rule(
      id         = "R024_REQUIRED_MISSING",
      domain     = domain,
      severity   = "error",
      category   = "data_entry",
      owner_team = owner_team,
      fn = function(df) {
        absent <- setdiff(required_vars, names(df))
        present_with_na <- character()
        for (v in intersect(required_vars, names(df))) {
          if (any(is.na(df[[v]]))) present_with_na <- c(present_with_na, v)
        }
        issues <- c(
          if (length(absent))          paste0(absent, " (column absent)"),
          if (length(present_with_na)) paste0(present_with_na, " (NA values present)")
        )
        if (!length(issues)) return(data.frame())
        data.frame(ISSUE = issues, stringsAsFactors = FALSE)
      }
    ),

    # R025 — Expected domain empty --------------------------------------------
    new_flag_rule(
      id         = "R025_DOMAIN_EMPTY",
      domain     = domain,
      severity   = "error",
      category   = "programming_etl",
      owner_team = owner_team,
      fn = function(df) {
        if (nrow(df) > 0L) return(data.frame())
        data.frame(ISSUE = paste0("Domain '", domain, "' contains zero records."), stringsAsFactors = FALSE)
      }
    ),

    # R026 — Subjects without follow-up after baseline -----------------------
    new_flag_rule(
      id         = "R026_NO_FOLLOWUP",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        bfv <- baseline_flag_var
        if (is.null(bfv) || !(bfv %in% names(df)) || !(subject_var %in% names(df))) {
          return(data.frame())
        }
        bl_subjs    <- unique(df[[subject_var]][!is.na(df[[bfv]]) & df[[bfv]] == "Y"])
        post_subjs  <- unique(df[[subject_var]][is.na(df[[bfv]]) | df[[bfv]] != "Y"])
        no_followup <- setdiff(bl_subjs, post_subjs)
        if (!length(no_followup)) return(data.frame())
        data.frame(
          USUBJID = no_followup,
          ISSUE   = "Baseline record present but no post-baseline records found.",
          stringsAsFactors = FALSE
        )
      }
    ),

    # R027 — New subjects missing baseline assessments -----------------------
    new_flag_rule(
      id         = "R027_MISSING_BASELINE",
      domain     = domain,
      severity   = "warning",
      category   = "ops_anomaly",
      owner_team = owner_team,
      fn = function(df) {
        bfv <- baseline_flag_var
        if (is.null(bfv) || !(bfv %in% names(df)) || !(subject_var %in% names(df))) {
          return(data.frame())
        }
        all_subjs    <- unique(df[[subject_var]])
        bl_subjs     <- unique(df[[subject_var]][!is.na(df[[bfv]]) & df[[bfv]] == "Y"])
        missing_bl   <- setdiff(all_subjs, bl_subjs)
        if (!length(missing_bl)) return(data.frame())
        data.frame(
          USUBJID = missing_bl,
          ISSUE   = "No baseline record found for this subject.",
          stringsAsFactors = FALSE
        )
      }
    )
  )
}
