#' Run rule flags on an artifact snapshot
#'
#' Applies each `FlagRule` to its target domain and combines non-empty outputs.
#'
#' @param artifact An `ArtifactSnapshot`.
#' @param rules List of `FlagRule` objects.
#'
#' @return A `data.frame` of flagged rows augmented with rule metadata columns
#'   (`RULE_ID`, `DOMAIN`, `SEVERITY`, `CATEGORY`, `OWNER_TEAM`).
#' @export
#'
#' @examples
#' dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = example_advs(),
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' art <- new_artifact_snapshot(domains = list(ADVS = dom))
#' rule <- new_flag_rule(
#'   id = "HI_SYSBP",
#'   domain = "ADVS",
#'   fn = function(df) {
#'     out <- df[df$PARAMCD == "SYSBP" & !is.na(df$AVAL) & df$AVAL > 145, , drop = FALSE]
#'     if (!nrow(out)) return(data.frame())
#'     out[, c("USUBJID", "PARAMCD", "AVAL"), drop = FALSE]
#'   }
#' )
#' run_flags(art, list(rule))
run_flags <- function(artifact, rules) {
  if (!S7::S7_inherits(artifact, ArtifactSnapshot)) {
    stop("artifact must be an ArtifactSnapshot.", call. = FALSE)
  }
  if (!is.list(rules) || !length(rules)) return(data.frame())

  rows <- list()
  k <- 0L

  for (rule in rules) {
    if (!S7::S7_inherits(rule, FlagRule)) {
      stop("All `rules` entries must be FlagRule objects.", call. = FALSE)
    }

    if (!(rule@domain %in% names(artifact@domains))) {
      next
    }

    dom <- artifact@domains[[rule@domain]]
    result <- rule@fn(dom@data)
    if (is.null(result) || !nrow(result)) {
      next
    }

    if (!is.data.frame(result)) {
      stop("Rule `", rule@id, "` must return a data.frame.", call. = FALSE)
    }

    result$RULE_ID <- rule@id
    result$DOMAIN <- rule@domain
    result$SEVERITY <- rule@severity
    result$CATEGORY <- rule@category
    result$OWNER_TEAM <- rule@owner_team

    k <- k + 1L
    rows[[k]] <- result
  }

  if (!k) return(data.frame())
  do.call(rbind, rows)
}

#' Run diff rules between two artifact snapshots
#'
#' Applies each `DiffFlagRule` to matching domains in an old/new artifact pair
#' and combines non-empty outputs.
#'
#' @param old_artifact Previous `ArtifactSnapshot`.
#' @param new_artifact Current `ArtifactSnapshot`.
#' @param rules List of `DiffFlagRule` objects.
#'
#' @return A `data.frame` of findings augmented with metadata columns
#'   (`RULE_ID`, `DOMAIN`, `SEVERITY`, `CATEGORY`, `OWNER_TEAM`).
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = example_advs(),
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' new_data <- example_advs()
#' new_data$AVAL <- new_data$AVAL + 1
#' new_dom <- new_domain_snapshot(
#'   name = "ADVS",
#'   data = new_data,
#'   key_vars = c("USUBJID", "PARAMCD", "AVISIT")
#' )
#' old_art <- new_artifact_snapshot(domains = list(ADVS = old_dom))
#' new_art <- new_artifact_snapshot(domains = list(ADVS = new_dom))
#' rule <- new_diff_flag_rule(
#'   id = "SHIFT",
#'   domain = "ADVS",
#'   fn = function(old_dom, new_dom) compare_record_volume(old_dom, new_dom)
#' )
#' run_diff_flags(old_art, new_art, list(rule))
run_diff_flags <- function(old_artifact, new_artifact, rules) {
  if (!S7::S7_inherits(old_artifact, ArtifactSnapshot)) {
    stop("old_artifact must be an ArtifactSnapshot.", call. = FALSE)
  }
  if (!S7::S7_inherits(new_artifact, ArtifactSnapshot)) {
    stop("new_artifact must be an ArtifactSnapshot.", call. = FALSE)
  }
  if (!is.list(rules) || !length(rules)) return(data.frame())

  rows <- list()
  k <- 0L

  for (rule in rules) {
    if (!S7::S7_inherits(rule, DiffFlagRule)) {
      stop("All `rules` entries must be DiffFlagRule objects.", call. = FALSE)
    }

    if (!(rule@domain %in% names(old_artifact@domains) && rule@domain %in% names(new_artifact@domains))) {
      next
    }

    old_dom <- old_artifact@domains[[rule@domain]]
    new_dom <- new_artifact@domains[[rule@domain]]
    result <- rule@fn(old_dom, new_dom)

    if (is.null(result) || !nrow(result)) {
      next
    }

    if (!is.data.frame(result)) {
      stop("Rule `", rule@id, "` must return a data.frame.", call. = FALSE)
    }

    result$RULE_ID <- rule@id
    result$DOMAIN <- rule@domain
    result$SEVERITY <- rule@severity
    result$CATEGORY <- rule@category
    result$OWNER_TEAM <- rule@owner_team

    k <- k + 1L
    rows[[k]] <- result
  }

  if (!k) return(data.frame())
  do.call(rbind, rows)
}

#' Diff longitudinal flags
#'
#' Compares old and new flag tables and classifies records as introduced,
#' resolved, or persisting.
#'
#' @param old_flags Previous flags `data.frame`.
#' @param new_flags Current flags `data.frame`.
#' @param by Key columns used to identify the same flag record across cuts.
#'
#' @return A list with `introduced`, `resolved`, and `persisting`
#'   `data.frame` elements.
#' @export
#'
#' @examples
#' old_flags <- data.frame(
#'   USUBJID = c("A", "B"), DOMAIN = c("ADVS", "ADVS"), RULE_ID = c("R1", "R1")
#' )
#' new_flags <- data.frame(
#'   USUBJID = c("B", "C"), DOMAIN = c("ADVS", "ADVS"), RULE_ID = c("R1", "R1")
#' )
#' diff_flags(old_flags, new_flags)
diff_flags <- function(old_flags, new_flags, by = c("USUBJID", "DOMAIN", "RULE_ID")) {
  if (!is.data.frame(old_flags)) old_flags <- data.frame()
  if (!is.data.frame(new_flags)) new_flags <- data.frame()

  shared_by <- intersect(by, intersect(names(old_flags), names(new_flags)))
  if (!length(shared_by)) {
    return(list(introduced = new_flags, resolved = old_flags, persisting = data.frame()))
  }

  old_id <- .key_id(old_flags, shared_by)
  new_id <- .key_id(new_flags, shared_by)

  introduced <- new_flags[!(new_id %in% old_id), , drop = FALSE]
  resolved <- old_flags[!(old_id %in% new_id), , drop = FALSE]
  persisting <- new_flags[new_id %in% old_id, , drop = FALSE]

  list(
    introduced = introduced,
    resolved = resolved,
    persisting = persisting
  )
}
