#' ID-004: High duplicate rate by subject
#'
#' @param domain Domain name.
#' @param key_vars Key variables for duplicate detection.
#' @param subject_var Subject variable.
#' @param threshold Duplicate-rate threshold.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_id_004_high_duplicate_rate_by_subject <- function(domain,
                                                       key_vars,
                                                       subject_var = "USUBJID",
                                                       threshold = 0.1,
                                                       owner_team = "DM") {
  new_rule(
    id = "ID-004",
    title = "High duplicate rate by subject",
    topology = "A2",
    scope = "group",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      if (!(subject_var %in% names(data))) {
        return(data.frame())
      }

      present_keys <- intersect(key_vars, names(data))
      if (!length(present_keys)) {
        return(data.frame())
      }

      dup <- duplicated(data[, present_keys, drop = FALSE]) |
        duplicated(data[, present_keys, drop = FALSE], fromLast = TRUE)

      by_subject <- tapply(dup, data[[subject_var]], mean, na.rm = TRUE)
      bad_subjects <- names(by_subject)[!is.na(by_subject) & by_subject > threshold]
      if (!length(bad_subjects)) {
        return(data.frame())
      }

      keep <- dup & data[[subject_var]] %in% bad_subjects
      out <- data[keep, unique(c(subject_var, present_keys)), drop = FALSE]
      out$dup_rate <- unname(by_subject[as.character(out[[subject_var]])])
      out$reason <- "Subject duplicate rate above threshold"
      out
    }
  )
}
