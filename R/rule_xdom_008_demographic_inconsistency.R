#' XDOM-008: Demographic inconsistency across domains
#'
#' @param left_domain First domain.
#' @param right_domain Second domain.
#' @param key_var Join key variable.
#' @param left_field Demographic field in first domain.
#' @param right_field Demographic field in second domain.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_xdom_008_demographic_inconsistency <- function(left_domain = "DM",
                                                    right_domain = "VS",
                                                    key_var = "USUBJID",
                                                    left_field = "SEX",
                                                    right_field = "SEX",
                                                    owner_team = "DM") {
  new_rule(
    id = "XDOM-008",
    title = "Demographic inconsistency across domains",
    topology = "A3",
    scope = "record",
    intent = "data_entry",
    domains = c(left_domain, right_domain),
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      left <- .core50_domain_df(domains, left_domain)
      right <- .core50_domain_df(domains, right_domain)
      needed_left <- c(key_var, left_field)
      needed_right <- c(key_var, right_field)
      if (is.null(left) || is.null(right) || !all(needed_left %in% names(left)) || !all(needed_right %in% names(right))) {
        return(data.frame())
      }

      merged <- merge(
        left[, needed_left, drop = FALSE],
        right[, needed_right, drop = FALSE],
        by = key_var,
        suffixes = c("_left", "_right")
      )
      if (!nrow(merged)) {
        return(data.frame())
      }

      left_col <- if (left_field == right_field) paste0(left_field, "_left") else left_field
      right_col <- if (left_field == right_field) paste0(right_field, "_right") else right_field

      bad <- !is.na(merged[[left_col]]) & !is.na(merged[[right_col]]) &
        (as.character(merged[[left_col]]) != as.character(merged[[right_col]]))
      if (!any(bad)) {
        return(data.frame())
      }

      out <- merged[bad, c(key_var, left_col, right_col), drop = FALSE]
      names(out) <- c(key_var, "left_value", "right_value")
      out$reason <- "Demographic field mismatch across domains"
      out
    }
  )
}
