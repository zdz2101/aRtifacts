#' GRP-001: Excessive missingness rate
#'
#' @param domain Domain name.
#' @param target_vars Variables to evaluate for missingness.
#' @param group_var Grouping variable.
#' @param threshold Missingness threshold.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_grp_001_excessive_missingness_rate <- function(domain,
                                                    target_vars,
                                                    group_var = "SITEID",
                                                    threshold = 0.1,
                                                    owner_team = "DM") {
  new_rule(
    id = "GRP-001",
    title = "Excessive missingness rate",
    topology = "A2",
    scope = "group",
    intent = "data_entry",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      if (!(group_var %in% names(data))) {
        return(data.frame())
      }

      check_vars <- intersect(target_vars, names(data))
      if (!length(check_vars)) {
        return(data.frame())
      }

      groups <- unique(data[[group_var]])
      out <- list()
      k <- 0L

      for (g in groups) {
        sub <- data[data[[group_var]] == g, check_vars, drop = FALSE]
        if (!nrow(sub)) {
          next
        }

        rates <- colMeans(is.na(sub) | (vapply(sub, is.character, logical(1)) & sub == ""))
        bad <- rates[rates > threshold]
        if (!length(bad)) {
          next
        }

        k <- k + 1L
        out[[k]] <- data.frame(
          group_id = as.character(g),
          variable = names(bad),
          miss_rate = unname(bad),
          reason = "Group missingness rate above threshold",
          stringsAsFactors = FALSE
        )
      }

      if (!k) {
        return(data.frame())
      }

      do.call(rbind, out)
    }
  )
}
