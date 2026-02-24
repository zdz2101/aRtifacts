#' GRP-003: Repeated identical lab values
#'
#' @param domain Domain name.
#' @param subject_var Subject variable.
#' @param test_var Test code variable.
#' @param value_var Numeric lab value variable.
#' @param min_repeats Minimum rows in a subject/test stratum.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_grp_003_repeated_identical_lab_values <- function(domain,
                                                       subject_var = "USUBJID",
                                                       test_var = "LBTESTCD",
                                                       value_var = "LBSTRESN",
                                                       min_repeats = 3,
                                                       owner_team = "Clinical Ops") {
  new_rule(
    id = "GRP-003",
    title = "Repeated identical lab values",
    topology = "A2",
    scope = "group",
    intent = "ops_anomaly",
    domain = domain,
    owner_team = owner_team,
    fn = function(data, context, rule) {
      needed <- c(subject_var, test_var, value_var)
      if (!all(needed %in% names(data))) {
        return(data.frame())
      }

      key <- paste(data[[subject_var]], data[[test_var]], sep = "||")
      split_idx <- split(seq_len(nrow(data)), key)

      out <- list()
      k <- 0L

      for (nm in names(split_idx)) {
        idx <- split_idx[[nm]]
        if (length(idx) < min_repeats) {
          next
        }

        vals <- suppressWarnings(as.numeric(data[[value_var]][idx]))
        vals <- vals[!is.na(vals)]
        if (!length(vals)) {
          next
        }

        if (length(unique(vals)) == 1L) {
          parts <- strsplit(nm, "\\|\\|", fixed = FALSE)[[1]]
          k <- k + 1L
          out[[k]] <- data.frame(
            subject_id = parts[[1]],
            test_code = parts[[2]],
            repeated_n = length(idx),
            unique_value_n = 1L,
            reason = "Repeated identical values within subject/test",
            stringsAsFactors = FALSE
          )
        }
      }

      if (!k) {
        return(data.frame())
      }

      do.call(rbind, out)
    }
  )
}
