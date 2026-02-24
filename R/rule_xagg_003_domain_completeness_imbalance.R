#' XAGG-003: Domain completeness imbalance
#'
#' @param domains_set Domains to compare (minimum 2).
#' @param group_var Grouping variable.
#' @param threshold_max_gap Maximum allowed completeness-rate gap.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_xagg_003_domain_completeness_imbalance <- function(domains_set = c("VS", "LB", "EG"),
                                                        group_var = "SITEID",
                                                        threshold_max_gap = 0.2,
                                                        owner_team = "Clinical Ops") {
  new_rule(
    id = "XAGG-003",
    title = "Domain completeness imbalance",
    topology = "A4",
    scope = "group",
    intent = "ops_anomaly",
    domains = domains_set,
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      if (length(domains_set) < 2L) {
        return(data.frame())
      }

      counts_by_domain <- list()
      for (nm in domains_set) {
        df <- .core50_domain_df(domains, nm)
        if (is.null(df) || !(group_var %in% names(df))) {
          next
        }
        counts <- as.data.frame(table(df[[group_var]]), stringsAsFactors = FALSE)
        names(counts) <- c(group_var, paste0("n_", nm))
        counts_by_domain[[nm]] <- counts
      }

      if (length(counts_by_domain) < 2L) {
        return(data.frame())
      }

      merged <- counts_by_domain[[1]]
      if (length(counts_by_domain) > 1L) {
        for (i in 2:length(counts_by_domain)) {
          merged <- merge(merged, counts_by_domain[[i]], by = group_var, all = TRUE)
        }
      }

      count_cols <- grep("^n_", names(merged), value = TRUE)
      merged[count_cols][is.na(merged[count_cols])] <- 0
      row_tot <- rowSums(merged[, count_cols, drop = FALSE])
      max_rate <- apply(merged[, count_cols, drop = FALSE], 1, function(x) max(.core50_safe_rate(x, sum(x)), na.rm = TRUE))
      min_rate <- apply(merged[, count_cols, drop = FALSE], 1, function(x) min(.core50_safe_rate(x, sum(x)), na.rm = TRUE))
      gap <- max_rate - min_rate

      bad <- !is.na(gap) & gap > threshold_max_gap & row_tot > 0
      if (!any(bad)) {
        return(data.frame())
      }

      out <- merged[bad, c(group_var, count_cols), drop = FALSE]
      out$max_rate <- max_rate[bad]
      out$min_rate <- min_rate[bad]
      out$gap <- gap[bad]
      out$threshold <- as.numeric(threshold_max_gap)
      out$reason <- "Cross-domain completeness imbalance"
      out
    }
  )
}
