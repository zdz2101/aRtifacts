#' AGG-003: Domain completeness imbalance
#'
#' @param domains_set Domain set evaluated for group-level balance.
#' @param group_var Group variable used for comparison.
#' @param threshold_max_gap Maximum allowed rate gap across domains per group.
#' @param owner_team Owner team.
#'
#' @return A `CoreRule` object.
#' @export
rule_agg_003_domain_completeness_imbalance <- function(domains_set = c("PRIMARY", "SUPPORT", "AUX"),
                                                       group_var = "group_id",
                                                       threshold_max_gap = 0.2,
                                                       owner_team = "Data Operations") {
  new_rule(
    id = "AGG-003",
    title = "Domain completeness imbalance",
    topology = "A4",
    scope = "group",
    intent = "ops_anomaly",
    domains = domains_set,
    owner_team = owner_team,
    fn = function(domains, context, rule) {
      count_tabs <- list()
      for (nm in domains_set) {
        df <- .topology_domain_df(domains, nm)
        if (is.null(df) || !(group_var %in% names(df))) {
          return(data.frame())
        }
        tab <- as.data.frame(table(as.character(df[[group_var]])), stringsAsFactors = FALSE)
        names(tab) <- c("group", paste0("n_", nm))
        count_tabs[[nm]] <- tab
      }

      merged <- Reduce(function(x, y) merge(x, y, by = "group", all = TRUE), count_tabs)
      merged[is.na(merged)] <- 0L

      count_cols <- grep("^n_", names(merged), value = TRUE)
      if (!length(count_cols)) {
        return(data.frame())
      }

      rate_gap <- apply(merged[, count_cols, drop = FALSE], 1, function(x) {
        total <- sum(x)
        if (!is.finite(total) || total <= 0) {
          return(NA_real_)
        }
        rates <- x / total
        max(rates) - min(rates)
      })

      bad <- !is.na(rate_gap) & rate_gap > threshold_max_gap
      if (!any(bad)) {
        return(data.frame())
      }

      out <- merged[bad, c("group", count_cols), drop = FALSE]
      names(out)[names(out) == "group"] <- group_var
      out$rate_gap <- as.numeric(rate_gap[bad])
      out$threshold <- as.numeric(threshold_max_gap)
      out$reason <- "Cross-domain group balance gap exceeds threshold"
      out
    }
  )
}
