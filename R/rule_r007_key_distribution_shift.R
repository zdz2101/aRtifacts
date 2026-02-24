rule_r007_key_distribution_shift <- function(domain, key_vars, subject_var = "USUBJID", site_var = "SITEID", dup_rate_threshold = 0.1, owner_team = "DM", ...) {
  list(
    id = "R007_KEY_DISTRIBUTION_SHIFT",
    domain = domain,
    severity = "warning",
    category = "ops_anomaly",
    owner_team = owner_team,
    scope = "diff",
    fn = function(old_dom, new_dom) {
      .assert_domain(old_dom, "old_dom")
      .assert_domain(new_dom, "new_dom")

      present_keys <- intersect(key_vars, intersect(names(old_dom@data), names(new_dom@data)))
      if (!length(present_keys)) {
        return(data.frame(
          KEY_VAR = character(0),
          KEY_LEVEL = character(0),
          OLD_RATE = numeric(0),
          NEW_RATE = numeric(0),
          ABS_DIFF = numeric(0),
          stringsAsFactors = FALSE
        ))
      }

      out <- list(); k <- 0L
      for (kv in sort(present_keys)) {
        old_vals <- as.character(old_dom@data[[kv]])
        new_vals <- as.character(new_dom@data[[kv]])
        old_vals[is.na(old_vals)] <- "<NA>"
        new_vals[is.na(new_vals)] <- "<NA>"

        old_tab <- table(old_vals)
        new_tab <- table(new_vals)
        levels_all <- sort(unique(c(names(old_tab), names(new_tab))))
        old_rate <- as.numeric(old_tab[levels_all]) / nrow(old_dom@data)
        new_rate <- as.numeric(new_tab[levels_all]) / nrow(new_dom@data)
        old_rate[is.na(old_rate)] <- 0
        new_rate[is.na(new_rate)] <- 0
        abs_diff <- abs(new_rate - old_rate)
        keep <- abs_diff > dup_rate_threshold
        if (!any(keep)) next

        k <- k + 1L
        out[[k]] <- data.frame(
          KEY_VAR = kv,
          KEY_LEVEL = levels_all[keep],
          OLD_RATE = old_rate[keep],
          NEW_RATE = new_rate[keep],
          ABS_DIFF = abs_diff[keep],
          stringsAsFactors = FALSE
        )
      }

      if (!k) return(data.frame(KEY_VAR = character(0), KEY_LEVEL = character(0), OLD_RATE = numeric(0), NEW_RATE = numeric(0), ABS_DIFF = numeric(0), stringsAsFactors = FALSE))
      out <- do.call(rbind, out)
      out[order(out$KEY_VAR, out$KEY_LEVEL), , drop = FALSE]
    }
  )
}
