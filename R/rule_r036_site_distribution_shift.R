#' Rule R036 — group-level distribution shift
#'
#' Builds a `DiffFlagRule` that compares per-group
#' distributions between old and new cuts and flags groups with detectable
#' drift.
#'
#' @inheritParams make_rule_pack_plausibility
#' @return A `DiffFlagRule` object.
rule_r036_site_distribution_shift <- function(domain, key_vars, subject_var = "USUBJID", group_var = "SITEID", numeric_vars = character(), non_negative_vars = character(), value_ranges = list(), visit_order_var = NULL, max_within_change = list(), iqr_multiplier = 3.0, zero_rate_threshold = 0.5, low_cv_threshold = 0.01, round_rate_threshold = 0.9, owner_team = "Biostats", ...) {
  new_diff_flag_rule(
    id = "R036_GROUP_DISTRIBUTION_SHIFT",
    domain = domain,
    severity = "warning",
    category = "ops_anomaly",
    owner_team = owner_team,
    fn = function(old_dom, new_dom) {
      .assert_domain(old_dom, "old_dom")
      .assert_domain(new_dom, "new_dom")

      vars <- numeric_vars
      if (!length(vars)) {
        shared <- intersect(names(old_dom@data), names(new_dom@data))
        shared <- setdiff(shared, unique(c(key_vars, subject_var, group_var)))
        vars <- shared[vapply(
          shared,
          function(v) is.numeric(old_dom@data[[v]]) && is.numeric(new_dom@data[[v]]),
          logical(1)
        )]
      }

      if (!length(vars)) {
        return(data.frame(
          domain = character(0),
          var = character(0),
          by = character(0),
          group = character(0),
          n_old = integer(0),
          n_new = integer(0),
          mean_old = numeric(0),
          mean_new = numeric(0),
          mean_delta = numeric(0),
          median_old = numeric(0),
          median_new = numeric(0),
          median_delta = numeric(0),
          q10_old = numeric(0),
          q10_new = numeric(0),
          q10_delta = numeric(0),
          q25_old = numeric(0),
          q25_new = numeric(0),
          q25_delta = numeric(0),
          q75_old = numeric(0),
          q75_new = numeric(0),
          q75_delta = numeric(0),
          q90_old = numeric(0),
          q90_new = numeric(0),
          q90_delta = numeric(0),
          ks_stat = numeric(0),
          ks_p_value = numeric(0),
          stringsAsFactors = FALSE
        ))
      }

      pieces <- lapply(vars, function(v) {
        out <- compare_distribution_shift(old_dom, new_dom, var = v, by = group_var)
        if (!nrow(out)) return(out)

        changed <- (out$n_old > 0L & out$n_new > 0L) & (
          out$mean_delta != 0 |
            out$median_delta != 0 |
            out$q25_delta != 0 |
            out$q75_delta != 0 |
            (!is.na(out$ks_stat) & out$ks_stat > 0)
        )

        out[changed, , drop = FALSE]
      })

      pieces <- Filter(function(x) nrow(x) > 0L, pieces)
      if (!length(pieces)) {
        return(data.frame(
          domain = character(0),
          var = character(0),
          by = character(0),
          group = character(0),
          n_old = integer(0),
          n_new = integer(0),
          mean_old = numeric(0),
          mean_new = numeric(0),
          mean_delta = numeric(0),
          median_old = numeric(0),
          median_new = numeric(0),
          median_delta = numeric(0),
          q10_old = numeric(0),
          q10_new = numeric(0),
          q10_delta = numeric(0),
          q25_old = numeric(0),
          q25_new = numeric(0),
          q25_delta = numeric(0),
          q75_old = numeric(0),
          q75_new = numeric(0),
          q75_delta = numeric(0),
          q90_old = numeric(0),
          q90_new = numeric(0),
          q90_delta = numeric(0),
          ks_stat = numeric(0),
          ks_p_value = numeric(0),
          stringsAsFactors = FALSE
        ))
      }

      out <- do.call(rbind, pieces)
      out[order(out$var, out$group), , drop = FALSE]
    }
  )
}

rule_r036_group_distribution_shift <- rule_r036_site_distribution_shift
