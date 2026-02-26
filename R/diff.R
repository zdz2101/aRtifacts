#' Diff key rows between two domain snapshots
#'
#' Compares key combinations between an old and new `DomainSnapshot`.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#'
#' @return A list with two `data.frame` elements: `added` and `removed`.
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = example_primary(),
#'   key_vars = c("entity_id", "record_id")
#' )
#' new_data <- example_primary()
#' new_data <- rbind(new_data, transform(new_data[1, , drop = FALSE], entity_id = "E999", record_id = 999))
#' new_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = new_data,
#'   key_vars = c("entity_id", "record_id")
#' )
#' diff_keys(old_dom, new_dom)
diff_keys <- function(old_dom, new_dom) {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  if (!identical(old_dom@key_vars, new_dom@key_vars)) {
    stop("`old_dom` and `new_dom` must share identical key_vars.", call. = FALSE)
  }

  keys <- old_dom@key_vars
  old_keys <- unique(old_dom@data[, keys, drop = FALSE])
  new_keys <- unique(new_dom@data[, keys, drop = FALSE])

  old_id <- .key_id(old_keys, keys)
  new_id <- .key_id(new_keys, keys)

  added <- new_keys[!(new_id %in% old_id), , drop = FALSE]
  removed <- old_keys[!(old_id %in% new_id), , drop = FALSE]

  list(added = added, removed = removed)
}

.safe_rate <- function(numerator, denominator) {
  if (denominator == 0L) {
    if (numerator == 0L) 0 else NA_real_
  } else {
    as.numeric(numerator) / as.numeric(denominator)
  }
}

#' Compare key churn between two domain snapshots
#'
#' Summarizes key-level churn across cuts, including added and removed key
#' counts and rates.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#'
#' @return A one-row `data.frame` with deterministic columns:
#'   `domain`, `old_n_keys`, `new_n_keys`, `added_n`, `removed_n`,
#'   `churn_n`, `added_rate`, `removed_rate`, `churn_rate`,
#'   `net_change_n`, and `net_change_rate`.
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = example_primary(),
#'   key_vars = c("entity_id", "record_id")
#' )
#' new_data <- example_primary()
#' new_data <- rbind(new_data, transform(new_data[1, , drop = FALSE], entity_id = "E999", record_id = 999))
#' new_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = new_data,
#'   key_vars = c("entity_id", "record_id")
#' )
#' churn <- compare_key_churn(old_dom, new_dom)
#' churn$churn_rate > 0.10
compare_key_churn <- function(old_dom, new_dom) {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  if (!identical(old_dom@key_vars, new_dom@key_vars)) {
    stop("`old_dom` and `new_dom` must share identical key_vars.", call. = FALSE)
  }

  key_diff <- diff_keys(old_dom, new_dom)
  old_n_keys <- nrow(unique(old_dom@data[, old_dom@key_vars, drop = FALSE]))
  new_n_keys <- nrow(unique(new_dom@data[, new_dom@key_vars, drop = FALSE]))
  added_n <- nrow(key_diff$added)
  removed_n <- nrow(key_diff$removed)
  churn_n <- added_n + removed_n
  net_change_n <- new_n_keys - old_n_keys

  data.frame(
    domain = as.character(old_dom@name),
    old_n_keys = as.integer(old_n_keys),
    new_n_keys = as.integer(new_n_keys),
    added_n = as.integer(added_n),
    removed_n = as.integer(removed_n),
    churn_n = as.integer(churn_n),
    added_rate = .safe_rate(added_n, old_n_keys),
    removed_rate = .safe_rate(removed_n, old_n_keys),
    churn_rate = .safe_rate(churn_n, old_n_keys),
    net_change_n = as.integer(net_change_n),
    net_change_rate = .safe_rate(net_change_n, old_n_keys),
    stringsAsFactors = FALSE
  )
}

#' Compare record volume between two domain snapshots
#'
#' Summarizes record-count changes across cuts, including absolute and
#' relative change.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#'
#' @return A one-row `data.frame` with deterministic columns:
#'   `domain`, `old_n_records`, `new_n_records`, `absolute_change`,
#'   `relative_change`, and `direction` (`"increase"`, `"decrease"`,
#'   `"unchanged"`).
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = example_primary(),
#'   key_vars = c("entity_id", "record_id")
#' )
#' new_data <- example_primary()[1:4, , drop = FALSE]
#' new_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = new_data,
#'   key_vars = c("entity_id", "record_id")
#' )
#' vol <- compare_record_volume(old_dom, new_dom)
#' vol$relative_change < -0.20
compare_record_volume <- function(old_dom, new_dom) {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  old_n_records <- nrow(old_dom@data)
  new_n_records <- nrow(new_dom@data)
  absolute_change <- new_n_records - old_n_records

  direction <- if (absolute_change > 0L) {
    "increase"
  } else if (absolute_change < 0L) {
    "decrease"
  } else {
    "unchanged"
  }

  data.frame(
    domain = as.character(old_dom@name),
    old_n_records = as.integer(old_n_records),
    new_n_records = as.integer(new_n_records),
    absolute_change = as.integer(absolute_change),
    relative_change = .safe_rate(absolute_change, old_n_records),
    direction = direction,
    stringsAsFactors = FALSE
  )
}

.quantile_or_na <- function(x, prob) {
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = prob, names = FALSE, type = 7, na.rm = TRUE))
}

#' Compare distribution shift between two domain snapshots
#'
#' Summarizes group-level distribution drift for a single numeric variable
#' between old and new cuts, using dependency-free metrics and optional
#' Kolmogorov-Smirnov statistics from `stats`.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#' @param var Single variable name to compare.
#' @param by Grouping variable name. Default `"group_id"`.
#'
#' @return A tidy `data.frame` with one row per group and deterministic
#'   columns: `domain`, `var`, `by`, `group`, `n_old`, `n_new`,
#'   `mean_old`, `mean_new`, `mean_delta`, `median_old`, `median_new`,
#'   `median_delta`, `q10_old`, `q10_new`, `q10_delta`, `q25_old`,
#'   `q25_new`, `q25_delta`, `q75_old`, `q75_new`, `q75_delta`, `q90_old`,
#'   `q90_new`, `q90_delta`, `ks_stat`, and `ks_p_value`.
#' @export
#'
#' @examples
#' old_df <- data.frame(
#'   ID = 1:6,
#'   group_id = c("A", "A", "A", "B", "B", "B"),
#'   value = c(10, 11, 12, 20, 21, 22),
#'   stringsAsFactors = FALSE
#' )
#' new_df <- old_df
#' new_df$value[new_df$group_id == "A"] <- new_df$value[new_df$group_id == "A"] + 5
#'
#' old_dom <- new_domain_snapshot("orders", old_df, key_vars = "ID")
#' new_dom <- new_domain_snapshot("orders", new_df, key_vars = "ID")
#'
#' compare_distribution_shift(old_dom, new_dom, var = "value", by = "group_id")
compare_distribution_shift <- function(old_dom, new_dom, var, by = "group_id") {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  if (!is.character(var) || length(var) != 1L || is.na(var) || !nzchar(var)) {
    stop("`var` must be a single non-empty character scalar.", call. = FALSE)
  }

  if (!is.character(by) || length(by) != 1L || is.na(by) || !nzchar(by)) {
    stop("`by` must be a single non-empty character scalar.", call. = FALSE)
  }

  if (!(var %in% names(old_dom@data) && var %in% names(new_dom@data))) {
    stop("`var` must exist in both snapshots.", call. = FALSE)
  }

  if (!(by %in% names(old_dom@data) && by %in% names(new_dom@data))) {
    stop("`by` must exist in both snapshots.", call. = FALSE)
  }

  old_group <- as.character(old_dom@data[[by]])
  new_group <- as.character(new_dom@data[[by]])
  groups <- sort(unique(c(old_group, new_group)))

  if (!length(groups)) {
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

  rows <- lapply(groups, function(g) {
    old_vals <- suppressWarnings(as.numeric(old_dom@data[[var]][old_group == g]))
    new_vals <- suppressWarnings(as.numeric(new_dom@data[[var]][new_group == g]))

    old_vals <- old_vals[!is.na(old_vals)]
    new_vals <- new_vals[!is.na(new_vals)]

    mean_old <- if (length(old_vals)) mean(old_vals) else NA_real_
    mean_new <- if (length(new_vals)) mean(new_vals) else NA_real_
    median_old <- if (length(old_vals)) stats::median(old_vals) else NA_real_
    median_new <- if (length(new_vals)) stats::median(new_vals) else NA_real_

    q10_old <- .quantile_or_na(old_vals, 0.10)
    q10_new <- .quantile_or_na(new_vals, 0.10)
    q25_old <- .quantile_or_na(old_vals, 0.25)
    q25_new <- .quantile_or_na(new_vals, 0.25)
    q75_old <- .quantile_or_na(old_vals, 0.75)
    q75_new <- .quantile_or_na(new_vals, 0.75)
    q90_old <- .quantile_or_na(old_vals, 0.90)
    q90_new <- .quantile_or_na(new_vals, 0.90)

    ks_stat <- NA_real_
    ks_p_value <- NA_real_
    if (length(old_vals) >= 2L && length(new_vals) >= 2L) {
      ks <- suppressWarnings(stats::ks.test(old_vals, new_vals))
      ks_stat <- as.numeric(ks$statistic)
      ks_p_value <- as.numeric(ks$p.value)
    }

    data.frame(
      domain = as.character(old_dom@name),
      var = var,
      by = by,
      group = g,
      n_old = as.integer(length(old_vals)),
      n_new = as.integer(length(new_vals)),
      mean_old = mean_old,
      mean_new = mean_new,
      mean_delta = mean_new - mean_old,
      median_old = median_old,
      median_new = median_new,
      median_delta = median_new - median_old,
      q10_old = q10_old,
      q10_new = q10_new,
      q10_delta = q10_new - q10_old,
      q25_old = q25_old,
      q25_new = q25_new,
      q25_delta = q25_new - q25_old,
      q75_old = q75_old,
      q75_new = q75_new,
      q75_delta = q75_new - q75_old,
      q90_old = q90_old,
      q90_new = q90_new,
      q90_delta = q90_new - q90_old,
      ks_stat = ks_stat,
      ks_p_value = ks_p_value,
      stringsAsFactors = FALSE
    )
  })

  out <- do.call(rbind, rows)
  out[order(out$group), , drop = FALSE]
}

.column_type_string <- function(x) {
  paste(class(x), collapse = "|")
}

#' Diff key variable types between two domain snapshots
#'
#' Compares classes of key variables between an old and new `DomainSnapshot`.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#'
#' @return A `data.frame` with `key_var`, `old_type`, `new_type`, and
#'   `changed`.
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = example_primary(),
#'   key_vars = c("entity_id", "record_id")
#' )
#' new_data <- example_primary()
#' new_data$entity_id <- as.factor(new_data$entity_id)
#' new_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = new_data,
#'   key_vars = c("entity_id", "record_id")
#' )
#' diff_key_types(old_dom, new_dom)
diff_key_types <- function(old_dom, new_dom) {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  key_vars <- sort(unique(c(old_dom@key_vars, new_dom@key_vars)))
  if (!length(key_vars)) {
    return(data.frame(
      key_var = character(0),
      old_type = character(0),
      new_type = character(0),
      changed = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  old_type <- vapply(
    key_vars,
    function(v) {
      if (v %in% names(old_dom@data)) .column_type_string(old_dom@data[[v]]) else NA_character_
    },
    character(1)
  )
  new_type <- vapply(
    key_vars,
    function(v) {
      if (v %in% names(new_dom@data)) .column_type_string(new_dom@data[[v]]) else NA_character_
    },
    character(1)
  )

  data.frame(
    key_var = key_vars,
    old_type = unname(old_type),
    new_type = unname(new_type),
    changed = !(is.na(old_type) & is.na(new_type)) &
      (is.na(old_type) | is.na(new_type) | old_type != new_type),
    stringsAsFactors = FALSE
  )
}

#' Diff column presence between two domain snapshots
#'
#' Compares old vs new dataset column presence and reports columns that are
#' added, removed, or unchanged.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#'
#' @return A `data.frame` with `column`, `old_present`, `new_present`,
#'   `status`, and `changed`.
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = example_primary(),
#'   key_vars = c("entity_id", "record_id")
#' )
#' new_data <- example_primary()
#' new_data$NEWCOL <- "X"
#' new_data$delta <- NULL
#' new_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = new_data,
#'   key_vars = c("entity_id", "record_id")
#' )
#' diff_columns(old_dom, new_dom)
diff_columns <- function(old_dom, new_dom) {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  columns <- sort(unique(c(names(old_dom@data), names(new_dom@data))))
  if (!length(columns)) {
    return(data.frame(
      column = character(0),
      old_present = logical(0),
      new_present = logical(0),
      status = character(0),
      changed = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  old_present <- columns %in% names(old_dom@data)
  new_present <- columns %in% names(new_dom@data)
  status <- ifelse(
    old_present & new_present,
    "unchanged",
    ifelse(!old_present & new_present, "added", "removed")
  )

  data.frame(
    column = columns,
    old_present = old_present,
    new_present = new_present,
    status = status,
    changed = status != "unchanged",
    stringsAsFactors = FALSE
  )
}

#' Diff shared column types between two domain snapshots
#'
#' Compares classes for columns that exist in both snapshots.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#'
#' @return A `data.frame` with `column`, `old_type`, `new_type`, and
#'   `changed`.
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = example_primary(),
#'   key_vars = c("entity_id", "record_id")
#' )
#' new_data <- example_primary()
#' new_data$value <- as.character(new_data$value)
#' new_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = new_data,
#'   key_vars = c("entity_id", "record_id")
#' )
#' diff_column_types(old_dom, new_dom)
diff_column_types <- function(old_dom, new_dom) {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  columns <- sort(intersect(names(old_dom@data), names(new_dom@data)))
  if (!length(columns)) {
    return(data.frame(
      column = character(0),
      old_type = character(0),
      new_type = character(0),
      changed = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  old_type <- vapply(columns, function(v) .column_type_string(old_dom@data[[v]]), character(1))
  new_type <- vapply(columns, function(v) .column_type_string(new_dom@data[[v]]), character(1))

  data.frame(
    column = columns,
    old_type = unname(old_type),
    new_type = unname(new_type),
    changed = old_type != new_type,
    stringsAsFactors = FALSE
  )
}

#' Diff values between two domain snapshots
#'
#' Compares one or more variables on matched keys between two
#' `DomainSnapshot` objects.
#'
#' @param old_dom Previous `DomainSnapshot`.
#' @param new_dom Current `DomainSnapshot`.
#' @param vars Character vector of variables to compare.
#'
#' @return A `data.frame` with key columns, `var`, `old`, and `new` for
#'   changed values.
#' @export
#'
#' @examples
#' old_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = example_primary(),
#'   key_vars = c("entity_id", "record_id")
#' )
#' new_data <- example_primary()
#' new_data$value[new_data$entity_id == "E001" & new_data$record_id == 1] <- 141
#' new_dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = new_data,
#'   key_vars = c("entity_id", "record_id")
#' )
#' diff_values(old_dom, new_dom, vars = "value")
diff_values <- function(old_dom, new_dom, vars = "value") {
  .assert_domain(old_dom, "old_dom")
  .assert_domain(new_dom, "new_dom")

  if (!identical(old_dom@key_vars, new_dom@key_vars)) {
    stop("`old_dom` and `new_dom` must share identical key_vars.", call. = FALSE)
  }

  keys <- old_dom@key_vars
  shared_vars <- intersect(vars, intersect(names(old_dom@data), names(new_dom@data)))
  if (!length(shared_vars)) {
    return(data.frame())
  }

  merged <- merge(
    old_dom@data[, c(keys, shared_vars), drop = FALSE],
    new_dom@data[, c(keys, shared_vars), drop = FALSE],
    by = keys,
    suffixes = c(".old", ".new"),
    all = FALSE
  )

  out <- vector("list", length(shared_vars))
  idx <- 0L

  for (var in shared_vars) {
    old_col <- paste0(var, ".old")
    new_col <- paste0(var, ".new")
    changed <- !(is.na(merged[[old_col]]) & is.na(merged[[new_col]])) &
      (is.na(merged[[old_col]]) | is.na(merged[[new_col]]) | merged[[old_col]] != merged[[new_col]])

    if (any(changed)) {
      idx <- idx + 1L
      piece <- merged[changed, keys, drop = FALSE]
      piece$var <- var
      piece$old <- merged[[old_col]][changed]
      piece$new <- merged[[new_col]][changed]
      out[[idx]] <- piece
    }
  }

  if (!idx) {
    return(data.frame())
  }

  do.call(rbind, out[seq_len(idx)])
}

#' Diff artifact metadata
#'
#' Compares top-level metadata fields between two `ArtifactSnapshot` objects.
#'
#' @param old_art Previous `ArtifactSnapshot`.
#' @param new_art Current `ArtifactSnapshot`.
#'
#' @return A `data.frame` with `field`, `old`, `new`, and `changed`.
#' @export
#'
#' @examples
#' dom <- new_domain_snapshot(
#'   name = "orders",
#'   data = example_primary(),
#'   key_vars = c("entity_id", "record_id")
#' )
#' old_art <- new_artifact_snapshot(domains = list(orders = dom), build_id = "run-001")
#' new_art <- new_artifact_snapshot(domains = list(orders = dom), build_id = "run-002")
#' diff_meta_artifact(old_art, new_art)
diff_meta_artifact <- function(old_art, new_art) {
  .assert_artifact(old_art, "old_art")
  .assert_artifact(new_art, "new_art")

  fields <- c("spec_version", "data_cut", "build_id", "code_sha", "inputs_hash")
  old_vals <- vapply(fields, function(x) S7::prop(old_art, x), character(1))
  new_vals <- vapply(fields, function(x) S7::prop(new_art, x), character(1))

  data.frame(
    field = fields,
    old = unname(old_vals),
    new = unname(new_vals),
    changed = !mapply(identical, old_vals, new_vals),
    stringsAsFactors = FALSE
  )
}

#' Compare flag states between two runs
#'
#' Compares two flag tables (typically outputs from `run_rules()`) and
#' classifies each unique key as `"introduced"`, `"resolved"`, or
#' `"persisting"`.
#'
#' @param old_flags Previous run flag table.
#' @param new_flags Current run flag table.
#' @param by Character vector of key columns used to identify a finding across
#'   runs. Defaults to `c("entity_id", "DOMAIN", "RULE_ID")`.
#'
#' @return A deterministic `data.frame` with columns `by` plus `status`.
#' @export
#'
#' @examples
#' old_flags <- data.frame(
#'   entity_id = c("01", "02"),
#'   DOMAIN = c("orders", "orders"),
#'   RULE_ID = c("TMP-900", "TMP-900"),
#'   stringsAsFactors = FALSE
#' )
#' new_flags <- data.frame(
#'   entity_id = c("02", "03"),
#'   DOMAIN = c("orders", "orders"),
#'   RULE_ID = c("TMP-900", "TMP-900"),
#'   stringsAsFactors = FALSE
#' )
#'
#' compare_flag_states(old_flags, new_flags)
compare_flag_states <- function(old_flags,
                                new_flags,
                                by = c("entity_id", "DOMAIN", "RULE_ID")) {
  if (!is.data.frame(old_flags)) {
    stop("`old_flags` must be a data.frame.", call. = FALSE)
  }

  if (!is.data.frame(new_flags)) {
    stop("`new_flags` must be a data.frame.", call. = FALSE)
  }

  if (!is.character(by) || !length(by) || anyNA(by) || any(!nzchar(by))) {
    stop("`by` must be a non-empty character vector of key columns.", call. = FALSE)
  }

  by <- unique(by)

  ensure_key_cols <- function(df, cols) {
    out <- as.data.frame(df, stringsAsFactors = FALSE)
    missing <- setdiff(cols, names(out))
    n <- nrow(out)
    for (col in missing) {
      out[[col]] <- rep(NA_character_, n)
    }
    out[, cols, drop = FALSE]
  }

  old_key <- unique(ensure_key_cols(old_flags, by))
  new_key <- unique(ensure_key_cols(new_flags, by))
  old_key$.present_old <- rep(TRUE, nrow(old_key))
  new_key$.present_new <- rep(TRUE, nrow(new_key))

  merged <- merge(old_key, new_key, by = by, all = TRUE)
  merged$status <- ifelse(
    is.na(merged$.present_old),
    "introduced",
    ifelse(is.na(merged$.present_new), "resolved", "persisting")
  )

  merged <- merged[, c(by, "status"), drop = FALSE]
  if (!nrow(merged)) {
    return(merged)
  }

  ord <- do.call(order, c(merged[, by, drop = FALSE], list(na.last = TRUE)))
  merged[ord, , drop = FALSE]
}
