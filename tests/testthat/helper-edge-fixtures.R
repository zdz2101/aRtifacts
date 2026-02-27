# Shared test helpers for broad edge-case coverage.
# Intentionally limited to setup/boilerplate so rule intent remains readable
# inside each individual test file.

# Canonical vectors used by edge scenarios across rule families.
edge_dirty_vectors <- function() {
  list(
    character = c("", " ", NA_character_, "A", "0"),
    numeric = c(0, -1, NA_real_, 1, 999),
    date_like = c("2024-01-01", "", " ", "not-a-date", NA_character_)
  )
}

edge_default_rule_domains <- function() {
  c(
    "PRIMARY", "REFERENCE", "CHILD", "PARENT", "LEFT", "RIGHT",
    "EVENTS", "WINDOW", "SUPPORT", "AUX", "DRIVER", "METRIC", "OTHER",
    "ADSL", "AE", "DM", "LB"
  )
}

edge_default_domain_data <- function(clean = TRUE, domain_name = "PRIMARY") {
  n <- 6L
  row_key <- seq_len(n)

  if (isTRUE(clean)) {
    data.frame(
      .edge_row_id = row_key,
      entity_id = paste0("E", row_key),
      record_id = row_key,
      group_id = rep(c("G1", "G2"), length.out = n),
      site_id = rep(c("S1", "S2"), length.out = n),
      event_id = row_key,
      support_id = row_key,
      aux_id = row_key,
      event_time = rep("2024-01-10", n),
      window_start = rep("2024-01-01", n),
      window_end = rep("2024-01-31", n),
      event_date = rep("2024-01-10", n),
      start_date = rep("2024-01-01", n),
      end_date = rep("2024-01-31", n),
      value = as.numeric(row_key),
      driver_value = as.numeric(row_key),
      metric_value = as.numeric(row_key),
      duration = as.numeric(row_key),
      status = rep("N", n),
      required_field = rep("ok", n),
      state_a = rep("N", n),
      state_b = rep("N", n),
      test_code = rep("ALT", n),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      .edge_row_id = row_key,
      entity_id = c("E1", "", " ", NA_character_, "E5", "E6"),
      record_id = c(1, 2, 3, 4, 5, 6),
      group_id = c("G1", "", "G2", NA_character_, " ", "G2"),
      site_id = c("S1", "", "S2", NA_character_, " ", "S2"),
      event_id = row_key,
      support_id = row_key,
      aux_id = row_key,
      event_time = c("2024-01-10", "", "not-a-date", NA_character_, "2024-02-01", "2024-01-15"),
      window_start = c("2024-01-01", "2024-01-01", "", NA_character_, "2024-01-01", "2024-01-01"),
      window_end = c("2024-01-31", "", "2024-01-31", NA_character_, "2024-01-15", "2024-01-31"),
      event_date = c("2024-01-10", "", "not-a-date", NA_character_, "2024-01-03", "2024-01-04"),
      start_date = c("2024-01-10", "bad-date", "", NA_character_, "2024-02-01", "2024-01-05"),
      end_date = c("2024-01-01", "2024-01-20", "bad-date", NA_character_, "2024-01-15", ""),
      value = c(0, NA_real_, 2, 3, -1, 999),
      driver_value = c(0, NA_real_, 2, 3, -1, 999),
      metric_value = c(0, NA_real_, 2, 3, -1, 999),
      duration = c(0, -1, NA_real_, 1, 2, 3),
      status = c("Y", "N", "Y", "N", "", NA_character_),
      required_field = c("", "ok", " ", NA_character_, "ok", "ok"),
      state_a = c("M", "N", "M", "", NA_character_, "N"),
      state_b = c("Y", "N", " ", "Y", "N", NA_character_),
      test_code = c("ALT", "ALT", "AST", "AST", "ALT", "AST"),
      stringsAsFactors = FALSE
    )
  }
}

# Build a DomainSnapshot and transparently enforce unique keys for fixtures.
# If requested keys are missing/non-unique, fall back to synthetic row key.
edge_new_domain <- function(name, data, key_vars = NULL) {
  df <- as.data.frame(data, stringsAsFactors = FALSE)

  if (is.null(key_vars) || !length(key_vars)) {
    key_vars <- ".edge_row_id"
  }

  if (!all(key_vars %in% names(df)) || anyDuplicated(df[, key_vars, drop = FALSE])) {
    df$.edge_row_id <- seq_len(nrow(df))
    key_vars <- ".edge_row_id"
  }

  new_domain_snapshot(name = name, data = df, key_vars = key_vars)
}

# Build ArtifactSnapshot from named domain data.frames.
edge_make_artifact <- function(domains_data,
                               key_vars_by_domain = list()) {
  stopifnot(is.list(domains_data), length(domains_data) > 0)

  doms <- lapply(names(domains_data), function(name) {
    data <- domains_data[[name]]
    keys <- NULL
    if (length(key_vars_by_domain) && name %in% names(key_vars_by_domain)) {
      keys <- key_vars_by_domain[[name]]
    }
    edge_new_domain(name = name, data = data, key_vars = keys)
  })
  names(doms) <- names(domains_data)

  new_artifact_snapshot(domains = doms)
}

# Build RuleContext with optional previous snapshot and runtime options.
edge_make_context <- function(current_domains,
                              previous_domains = NULL,
                              options = list(),
                              current_key_vars = list(),
                              previous_key_vars = list()) {
  current <- edge_make_artifact(current_domains, key_vars_by_domain = current_key_vars)
  previous <- NULL
  if (!is.null(previous_domains)) {
    previous <- edge_make_artifact(previous_domains, key_vars_by_domain = previous_key_vars)
  }

  new_rule_context(current = current, previous = previous, options = options)
}

# Convenience context for broad contract tests:
# clean=TRUE -> should be quiet for most rules
# clean=FALSE -> should not crash under messy payloads
edge_make_full_context <- function(clean = TRUE,
                                   with_previous = TRUE,
                                   options = list()) {
  domain_names <- edge_default_rule_domains()

  current_domains <- setNames(
    lapply(domain_names, function(nm) edge_default_domain_data(clean = clean, domain_name = nm)),
    domain_names
  )

  previous_domains <- NULL
  if (isTRUE(with_previous)) {
    previous_domains <- setNames(
      lapply(domain_names, function(nm) edge_default_domain_data(clean = TRUE, domain_name = nm)),
      domain_names
    )

    if (isTRUE(clean)) {
      previous_domains <- current_domains
    }
  }

  edge_make_context(
    current_domains = current_domains,
    previous_domains = previous_domains,
    options = options
  )
}

# Common assertion for rule outputs without hiding scenario-specific checks.
edge_expect_rule_output <- function(out,
                                    expected_id = NULL,
                                    require_rows = FALSE) {
  expect_s3_class(out, "data.frame")

  if (isTRUE(require_rows)) {
    expect_gt(nrow(out), 0L)
  }

  if (!is.null(expected_id) && nrow(out) > 0L) {
    expect_true("RULE_ID" %in% names(out))
    expect_true(all(out$RULE_ID == expected_id))
  }
}

# Heuristic defaults for constructor smoke/contract tests.
edge_default_arg_value <- function(arg_name) {
  if (grepl("domains$", arg_name)) {
    return(c("PRIMARY", "REFERENCE"))
  }

  if (grepl("domain", arg_name)) {
    if (grepl("right|parent|reference|support|other", arg_name)) {
      return("REFERENCE")
    }
    return("PRIMARY")
  }

  if (grepl("keys$|key_vars$", arg_name)) {
    return(c("entity_id", "record_id"))
  }

  if (grepl("_var$|_by$|_field$", arg_name)) {
    return("entity_id")
  }

  if (grepl("_vars$|_fields$", arg_name)) {
    return(c("entity_id"))
  }

  if (grepl("levels$", arg_name)) {
    return(c("A", "B"))
  }

  if (grepl("threshold|tol|rate|k$|window|days|multiplier|min|max|cutoff", arg_name)) {
    return(1)
  }

  1
}

edge_build_rule <- function(fn, explicit = list()) {
  fn_formals <- formals(fn)
  required <- names(fn_formals)[vapply(fn_formals, function(x) identical(x, quote(expr = )), logical(1))]

  args <- explicit
  for (arg_name in required) {
    if (!(arg_name %in% names(args))) {
      args[[arg_name]] <- edge_default_arg_value(arg_name)
    }
  }

  do.call(fn, args)
}

# Overrides for rules that need explicit clean-boundary settings.
edge_boundary_overrides <- function(rule_name) {
  switch(
    rule_name,
    rule_de_003_invalid_categorical_code = list(
      domain = "PRIMARY",
      value_var = "state_a",
      allowed_levels = c("N", "M")
    ),
    rule_tmp_001_invalid_date_format = list(
      domain = "PRIMARY",
      date_var = "event_date"
    ),
    list()
  )
}

edge_build_rule_for_boundary <- function(rule_name) {
  fn <- get(rule_name, mode = "function")
  edge_build_rule(fn, explicit = edge_boundary_overrides(rule_name))
}
