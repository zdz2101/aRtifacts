# Guided-builder helpers ------------------------------------------------------
# These helpers isolate wizard-specific decisions so behavior can be validated
# with unit tests instead of relying only on interactive UI checks.

.rb_guided_step_labels <- function() {
  c(
    "Pack Setup",
    "Topology Questions",
    "Rule Core Metadata",
    "Confirm + Add Rule",
    "Finish / Export"
  )
}

.rb_guided_n_domains <- function(bucket) {
  key <- .rb_scalar_text(bucket, default = "1")
  if (identical(key, "2+")) {
    return(2L)
  }
  1L
}

.rb_guided_check_kind_choices <- function(is_longitudinal, n_domain_bucket) {
  long <- isTRUE(is_longitudinal)
  multi <- identical(.rb_scalar_text(n_domain_bucket, default = "1"), "2+")

  if (!long && !multi) {
    return(c(
      "Schema structure check" = "schema",
      "Record-level check" = "record",
      "Aggregate/group check" = "aggregate"
    ))
  }

  if (!long && multi) {
    return(c(
      "Cross-domain relational check" = "relational",
      "Cross-domain aggregate check" = "aggregate"
    ))
  }

  if (long && !multi) {
    return(c(
      "Record-level longitudinal check" = "record",
      "Aggregate longitudinal check" = "aggregate"
    ))
  }

  # Longitudinal + multi-domain maps to A7 directly.
  character()
}

.rb_guided_resolve_topology <- function(is_longitudinal,
                                        n_domain_bucket,
                                        check_kind = "",
                                        topology_override_used = FALSE,
                                        topology_override = "A1") {
  if (isTRUE(topology_override_used)) {
    override <- toupper(.rb_scalar_text(topology_override, default = ""))
    if (!(override %in% .rb_supported_topologies())) {
      stop("Manual topology override must be one of A0-A7.", call. = FALSE)
    }
    return(override)
  }

  long <- isTRUE(is_longitudinal)
  multi <- identical(.rb_scalar_text(n_domain_bucket, default = "1"), "2+")

  if (long && multi) {
    return("A7")
  }

  level <- .rb_scalar_text(check_kind, default = "")
  if (!nzchar(level)) {
    stop("Check type is required to resolve topology.", call. = FALSE)
  }

  choose_topology(
    is_longitudinal = long,
    n_domains = .rb_guided_n_domains(n_domain_bucket),
    level = level
  )
}

.rb_guided_parse_csv <- function(x) {
  .rb_split_csv_values(x)
}

.rb_guided_next_rule_id <- function(rules, prefix = "RB") {
  prefix <- toupper(.rb_scalar_text(prefix, default = "RB"))
  if (!nzchar(prefix)) {
    prefix <- "RB"
  }

  if (is.null(rules) || !length(rules)) {
    return(paste0(prefix, "-001"))
  }

  ids <- vapply(
    rules,
    function(rule) .rb_scalar_text(rule$id, default = ""),
    character(1)
  )

  pattern <- paste0("^", prefix, "-([0-9]+)$")
  captures <- regmatches(ids, regexec(pattern, ids))
  numeric_parts <- suppressWarnings(
    as.integer(vapply(captures, function(hit) if (length(hit) >= 2L) hit[[2]] else NA_character_, character(1)))
  )

  next_n <- if (all(is.na(numeric_parts))) {
    length(ids) + 1L
  } else {
    max(numeric_parts, na.rm = TRUE) + 1L
  }

  paste0(prefix, "-", sprintf("%03d", next_n))
}

.rb_guided_build_rule_candidate <- function(draft,
                                            pack,
                                            domain_registry = list()) {
  if (!is.list(draft)) {
    stop("`draft` must be a list.", call. = FALSE)
  }

  topology <- .rb_guided_resolve_topology(
    is_longitudinal = isTRUE(draft$is_longitudinal),
    n_domain_bucket = .rb_scalar_text(draft$n_domain_bucket, default = "1"),
    check_kind = .rb_scalar_text(draft$check_kind, default = ""),
    topology_override_used = isTRUE(draft$topology_override_used),
    topology_override = .rb_scalar_text(draft$topology_override, default = "A1")
  )

  primary_domain <- .rb_scalar_text(draft$primary_domain, default = "")
  cross_domains <- .rb_guided_parse_csv(draft$domains_text)

  domain <- primary_domain
  domains <- cross_domains

  if (topology %in% c("A0", "A1", "A2", "A5", "A6")) {
    if (!nzchar(domain) && length(domains)) {
      domain <- domains[[1]]
    }
    domains <- character()
  } else {
    domains <- unique(c(domains, if (nzchar(domain)) domain else character()))
    domain <- ""
  }

  optional_args <- list()
  keys <- .rb_guided_parse_csv(draft$opt_keys)
  vars <- .rb_guided_parse_csv(draft$opt_vars)
  if (length(keys)) optional_args$keys <- keys
  if (length(vars)) optional_args$vars <- vars
  if (!is.null(draft$opt_threshold) && length(draft$opt_threshold) &&
      !is.na(as.numeric(draft$opt_threshold[[1]]))) {
    optional_args$threshold <- as.numeric(draft$opt_threshold[[1]])
  }
  if (!is.null(draft$opt_window_days) && length(draft$opt_window_days) &&
      !is.na(as.numeric(draft$opt_window_days[[1]]))) {
    optional_args$window_days <- as.numeric(draft$opt_window_days[[1]])
  }

  misc_json <- .rb_scalar_text(draft$opt_misc_json, default = "")
  if (nzchar(misc_json)) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      stop("Package `jsonlite` is required for misc JSON parsing.", call. = FALSE)
    }
    parsed <- tryCatch(
      jsonlite::fromJSON(misc_json, simplifyVector = FALSE),
      error = function(e) e
    )
    if (inherits(parsed, "error")) {
      stop("Invalid misc JSON payload.", call. = FALSE)
    }
    if (is.list(parsed)) {
      for (name in names(parsed)) {
        optional_args[[name]] <- parsed[[name]]
      }
    }
  }

  mapped_domains <- if (length(domains)) domains else if (nzchar(domain)) domain else character()
  mapped_columns <- setNames(vector("list", length(mapped_domains)), mapped_domains)
  if (length(mapped_domains) && length(domain_registry)) {
    for (domain_name in mapped_domains) {
      if (domain_name %in% names(domain_registry)) {
        mapped_columns[[domain_name]] <- domain_registry[[domain_name]]
      } else {
        mapped_columns[[domain_name]] <- character()
      }
    }
  }

  decision_level <- if (isTRUE(draft$is_longitudinal) &&
      identical(.rb_scalar_text(draft$n_domain_bucket, default = "1"), "2+")) {
    "record"
  } else {
    .rb_scalar_text(draft$check_kind, default = "")
  }

  list(
    id = .rb_scalar_text(draft$rule_id, default = ""),
    title = .rb_scalar_text(draft$title, default = ""),
    topology = topology,
    scope = .rb_scalar_text(draft$scope, default = .rb_scalar_text(pack$default_scope, default = "record")),
    intent = .rb_scalar_text(draft$intent, default = .rb_scalar_text(pack$default_intent, default = "quality_screening")),
    severity = .rb_scalar_text(draft$severity, default = .rb_scalar_text(pack$default_severity, default = "warning")),
    owner_team = .rb_scalar_text(draft$owner_team, default = .rb_scalar_text(pack$owner_team, default = "Data Operations")),
    domain = domain,
    domains = domains,
    decision = list(
      is_longitudinal = isTRUE(draft$is_longitudinal),
      n_domains = .rb_guided_n_domains(.rb_scalar_text(draft$n_domain_bucket, default = "1")),
      level = decision_level,
      topology_override_used = isTRUE(draft$topology_override_used)
    ),
    domain_mapping = list(
      domain = domain,
      domains = domains,
      columns = mapped_columns
    ),
    optional_args = optional_args,
    handoff = list(
      objective = .rb_scalar_text(draft$objective, default = ""),
      candidate_columns = .rb_guided_parse_csv(draft$candidate_columns),
      pseudo_logic = .rb_scalar_text(draft$pseudo_logic, default = ""),
      edge_cases = .rb_guided_parse_csv(draft$edge_cases)
    )
  )
}
