# Rule builder spec helpers ---------------------------------------------------
# These helpers intentionally keep data normalization explicit and verbose so
# the generated rule packs are easy to audit before code is written.

.rb_supported_topologies <- function() {
  c("A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7")
}

.rb_scope_values <- function() {
  c("record", "group", "snapshot")
}

.rb_scalar_text <- function(x, default = "") {
  if (is.null(x) || !length(x)) {
    return(default)
  }

  value <- as.character(x[[1]])
  if (is.na(value)) {
    return(default)
  }

  trimws(value)
}

.rb_non_empty_scalar <- function(x, field_name) {
  value <- .rb_scalar_text(x, default = "")
  if (!nzchar(value)) {
    stop("`", field_name, "` must be a non-empty string.", call. = FALSE)
  }
  value
}

.rb_normalize_chr <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  out <- as.character(x)
  out <- trimws(out[!is.na(out)])
  unique(out[nzchar(out)])
}

.rb_slugify <- function(x) {
  value <- tolower(.rb_scalar_text(x, default = "rule-pack"))
  value <- gsub("[^a-z0-9]+", "-", value)
  value <- gsub("(^-+|-+$)", "", value)
  value <- gsub("-{2,}", "-", value)

  if (!nzchar(value)) {
    "rule-pack"
  } else {
    value
  }
}

.rb_sanitize_function_name <- function(function_name, pack_name) {
  raw <- .rb_scalar_text(function_name, default = "")
  if (!nzchar(raw)) {
    raw <- paste0("make_rule_pack_", gsub("-", "_", .rb_slugify(pack_name)))
  }

  raw <- gsub("[^A-Za-z0-9_.]", "_", raw)
  raw <- make.names(raw, unique = FALSE)

  if (!grepl("^[A-Za-z.]", raw)) {
    raw <- paste0("make_rule_pack_", raw)
  }

  raw
}

.rb_parse_decision_shape <- function(rule, topology, domain, domains) {
  decision <- if (is.list(rule$decision)) rule$decision else list()
  inferred <- .rb_infer_decision_shape(topology, domain, domains)

  is_longitudinal <- decision$is_longitudinal
  if (is.null(is_longitudinal) || !length(is_longitudinal)) {
    is_longitudinal <- inferred$is_longitudinal
  }
  if (!is.logical(is_longitudinal) || length(is_longitudinal) != 1L || is.na(is_longitudinal)) {
    stop("Rule `", rule$id, "` has invalid `decision$is_longitudinal`.", call. = FALSE)
  }
  is_longitudinal <- isTRUE(is_longitudinal)

  n_domains <- decision$n_domains
  if (is.null(n_domains) || !length(n_domains)) {
    n_domains <- inferred$n_domains
  }
  if (!is.numeric(n_domains) || length(n_domains) != 1L || is.na(n_domains) ||
      n_domains < 1 || n_domains %% 1 != 0) {
    stop("Rule `", rule$id, "` has invalid `decision$n_domains`.", call. = FALSE)
  }
  n_domains <- as.integer(n_domains)

  level <- .rb_scalar_text(decision$level, default = inferred$level)
  if (!nzchar(level)) {
    stop("Rule `", rule$id, "` has invalid `decision$level`.", call. = FALSE)
  }

  override <- decision$topology_override_used
  if (is.null(override) || !length(override)) {
    override <- .rb_scalar_text(rule$topology_override_used, default = "FALSE")
    override <- identical(tolower(override), "true")
  }
  if (!is.logical(override) || length(override) != 1L || is.na(override)) {
    stop("Rule `", rule$id, "` has invalid `decision$topology_override_used`.", call. = FALSE)
  }
  override <- isTRUE(override)

  # When no manual override is used, the captured decision shape must map back
  # to the chosen topology exactly; this catches conflicting form state early.
  if (!override) {
    computed <- choose_topology(
      is_longitudinal = is_longitudinal,
      n_domains = n_domains,
      level = level
    )

    if (!identical(computed, topology)) {
      stop(
        "Rule `", rule$id, "` decision shape resolves to `", computed,
        "` but topology is `", topology, "`.",
        call. = FALSE
      )
    }
  }

  list(
    is_longitudinal = is_longitudinal,
    n_domains = n_domains,
    level = level,
    topology_override_used = override
  )
}

.rb_infer_decision_shape <- function(topology, domain, domains) {
  domain_count <- if (length(domains)) length(domains) else if (nzchar(domain)) 1L else 1L

  switch(
    topology,
    A0 = list(is_longitudinal = FALSE, n_domains = domain_count, level = "schema"),
    A1 = list(is_longitudinal = FALSE, n_domains = 1L, level = "record"),
    A2 = list(is_longitudinal = FALSE, n_domains = 1L, level = "aggregate"),
    A3 = list(is_longitudinal = FALSE, n_domains = max(2L, domain_count), level = "relational"),
    A4 = list(is_longitudinal = FALSE, n_domains = max(2L, domain_count), level = "aggregate"),
    A5 = list(is_longitudinal = TRUE, n_domains = 1L, level = "record"),
    A6 = list(is_longitudinal = TRUE, n_domains = 1L, level = "aggregate"),
    A7 = list(is_longitudinal = TRUE, n_domains = max(2L, domain_count), level = "record"),
    stop("Unsupported topology `", topology, "`.", call. = FALSE)
  )
}

.rb_normalize_domain_columns <- function(columns) {
  if (is.null(columns)) {
    return(list())
  }

  if (!is.list(columns)) {
    stop("`domain_columns` must be a named list.", call. = FALSE)
  }

  nm <- names(columns)
  if (is.null(nm)) {
    stop("`domain_columns` must be a named list.", call. = FALSE)
  }

  out <- list()
  for (name in nm) {
    clean_name <- trimws(name)
    if (!nzchar(clean_name)) {
      next
    }
    out[[clean_name]] <- .rb_normalize_chr(columns[[name]])
  }
  out
}

.rb_validate_rule <- function(rule) {
  if (!nzchar(rule$id)) {
    stop("Rule IDs must be non-empty.", call. = FALSE)
  }

  if (!(rule$topology %in% .rb_supported_topologies())) {
    stop("Rule `", rule$id, "` has invalid topology `", rule$topology, "`.", call. = FALSE)
  }

  if (!(rule$scope %in% .rb_scope_values())) {
    stop("Rule `", rule$id, "` has invalid scope `", rule$scope, "`.", call. = FALSE)
  }

  if (!nzchar(rule$intent)) {
    stop("Rule `", rule$id, "` must define a non-empty intent.", call. = FALSE)
  }

  if (rule$topology %in% c("A0", "A1", "A2", "A5", "A6") && !nzchar(rule$domain)) {
    stop("Rule `", rule$id, "` requires `domain` for topology `", rule$topology, "`.", call. = FALSE)
  }

  if (rule$topology %in% c("A3", "A4", "A7") && length(rule$domains) < 2L) {
    stop("Rule `", rule$id, "` requires at least two entries in `domains`.", call. = FALSE)
  }
}

.rb_normalize_rule <- function(rule, pack_defaults, idx) {
  if (!is.list(rule)) {
    stop("`rules[[", idx, "]]` must be a list.", call. = FALSE)
  }

  rule_id <- .rb_non_empty_scalar(rule$id, paste0("rules[[", idx, "]]$id"))
  title <- .rb_scalar_text(rule$title, default = rule_id)
  topology <- toupper(.rb_scalar_text(rule$topology, default = ""))

  domain_mapping <- if (is.list(rule$domain_mapping)) rule$domain_mapping else list()

  domain <- .rb_scalar_text(rule$domain, default = .rb_scalar_text(domain_mapping$domain, default = ""))

  domains <- .rb_normalize_chr(rule$domains)
  if (!length(domains)) {
    domains <- .rb_normalize_chr(domain_mapping$domains)
  }

  # Single-domain rules frequently arrive with only `domains = "X"` from form
  # controls. Promote to `domain` so downstream constructors stay consistent.
  if (!nzchar(domain) && length(domains) == 1L) {
    domain <- domains[[1]]
  }

  if (!nzchar(topology)) {
    decision <- if (is.list(rule$decision)) rule$decision else list()
    topology <- choose_topology(
      is_longitudinal = decision$is_longitudinal,
      n_domains = decision$n_domains,
      level = decision$level
    )
  }

  if (topology %in% c("A3", "A4", "A7")) {
    domains <- unique(c(domains, if (nzchar(domain)) domain else character()))
  }

  scope <- tolower(.rb_scalar_text(rule$scope, default = "record"))
  intent <- .rb_scalar_text(rule$intent, default = pack_defaults$default_intent)
  severity <- .rb_scalar_text(rule$severity, default = pack_defaults$default_severity)
  owner_team <- .rb_scalar_text(rule$owner_team, default = pack_defaults$owner_team)

  optional_args <- rule$optional_args
  if (is.null(optional_args)) {
    optional_args <- rule$args
  }
  if (is.null(optional_args)) {
    optional_args <- list()
  }
  if (!is.list(optional_args)) {
    stop("Rule `", rule_id, "` must provide `optional_args` as a list.", call. = FALSE)
  }

  handoff <- if (is.list(rule$handoff)) rule$handoff else list()
  objective <- .rb_scalar_text(handoff$objective, default = .rb_scalar_text(rule$objective, default = ""))
  candidate_columns <- .rb_normalize_chr(c(handoff$candidate_columns, rule$candidate_columns))
  pseudo_logic <- .rb_scalar_text(handoff$pseudo_logic, default = .rb_scalar_text(rule$pseudo_logic, default = ""))
  edge_cases <- .rb_normalize_chr(c(handoff$edge_cases, rule$edge_cases))

  domain_columns <- .rb_normalize_domain_columns(domain_mapping$columns %||% rule$domain_columns)

  rule_for_decision <- rule
  rule_for_decision$id <- rule_id

  normalized <- list(
    id = rule_id,
    title = title,
    topology = topology,
    scope = scope,
    intent = intent,
    severity = severity,
    owner_team = owner_team,
    domain = domain,
    domains = domains,
    decision = .rb_parse_decision_shape(
      rule = rule_for_decision,
      topology = topology,
      domain = domain,
      domains = domains
    ),
    domain_mapping = list(
      domain = domain,
      domains = domains,
      columns = domain_columns
    ),
    optional_args = optional_args,
    handoff = list(
      objective = objective,
      candidate_columns = candidate_columns,
      pseudo_logic = pseudo_logic,
      edge_cases = edge_cases
    )
  )

  .rb_validate_rule(normalized)
  normalized
}

.rb_validate_spec <- function(spec) {
  if (!is.list(spec$rules)) {
    stop("`spec$rules` must be a list.", call. = FALSE)
  }

  ids <- vapply(spec$rules, function(rule) rule$id, character(1))
  if (length(ids) && anyDuplicated(ids)) {
    dupes <- unique(ids[duplicated(ids)])
    stop("Rule IDs must be unique. Duplicates: ", paste(dupes, collapse = ", "), ".", call. = FALSE)
  }

  invisible(TRUE)
}

# Workspace helpers -----------------------------------------------------------
# Kept in non-exported helpers so the app can stay focused on UI wiring.

.rb_upsert_workspace_rule <- function(rules, rule, edit_id = NULL) {
  if (is.null(rules)) {
    rules <- list()
  }
  if (!is.list(rules)) {
    stop("`rules` must be a list.", call. = FALSE)
  }
  if (!is.list(rule) || !nzchar(.rb_scalar_text(rule$id, default = ""))) {
    stop("`rule` must be a list with a non-empty `id`.", call. = FALSE)
  }

  rule_id <- .rb_scalar_text(rule$id, default = "")
  existing_ids <- vapply(rules, function(item) .rb_scalar_text(item$id, default = ""), character(1))

  if (is.null(edit_id) || !nzchar(.rb_scalar_text(edit_id, default = ""))) {
    if (rule_id %in% existing_ids) {
      stop("Rule ID `", rule_id, "` already exists.", call. = FALSE)
    }
    return(c(rules, list(rule)))
  }

  edit_id <- .rb_scalar_text(edit_id, default = "")
  idx <- match(edit_id, existing_ids)
  if (is.na(idx)) {
    stop("Cannot edit unknown rule ID `", edit_id, "`.", call. = FALSE)
  }

  conflicting <- setdiff(existing_ids, edit_id)
  if (rule_id %in% conflicting) {
    stop("Rule ID `", rule_id, "` already exists.", call. = FALSE)
  }

  rules[[idx]] <- rule
  rules
}

.rb_delete_workspace_rule <- function(rules, rule_id) {
  if (is.null(rules) || !length(rules)) {
    return(list())
  }
  if (!is.list(rules)) {
    stop("`rules` must be a list.", call. = FALSE)
  }

  keep <- vapply(
    rules,
    function(item) !identical(.rb_scalar_text(item$id, default = ""), .rb_scalar_text(rule_id, default = "")),
    logical(1)
  )

  rules[keep]
}

.rb_move_workspace_rule <- function(rules, rule_id, direction = c("up", "down")) {
  direction <- match.arg(direction)
  if (!is.list(rules) || !length(rules)) {
    return(rules)
  }

  ids <- vapply(rules, function(item) .rb_scalar_text(item$id, default = ""), character(1))
  idx <- match(.rb_scalar_text(rule_id, default = ""), ids)
  if (is.na(idx)) {
    return(rules)
  }

  swap_idx <- if (direction == "up") idx - 1L else idx + 1L
  if (swap_idx < 1L || swap_idx > length(rules)) {
    return(rules)
  }

  tmp <- rules[[idx]]
  rules[[idx]] <- rules[[swap_idx]]
  rules[[swap_idx]] <- tmp
  rules
}

# CSV helpers -----------------------------------------------------------------

.rb_read_csv_header <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) {
    stop("CSV file does not exist.", call. = FALSE)
  }

  first_line <- readLines(path, n = 1L, warn = FALSE)
  if (!length(first_line) || !nzchar(trimws(first_line[[1]]))) {
    stop("CSV must include a header row.", call. = FALSE)
  }

  header_tokens <- strsplit(first_line[[1]], ",", fixed = TRUE)[[1]]
  header_tokens <- trimws(gsub('^"|"$', "", header_tokens))
  if (!length(header_tokens) || any(!nzchar(header_tokens))) {
    stop("CSV must include a header row.", call. = FALSE)
  }

  # Numeric-only first row is a strong signal that no header was provided.
  if (all(grepl("^[-+]?([0-9]*\\.[0-9]+|[0-9]+)$", header_tokens))) {
    stop("CSV must include a header row.", call. = FALSE)
  }

  header <- tryCatch(
    utils::read.csv(
      path,
      nrows = 0L,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(header) || !length(names(header))) {
    stop("CSV must include a header row.", call. = FALSE)
  }

  .rb_normalize_chr(names(header))
}

.rb_split_csv_values <- function(text) {
  raw <- .rb_scalar_text(text, default = "")
  if (!nzchar(raw)) {
    return(character())
  }

  .rb_normalize_chr(strsplit(raw, ",", fixed = TRUE)[[1]])
}

#' Build a structured rule-pack specification
#'
#' Normalizes and validates pack metadata plus a rule workspace into a list
#' shape that can be rendered as YAML, R scaffolds, and agent prompt bundles.
#'
#' @param pack Named list with pack fields (`pack_name`, `function_name`,
#'   `owner_team`, `default_severity`, `default_intent`, `notes`).
#' @param rules List of rule entries captured from the funnel workspace.
#' @param validate Logical scalar; when `TRUE`, run all validation checks.
#'
#' @return A list with class `rule_pack_spec` containing `pack` and `rules`.
#' @export
build_rule_pack_spec <- function(pack, rules = list(), validate = TRUE) {
  if (missing(pack) || is.null(pack)) {
    pack <- list()
  }
  if (!is.list(pack)) {
    stop("`pack` must be a named list.", call. = FALSE)
  }
  if (!is.list(rules)) {
    stop("`rules` must be a list.", call. = FALSE)
  }

  pack_name <- .rb_non_empty_scalar(pack$pack_name, "pack$pack_name")
  pack_slug <- .rb_slugify(pack_name)
  function_name <- .rb_sanitize_function_name(pack$function_name, pack_name)

  pack_defaults <- list(
    owner_team = .rb_scalar_text(pack$owner_team, default = "Data Operations"),
    default_severity = .rb_scalar_text(pack$default_severity, default = "warning"),
    default_intent = .rb_scalar_text(pack$default_intent, default = "quality_screening"),
    notes = .rb_scalar_text(pack$notes, default = "")
  )

  normalized_rules <- lapply(seq_along(rules), function(idx) {
    .rb_normalize_rule(
      rule = rules[[idx]],
      pack_defaults = pack_defaults,
      idx = idx
    )
  })

  spec <- list(
    spec_version = "1.0",
    pack = list(
      pack_name = pack_name,
      pack_slug = pack_slug,
      function_name = function_name,
      owner_team = pack_defaults$owner_team,
      default_severity = pack_defaults$default_severity,
      default_intent = pack_defaults$default_intent,
      notes = pack_defaults$notes,
      generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
    ),
    rules = normalized_rules
  )
  class(spec) <- c("rule_pack_spec", "list")

  if (isTRUE(validate)) {
    .rb_validate_spec(spec)
  }

  spec
}
