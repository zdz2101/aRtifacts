#' Rule context S7 class
#'
#' Execution context used by topology-dispatched rules.
#'
#' @param current Current `ArtifactSnapshot`.
#' @param previous Optional previous `ArtifactSnapshot`.
#' @param options Named list of runtime options (thresholds, tolerances, etc.).
#'
#' @name RuleContext
#' @return An S7 class object.
#' @export
RuleContext <- S7::new_class(
  "RuleContext",
  properties = list(
    current = S7::class_any,
    previous = S7::class_any,
    options = S7::class_list
  ),
  validator = function(self) {
    issues <- character()

    if (!S7::S7_inherits(self@current, ArtifactSnapshot)) {
      issues <- c(issues, "`current` must be an ArtifactSnapshot.")
    }

    if (!is.null(self@previous) && !S7::S7_inherits(self@previous, ArtifactSnapshot)) {
      issues <- c(issues, "`previous` must be NULL or an ArtifactSnapshot.")
    }

    if (length(issues)) issues else NULL
  }
)

#' Identity spec S7 class
#'
#' Defines identity alignment logic for cross-snapshot comparisons.
#'
#' @param keys Character vector of identity keys.
#' @param method Matching strategy (`"exact"`, `"hash"`, `"fuzzy"`).
#' @param hash_var Optional hash column used when `method = "hash"`.
#'
#' @name IdentitySpec
#' @return An S7 class object.
#' @export
IdentitySpec <- S7::new_class(
  "IdentitySpec",
  properties = list(
    keys = S7::class_character,
    method = S7::class_character,
    hash_var = S7::class_character
  ),
  validator = function(self) {
    issues <- character()

    if (!length(self@keys)) {
      issues <- c(issues, "`keys` must contain at least one key variable.")
    }

    if (!(self@method %in% c("exact", "hash", "fuzzy"))) {
      issues <- c(issues, "`method` must be one of: exact, hash, fuzzy.")
    }

    if (length(issues)) issues else NULL
  }
)

#' Match spec S7 class
#'
#' Defines cross-domain matching logic within a snapshot.
#'
#' @param by Character vector of join keys.
#' @param join_type Join type (`"inner"`, `"left"`, `"anti"`, `"semi"`).
#' @param window_days Optional time-window in days for temporal joins.
#'
#' @name MatchSpec
#' @return An S7 class object.
#' @export
MatchSpec <- S7::new_class(
  "MatchSpec",
  properties = list(
    by = S7::class_character,
    join_type = S7::class_character,
    window_days = S7::class_numeric
  ),
  validator = function(self) {
    issues <- character()

    if (!length(self@by)) {
      issues <- c(issues, "`by` must contain at least one join key.")
    }

    if (!(self@join_type %in% c("inner", "left", "anti", "semi"))) {
      issues <- c(issues, "`join_type` must be one of: inner, left, anti, semi.")
    }

    if (length(self@window_days) != 1L || is.na(self@window_days) || self@window_days < 0) {
      issues <- c(issues, "`window_days` must be a non-negative numeric scalar.")
    }

    if (length(issues)) issues else NULL
  }
)

#' Delta spec S7 class
#'
#' Defines field-level comparison and tolerance for snapshot deltas.
#'
#' @param fields Character vector of fields to compare.
#' @param abs_tol Absolute tolerance for numeric comparisons.
#' @param rel_tol Relative tolerance for numeric comparisons.
#' @param method Comparison method (`"exact"`, `"tolerant"`).
#'
#' @name DeltaSpec
#' @return An S7 class object.
#' @export
DeltaSpec <- S7::new_class(
  "DeltaSpec",
  properties = list(
    fields = S7::class_character,
    abs_tol = S7::class_numeric,
    rel_tol = S7::class_numeric,
    method = S7::class_character
  ),
  validator = function(self) {
    issues <- character()

    if (!length(self@fields)) {
      issues <- c(issues, "`fields` must contain at least one field.")
    }

    if (length(self@abs_tol) != 1L || is.na(self@abs_tol) || self@abs_tol < 0) {
      issues <- c(issues, "`abs_tol` must be a non-negative numeric scalar.")
    }

    if (length(self@rel_tol) != 1L || is.na(self@rel_tol) || self@rel_tol < 0) {
      issues <- c(issues, "`rel_tol` must be a non-negative numeric scalar.")
    }

    if (!(self@method %in% c("exact", "tolerant"))) {
      issues <- c(issues, "`method` must be one of: exact, tolerant.")
    }

    if (length(issues)) issues else NULL
  }
)

#' Core rule S7 class
#'
#' Topology-aware rule definition used by the unified topology dispatcher.
#'
#' @param id Stable rule ID (for example, `"SCH-001"`).
#' @param title Short human-readable title.
#' @param topology Topology tag (`A0` to `A7`).
#' @param scope Scope tag (`record`, `group`, `snapshot`).
#' @param intent Non-empty intent label describing why the rule exists.
#' @param domain Optional primary domain name.
#' @param domains Optional domain set used by cross-domain rules.
#' @param severity Severity label.
#' @param owner_team Owner team label.
#' @param identity_spec Optional `IdentitySpec`.
#' @param match_spec Optional `MatchSpec`.
#' @param delta_spec Optional `DeltaSpec`.
#' @param fn Evaluation function used by `run_rules()`.
#'
#' @name CoreRule
#' @return An S7 class object.
#' @export
CoreRule <- S7::new_class(
  "CoreRule",
  properties = list(
    id = S7::class_character,
    title = S7::class_character,
    topology = S7::class_character,
    scope = S7::class_character,
    intent = S7::class_character,
    domain = S7::class_character,
    domains = S7::class_character,
    severity = S7::class_character,
    owner_team = S7::class_character,
    identity_spec = S7::class_any,
    match_spec = S7::class_any,
    delta_spec = S7::class_any,
    fn = S7::class_any
  ),
  validator = function(self) {
    issues <- character()

    if (!(self@topology %in% c("A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7"))) {
      issues <- c(issues, "`topology` must be one of: A0, A1, A2, A3, A4, A5, A6, A7.")
    }

    if (!(self@scope %in% c("record", "group", "snapshot"))) {
      issues <- c(issues, "`scope` must be one of: record, group, snapshot.")
    }

    if (length(self@intent) != 1L || is.na(self@intent) || !nzchar(trimws(self@intent))) {
      issues <- c(issues, "`intent` must be a single non-empty character scalar.")
    }

    if (!is.function(self@fn)) {
      issues <- c(issues, "`fn` must be a function.")
    }

    if (!is.null(self@identity_spec) && !S7::S7_inherits(self@identity_spec, IdentitySpec)) {
      issues <- c(issues, "`identity_spec` must be NULL or an IdentitySpec.")
    }

    if (!is.null(self@match_spec) && !S7::S7_inherits(self@match_spec, MatchSpec)) {
      issues <- c(issues, "`match_spec` must be NULL or a MatchSpec.")
    }

    if (!is.null(self@delta_spec) && !S7::S7_inherits(self@delta_spec, DeltaSpec)) {
      issues <- c(issues, "`delta_spec` must be NULL or a DeltaSpec.")
    }

    if (self@topology %in% c("A1", "A2", "A5", "A6") && !nzchar(self@domain)) {
      issues <- c(
        issues,
        paste0(
          "`domain` is required for topology ", self@topology,
          ". Hint: single-domain topologies (A1/A2/A5/A6) require `domain = \"...\"`."
        )
      )
    }

    if (self@topology %in% c("A3", "A4", "A7") && length(self@domains) < 2L) {
      issues <- c(
        issues,
        paste0(
          "`domains` must contain at least two domain names for topology ", self@topology,
          ". Hint: cross-domain topologies (A3/A4/A7) require `domains = c(\"...\", \"...\")`."
        )
      )
    }

    if (self@topology != "A0" && !nzchar(self@domain) && !length(self@domains)) {
      issues <- c(issues, "Either `domain` or `domains` must be supplied.")
    }

    if (length(issues)) issues else NULL
  }
)

#' Create a rule context
#'
#' @param current Current `ArtifactSnapshot`.
#' @param previous Optional previous `ArtifactSnapshot`.
#' @param options Named list of runtime options.
#'
#' @return A validated `RuleContext` object.
#' @export
new_rule_context <- function(current, previous = NULL, options = list()) {
  RuleContext(current = current, previous = previous, options = options)
}

#' Create an identity spec
#'
#' @param keys Identity key columns.
#' @param method Matching method.
#' @param hash_var Optional hash column.
#'
#' @return A validated `IdentitySpec` object.
#' @export
new_identity_spec <- function(keys, method = "exact", hash_var = NA_character_) {
  IdentitySpec(keys = keys, method = method, hash_var = hash_var)
}

#' Create a match spec
#'
#' @param by Join keys.
#' @param join_type Join type.
#' @param window_days Optional non-negative time window.
#'
#' @return A validated `MatchSpec` object.
#' @export
new_match_spec <- function(by, join_type = "inner", window_days = 0) {
  MatchSpec(by = by, join_type = join_type, window_days = as.numeric(window_days))
}

#' Create a delta spec
#'
#' @param fields Fields to compare.
#' @param abs_tol Absolute tolerance.
#' @param rel_tol Relative tolerance.
#' @param method Comparison method.
#'
#' @return A validated `DeltaSpec` object.
#' @export
new_delta_spec <- function(fields, abs_tol = 0, rel_tol = 0, method = "exact") {
  DeltaSpec(
    fields = fields,
    abs_tol = as.numeric(abs_tol),
    rel_tol = as.numeric(rel_tol),
    method = method
  )
}

#' Create a topology-aware rule
#'
#' @param id Rule ID.
#' @param title Rule title.
#' @param topology Topology tag (`A0`-`A7`).
#' @param scope Scope tag.
#' @param intent Intent tag.
#' @param domain Optional primary domain.
#' @param domains Optional multi-domain set.
#' @param severity Severity label.
#' @param owner_team Owner team label.
#' @param identity_spec Optional identity spec.
#' @param match_spec Optional match spec.
#' @param delta_spec Optional delta spec.
#' @param fn Evaluation function.
#'
#' @return A validated `CoreRule` object.
#' @export
new_rule <- function(id,
                     title,
                     topology,
                     scope,
                     intent,
                     domain = "",
                     domains = character(),
                     severity = "warning",
                     owner_team = "Biostats",
                     identity_spec = NULL,
                     match_spec = NULL,
                     delta_spec = NULL,
                     fn) {
  topology <- as.character(topology)

  domain <- if (length(domain)) as.character(domain[[1]]) else ""
  if (is.na(domain)) {
    domain <- ""
  }
  domain <- trimws(domain)

  if (is.null(domains)) {
    domains <- character()
  }
  domains <- as.character(domains)
  domains <- unique(trimws(domains[!is.na(domains)]))
  domains <- domains[nzchar(domains)]

  if (topology %in% c("A1", "A2", "A5", "A6") && !nzchar(domain)) {
    stop(
      paste0(
        "`domain` is required for topology ", topology, ". ",
        "Hint: single-domain topologies (A1/A2/A5/A6) use `domain = \"...\"`. ",
        "If your rule spans multiple domains, use A3/A4/A7 with `domains = c(\"...\", \"...\")`."
      ),
      call. = FALSE
    )
  }

  if (topology %in% c("A3", "A4", "A7") && length(domains) < 2L) {
    hint <- if (topology == "A3") {
      "Try choose_topology(is_longitudinal = FALSE, n_domains = 2, level = \"relational\")."
    } else if (topology == "A4") {
      "Try choose_topology(is_longitudinal = FALSE, n_domains = 2, level = \"aggregate\")."
    } else {
      "Try choose_topology(is_longitudinal = TRUE, n_domains = 2, level = \"record\")."
    }

    stop(
      paste0(
        "`domains` must contain at least two domain names for topology ", topology, ". ",
        "Hint: cross-domain topologies (A3/A4/A7) use `domains = c(\"...\", \"...\")`. ",
        hint
      ),
      call. = FALSE
    )
  }

  CoreRule(
    id = id,
    title = title,
    topology = topology,
    scope = scope,
    intent = intent,
    domain = domain,
    domains = domains,
    severity = severity,
    owner_team = owner_team,
    identity_spec = identity_spec,
    match_spec = match_spec,
    delta_spec = delta_spec,
    fn = fn
  )
}
