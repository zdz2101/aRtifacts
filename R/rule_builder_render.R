# Rule builder rendering helpers ----------------------------------------------
# Rendering is intentionally deterministic so users can diff generated output
# and keep hand-edited follow-up changes reviewable.

.rb_assert_spec <- function(spec) {
  if (!is.list(spec) || is.null(spec$pack) || is.null(spec$rules)) {
    stop("`spec` must be a list returned by `build_rule_pack_spec()`.", call. = FALSE)
  }

  .rb_validate_spec(spec)
  invisible(TRUE)
}

.rb_r_string <- function(x) {
  value <- .rb_scalar_text(x, default = "")
  value <- gsub("\\\\", "\\\\\\\\", value)
  value <- gsub("\"", "\\\\\"", value)
  paste0("\"", value, "\"")
}

.rb_r_chr_vector <- function(values) {
  values <- .rb_normalize_chr(values)
  if (!length(values)) {
    return("character()")
  }
  paste0("c(", paste(vapply(values, .rb_r_string, character(1)), collapse = ", "), ")")
}

.rb_topology_fn_signature <- function(topology) {
  switch(
    topology,
    A0 = "function(domain, context, rule)",
    A1 = "function(data, context, rule)",
    A2 = "function(data, context, rule)",
    A3 = "function(domains, context, rule)",
    A4 = "function(domains, context, rule)",
    A5 = "function(previous, current, context, rule)",
    A6 = "function(previous, current, context, rule)",
    A7 = "function(previous, current, context, rule)",
    stop("Unsupported topology `", topology, "`.", call. = FALSE)
  )
}

.rb_topology_payload_contract <- function(topology) {
  switch(
    topology,
    A0 = "Payload: `domain` is a `DomainSnapshot` scoped to one current-cut domain.",
    A1 = "Payload: `data` is one current-cut domain `data.frame`.",
    A2 = "Payload: `data` is one current-cut domain `data.frame`.",
    A3 = "Payload: `domains` is a named list of current-cut domain `data.frame`s.",
    A4 = "Payload: `domains` is a named list of current-cut domain `data.frame`s.",
    A5 = "Payload: `previous` and `current` are same-domain `data.frame`s across cuts.",
    A6 = "Payload: `previous` and `current` are same-domain `data.frame`s across cuts.",
    A7 = "Payload: `previous` and `current` are named lists of cross-domain `data.frame`s.",
    stop("Unsupported topology `", topology, "`.", call. = FALSE)
  )
}

.rb_optional_args_comment <- function(optional_args, indent = "      ") {
  if (!length(optional_args)) {
    return(character())
  }

  lines <- c(paste0(indent, "# Optional args captured from funnel inputs:"))
  for (name in names(optional_args)) {
    value <- paste(capture.output(str(optional_args[[name]], give.attr = FALSE, vec.len = 8L)), collapse = " ")
    lines <- c(lines, paste0(indent, "# - ", name, ": ", value))
  }
  lines
}

.rb_handoff_comment <- function(rule, indent = "        ") {
  handoff <- rule$handoff
  objective <- .rb_scalar_text(handoff$objective, default = "")
  pseudo <- .rb_scalar_text(handoff$pseudo_logic, default = "")
  candidates <- .rb_normalize_chr(handoff$candidate_columns)
  edge_cases <- .rb_normalize_chr(handoff$edge_cases)

  lines <- character()
  if (nzchar(objective)) {
    lines <- c(lines, paste0(indent, "# Objective: ", objective))
  }
  if (length(candidates)) {
    lines <- c(lines, paste0(indent, "# Candidate columns: ", paste(candidates, collapse = ", ")))
  }
  if (nzchar(pseudo)) {
    pseudo_lines <- strsplit(pseudo, "\n", fixed = TRUE)[[1]]
    lines <- c(lines, paste0(indent, "# Pseudo-logic: ", pseudo_lines[[1]]))
    if (length(pseudo_lines) > 1L) {
      lines <- c(lines, paste0(indent, "#   ", pseudo_lines[-1L]))
    }
  }
  if (length(edge_cases)) {
    lines <- c(lines, paste0(indent, "# Edge cases requested: ", paste(edge_cases, collapse = ", ")))
  }
  lines
}

.rb_render_scaffold_rule <- function(rule, pack_owner_team, append_comma = FALSE) {
  lines <- c(
    paste0("    # ", rule$id, ": ", rule$title),
    "    new_rule(",
    paste0("      id = ", .rb_r_string(rule$id), ","),
    paste0("      title = ", .rb_r_string(rule$title), ","),
    paste0("      topology = ", .rb_r_string(rule$topology), ","),
    paste0("      scope = ", .rb_r_string(rule$scope), ","),
    paste0("      intent = ", .rb_r_string(rule$intent), ",")
  )

  if (nzchar(rule$domain)) {
    lines <- c(lines, paste0("      domain = ", .rb_r_string(rule$domain), ","))
  }
  if (length(rule$domains)) {
    lines <- c(lines, paste0("      domains = ", .rb_r_chr_vector(rule$domains), ","))
  }

  owner_team_line <- if (identical(rule$owner_team, pack_owner_team)) {
    "      owner_team = owner_team,"
  } else {
    paste0("      owner_team = ", .rb_r_string(rule$owner_team), ",")
  }

  lines <- c(
    lines,
    paste0("      severity = ", .rb_r_string(rule$severity), ","),
    owner_team_line,
    .rb_optional_args_comment(rule$optional_args, indent = "      "),
    paste0("      fn = ", .rb_topology_fn_signature(rule$topology), " {"),
    .rb_handoff_comment(rule, indent = "        "),
    "        # TODO: implement rule logic.",
    "        # Return a data.frame with finding-level context columns.",
    "        data.frame()",
    "      }",
    if (isTRUE(append_comma)) "    )," else "    )"
  )

  lines
}

.rb_spec_yaml <- function(spec) {
  .rb_assert_spec(spec)
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package `yaml` is required to render YAML output.", call. = FALSE)
  }

  yaml::as.yaml(unclass(spec))
}

#' Render rule-pack scaffold source
#'
#' Generates a prefilled R scaffold with `new_rule(...)` blocks and topology-
#' specific `fn` signatures. Rule logic remains intentionally TODO-only.
#'
#' @param spec Rule-pack spec from `build_rule_pack_spec()`.
#'
#' @return A single character string containing R source.
#' @export
render_rule_pack_scaffold <- function(spec) {
  .rb_assert_spec(spec)

  function_name <- .rb_scalar_text(spec$pack$function_name, default = "")
  if (!nzchar(function_name)) {
    function_name <- paste0("make_rule_pack_", .rb_scalar_text(spec$pack$pack_slug, default = "rule_pack"))
  }

  lines <- c(
    "# Auto-generated by aRtifacts::launch_rule_builder().",
    "# Keep metadata synchronized with rule-pack.spec.yaml while filling TODOs.",
    "",
    paste0(function_name, " <- function(owner_team = ", .rb_r_string(spec$pack$owner_team), ") {"),
    "  list()",
    "}"
  )

  if (length(spec$rules)) {
    lines <- c(
      "# Auto-generated by aRtifacts::launch_rule_builder().",
      "# Keep metadata synchronized with rule-pack.spec.yaml while filling TODOs.",
      "",
      paste0(function_name, " <- function(owner_team = ", .rb_r_string(spec$pack$owner_team), ") {"),
      "  list("
    )

    for (idx in seq_along(spec$rules)) {
      lines <- c(
        lines,
        .rb_render_scaffold_rule(
          rule = spec$rules[[idx]],
          pack_owner_team = spec$pack$owner_team,
          append_comma = idx < length(spec$rules)
        )
      )
    }

    lines <- c(lines, "  )", "}")
  }

  paste(lines, collapse = "\n")
}

#' Render agent prompt bundle for rule implementation handoff
#'
#' Produces Markdown that captures topology contracts, metadata, domain context,
#' and handoff notes for human or agent-assisted implementation.
#'
#' @param spec Rule-pack spec from `build_rule_pack_spec()`.
#'
#' @return A single character string containing Markdown.
#' @export
render_rule_agent_prompt_bundle <- function(spec) {
  .rb_assert_spec(spec)

  lines <- c(
    paste0("# Agent Prompt Bundle: ", spec$pack$pack_name),
    "",
    "## Pack Summary",
    paste0("- Pack name: `", spec$pack$pack_name, "`"),
    paste0("- Pack constructor: `", spec$pack$function_name, "()`"),
    paste0("- Default owner team: `", spec$pack$owner_team, "`"),
    paste0("- Default severity: `", spec$pack$default_severity, "`"),
    paste0("- Default intent: `", spec$pack$default_intent, "`")
  )

  if (nzchar(.rb_scalar_text(spec$pack$notes, default = ""))) {
    lines <- c(lines, paste0("- Notes: ", spec$pack$notes))
  }

  if (!length(spec$rules)) {
    lines <- c(lines, "", "_No rules currently defined in this spec._")
    return(paste(lines, collapse = "\n"))
  }

  for (rule in spec$rules) {
    lines <- c(
      lines,
      "",
      paste0("## ", rule$id, " - ", rule$title),
      paste0("- Topology: `", rule$topology, "`"),
      paste0("- Scope: `", rule$scope, "`"),
      paste0("- Intent: `", rule$intent, "`"),
      paste0("- Severity: `", rule$severity, "`"),
      paste0("- Owner team: `", rule$owner_team, "`"),
      paste0("- Contract: `", .rb_topology_fn_signature(rule$topology), "`"),
      paste0("- ", .rb_topology_payload_contract(rule$topology))
    )

    if (nzchar(rule$domain)) {
      lines <- c(lines, paste0("- Primary domain: `", rule$domain, "`"))
    }
    if (length(rule$domains)) {
      lines <- c(lines, paste0("- Domains: `", paste(rule$domains, collapse = "`, `"), "`"))
    }

    if (length(rule$domain_mapping$columns)) {
      lines <- c(lines, "- Domain columns:")
      for (domain_name in names(rule$domain_mapping$columns)) {
        cols <- rule$domain_mapping$columns[[domain_name]]
        if (!length(cols)) {
          lines <- c(lines, paste0("  - ", domain_name, ": _(no columns captured)_"))
        } else {
          lines <- c(lines, paste0("  - ", domain_name, ": `", paste(cols, collapse = "`, `"), "`"))
        }
      }
    }

    if (length(rule$optional_args)) {
      lines <- c(lines, "- Optional args:")
      for (arg_name in names(rule$optional_args)) {
        value <- paste(capture.output(str(rule$optional_args[[arg_name]], give.attr = FALSE, vec.len = 8L)), collapse = " ")
        lines <- c(lines, paste0("  - ", arg_name, ": ", value))
      }
    }

    lines <- c(lines, "", "### Objective")
    objective <- .rb_scalar_text(rule$handoff$objective, default = "")
    if (!nzchar(objective)) {
      objective <- "TODO: describe objective."
    }
    lines <- c(lines, objective)

    lines <- c(lines, "", "### Candidate Columns")
    candidates <- .rb_normalize_chr(rule$handoff$candidate_columns)
    if (!length(candidates)) {
      lines <- c(lines, "- TODO: provide candidate columns.")
    } else {
      lines <- c(lines, paste0("- `", candidates, "`"))
    }

    lines <- c(lines, "", "### Pseudo-Logic Notes")
    pseudo <- .rb_scalar_text(rule$handoff$pseudo_logic, default = "")
    if (!nzchar(pseudo)) {
      lines <- c(lines, "TODO: provide pseudo-logic notes.")
    } else {
      lines <- c(lines, pseudo)
    }

    lines <- c(lines, "", "### Edge Cases To Cover")
    edge_cases <- .rb_normalize_chr(rule$handoff$edge_cases)
    if (!length(edge_cases)) {
      lines <- c(lines, "- `0`")
      lines <- c(lines, "- blank/whitespace strings")
      lines <- c(lines, "- `NA` and missing columns/options/domains")
    } else {
      lines <- c(lines, paste0("- ", edge_cases))
    }
  }

  paste(lines, collapse = "\n")
}

.rb_safe_timestamp <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%S")
}

.rb_compose_output_dir <- function(output_dir, pack_slug, timestamp = NULL) {
  if (!is.character(output_dir) || length(output_dir) != 1L || !nzchar(trimws(output_dir))) {
    stop("`output_dir` must be a non-empty string path.", call. = FALSE)
  }

  stamp <- if (is.null(timestamp)) .rb_safe_timestamp() else .rb_scalar_text(timestamp, default = .rb_safe_timestamp())
  path <- file.path(output_dir, "rule-builder", pack_slug, stamp)

  # Guarantee no in-place overwrite; append suffix if a timestamp collision
  # occurs due to repeated generation in the same second.
  if (dir.exists(path)) {
    suffix <- format(Sys.time(), "%OS3")
    suffix <- gsub("[^0-9]", "", suffix)
    path <- paste0(path, "-", suffix)
  }

  path
}

.rb_write_rule_builder_outputs <- function(spec, output_dir, confirm = FALSE) {
  .rb_assert_spec(spec)
  if (!isTRUE(confirm)) {
    stop("Write action requires explicit confirmation.", call. = FALSE)
  }

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package `yaml` is required to write spec output.", call. = FALSE)
  }

  pack_slug <- .rb_scalar_text(spec$pack$pack_slug, default = "rule-pack")
  root <- .rb_compose_output_dir(output_dir = output_dir, pack_slug = pack_slug)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  spec_path <- file.path(root, "rule-pack.spec.yaml")
  scaffold_path <- file.path(root, paste0("rule_pack_", pack_slug, ".R"))
  prompt_path <- file.path(root, paste0("agent_prompt_rule_pack_", pack_slug, ".md"))

  writeLines(.rb_spec_yaml(spec), con = spec_path, useBytes = TRUE)
  writeLines(render_rule_pack_scaffold(spec), con = scaffold_path, useBytes = TRUE)
  writeLines(render_rule_agent_prompt_bundle(spec), con = prompt_path, useBytes = TRUE)

  list(
    output_dir = root,
    spec_file = spec_path,
    scaffold_file = scaffold_path,
    prompt_file = prompt_path
  )
}
