mod_guided_builder_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::div(
      class = "rb-guided-shell",
      shiny::h2("Guided Rule Builder"),
      shiny::p(
        "One-way funnel: topology questions -> core metadata -> add rule ->",
        "repeat or finish."
      ),
      shiny::uiOutput(ns("guided_stepper")),
      shiny::uiOutput(ns("guided_step_body"))
    )
  )
}

mod_guided_builder_server <- function(id, workspace, dry_run = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    scalar_or <- function(x, default = "") {
      if (is.null(x) || !length(x)) return(default)
      value <- as.character(x[[1]])
      if (is.na(value)) return(default)
      value
    }

    numeric_or <- function(x, default = NA_real_) {
      if (is.null(x) || !length(x)) return(default)
      value <- suppressWarnings(as.numeric(x[[1]]))
      if (is.na(value)) return(default)
      value
    }

    ti <- function(id, ...) shiny::textInput(ns(id), ...)
    tai <- function(id, ...) shiny::textAreaInput(ns(id), ...)
    si <- function(id, ...) shiny::selectInput(ns(id), ...)
    ri <- function(id, ...) shiny::radioButtons(ns(id), ...)
    ni <- function(id, ...) shiny::numericInput(ns(id), ...)
    ci <- function(id, ...) shiny::checkboxInput(ns(id), ...)
    fi <- function(id, ...) shiny::fileInput(ns(id), ...)
    ab <- function(id, ...) shiny::actionButton(ns(id), ...)
    to <- function(id, ...) shiny::tableOutput(ns(id), ...)
    vo <- function(id, ...) shiny::verbatimTextOutput(ns(id), ...)

    resolve_internal_fn <- function(name, fallback) {
      ns_obj <- tryCatch(asNamespace("aRtifacts"), error = function(e) NULL)
      if (!is.null(ns_obj) && exists(name, envir = ns_obj, inherits = FALSE)) {
        return(get(name, envir = ns_obj, inherits = FALSE))
      }
      fallback
    }

    normalize_chr <- function(x) {
      if (is.null(x)) return(character())
      out <- as.character(x)
      out <- trimws(out[!is.na(out)])
      unique(out[nzchar(out)])
    }

    parse_csv <- function(x) {
      raw <- scalar_or(x, default = "")
      if (!nzchar(trimws(raw))) {
        return(character())
      }
      normalize_chr(strsplit(raw, ",", fixed = TRUE)[[1]])
    }

    guided_step_labels_fn <- resolve_internal_fn(
      ".rb_guided_step_labels",
      function() {
        c(
          "Pack Setup",
          "Topology Questions",
          "Rule Core Metadata",
          "Confirm + Add Rule",
          "Finish / Export"
        )
      }
    )

    guided_check_kind_choices_fn <- resolve_internal_fn(
      ".rb_guided_check_kind_choices",
      function(is_longitudinal, n_domain_bucket) {
        long <- isTRUE(is_longitudinal)
        multi <- identical(scalar_or(n_domain_bucket, default = "1"), "2+")

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

        character()
      }
    )

    guided_resolve_topology_fn <- resolve_internal_fn(
      ".rb_guided_resolve_topology",
      function(is_longitudinal,
               n_domain_bucket,
               check_kind = "",
               topology_override_used = FALSE,
               topology_override = "A1") {
        if (isTRUE(topology_override_used)) {
          override <- toupper(scalar_or(topology_override, default = ""))
          if (!(override %in% c("A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7"))) {
            stop("Manual topology override must be one of A0-A7.", call. = FALSE)
          }
          return(override)
        }

        long <- isTRUE(is_longitudinal)
        multi <- identical(scalar_or(n_domain_bucket, default = "1"), "2+")
        if (long && multi) {
          return("A7")
        }

        level <- scalar_or(check_kind, default = "")
        if (!nzchar(level)) {
          stop("Check type is required to resolve topology.", call. = FALSE)
        }

        aRtifacts::choose_topology(
          is_longitudinal = long,
          n_domains = if (multi) 2L else 1L,
          level = level
        )
      }
    )

    guided_next_rule_id_fn <- resolve_internal_fn(
      ".rb_guided_next_rule_id",
      function(rules, prefix = "RB") {
        prefix <- toupper(scalar_or(prefix, default = "RB"))
        if (!nzchar(prefix)) prefix <- "RB"

        if (is.null(rules) || !length(rules)) {
          return(paste0(prefix, "-001"))
        }

        ids <- vapply(rules, function(rule) scalar_or(rule$id, ""), character(1))
        pattern <- paste0("^", prefix, "-([0-9]+)$")
        captures <- regmatches(ids, regexec(pattern, ids))
        nums <- suppressWarnings(
          as.integer(vapply(captures, function(hit) if (length(hit) >= 2L) hit[[2]] else NA_character_, character(1)))
        )

        next_n <- if (all(is.na(nums))) length(ids) + 1L else max(nums, na.rm = TRUE) + 1L
        paste0(prefix, "-", sprintf("%03d", next_n))
      }
    )

    guided_build_rule_candidate_fn <- resolve_internal_fn(
      ".rb_guided_build_rule_candidate",
      function(draft, pack, domain_registry = list()) {
        topology <- guided_resolve_topology_fn(
          is_longitudinal = isTRUE(draft$is_longitudinal),
          n_domain_bucket = scalar_or(draft$n_domain_bucket, default = "1"),
          check_kind = scalar_or(draft$check_kind, default = ""),
          topology_override_used = isTRUE(draft$topology_override_used),
          topology_override = scalar_or(draft$topology_override, default = "A1")
        )

        primary_domain <- scalar_or(draft$primary_domain, default = "")
        cross_domains <- parse_csv(draft$domains_text)

        domain <- primary_domain
        domains <- cross_domains

        if (topology %in% c("A0", "A1", "A2", "A5", "A6")) {
          if (!nzchar(domain) && length(domains)) domain <- domains[[1]]
          domains <- character()
        } else {
          domains <- unique(c(domains, if (nzchar(domain)) domain else character()))
          domain <- ""
        }

        optional_args <- list()
        keys <- parse_csv(draft$opt_keys)
        vars <- parse_csv(draft$opt_vars)
        if (length(keys)) optional_args$keys <- keys
        if (length(vars)) optional_args$vars <- vars

        thr <- suppressWarnings(as.numeric(draft$opt_threshold))
        wnd <- suppressWarnings(as.numeric(draft$opt_window_days))
        if (length(thr) && !is.na(thr[[1]])) optional_args$threshold <- thr[[1]]
        if (length(wnd) && !is.na(wnd[[1]])) optional_args$window_days <- wnd[[1]]

        misc_json <- scalar_or(draft$opt_misc_json, default = "")
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
        for (domain_name in mapped_domains) {
          if (length(domain_registry) && domain_name %in% names(domain_registry)) {
            mapped_columns[[domain_name]] <- normalize_chr(domain_registry[[domain_name]])
          } else {
            mapped_columns[[domain_name]] <- character()
          }
        }

        decision_level <- if (isTRUE(draft$is_longitudinal) && identical(scalar_or(draft$n_domain_bucket, default = "1"), "2+")) {
          "record"
        } else {
          scalar_or(draft$check_kind, default = "")
        }

        list(
          id = scalar_or(draft$rule_id, default = ""),
          title = scalar_or(draft$title, default = ""),
          topology = topology,
          scope = scalar_or(draft$scope, default = scalar_or(pack$default_scope, default = "record")),
          intent = scalar_or(draft$intent, default = scalar_or(pack$default_intent, default = "quality_screening")),
          severity = scalar_or(draft$severity, default = scalar_or(pack$default_severity, default = "warning")),
          owner_team = scalar_or(draft$owner_team, default = scalar_or(pack$owner_team, default = "Data Operations")),
          domain = domain,
          domains = domains,
          decision = list(
            is_longitudinal = isTRUE(draft$is_longitudinal),
            n_domains = if (identical(scalar_or(draft$n_domain_bucket, default = "1"), "2+")) 2L else 1L,
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
            objective = scalar_or(draft$objective, default = ""),
            candidate_columns = parse_csv(draft$candidate_columns),
            pseudo_logic = scalar_or(draft$pseudo_logic, default = ""),
            edge_cases = parse_csv(draft$edge_cases)
          )
        )
      }
    )

    step_labels <- guided_step_labels_fn()

    rv <- shiny::reactiveValues(
      step = 1L,
      is_longitudinal = FALSE,
      n_domain_bucket = "1",
      check_kind = "record",
      topology_override_used = FALSE,
      topology_override = "A1",
      rule_id = "",
      title = "",
      objective = "",
      primary_domain = "",
      domains_text = "",
      edge_cases = "0, blank, whitespace, NA",
      scope = "record",
      intent = "quality_screening",
      severity = "warning",
      owner_team = "Data Operations",
      candidate_columns = "",
      pseudo_logic = "",
      opt_keys = "",
      opt_vars = "",
      opt_threshold = NA_real_,
      opt_window_days = NA_real_,
      opt_misc_json = "",
      output_dir = getwd(),
      quick_rule_id = "",
      quick_id = "",
      quick_title = "",
      quick_objective = "",
      quick_domain = "",
      quick_domains_text = "",
      quick_scope = "record",
      quick_intent = "quality_screening",
      quick_severity = "warning",
      quick_owner_team = "Data Operations"
    )

    preview_state <- shiny::reactiveVal(list(spec = NULL, error = NULL))

    # Pull reactive workspace output path only inside a reactive consumer.
    observeEvent(workspace$output_dir, {
      rv$output_dir <- scalar_or(workspace$output_dir, getwd())
    }, ignoreInit = FALSE)

    next_rule_id <- shiny::reactive({
      guided_next_rule_id_fn(workspace$rules, prefix = "RB")
    })

    reset_rule_draft <- function() {
      rv$is_longitudinal <- FALSE
      rv$n_domain_bucket <- "1"
      rv$check_kind <- "record"
      rv$topology_override_used <- FALSE
      rv$topology_override <- "A1"

      rv$rule_id <- next_rule_id()
      rv$title <- ""
      rv$objective <- ""
      rv$primary_domain <- ""
      rv$domains_text <- ""
      rv$edge_cases <- "0, blank, whitespace, NA"
      rv$scope <- "record"
      rv$intent <- scalar_or(workspace$pack$default_intent, "quality_screening")
      rv$severity <- scalar_or(workspace$pack$default_severity, "warning")
      rv$owner_team <- scalar_or(workspace$pack$owner_team, "Data Operations")
      rv$candidate_columns <- ""
      rv$pseudo_logic <- ""
      rv$opt_keys <- ""
      rv$opt_vars <- ""
      rv$opt_threshold <- NA_real_
      rv$opt_window_days <- NA_real_
      rv$opt_misc_json <- ""
    }

    capture_topology_inputs <- function() {
      rv$is_longitudinal <- identical(scalar_or(input$q_is_longitudinal, if (rv$is_longitudinal) "yes" else "no"), "yes")
      rv$n_domain_bucket <- scalar_or(input$q_n_domain_bucket, rv$n_domain_bucket)

      if (!(rv$is_longitudinal && identical(rv$n_domain_bucket, "2+"))) {
        rv$check_kind <- scalar_or(input$q_check_kind, rv$check_kind)
      } else {
        rv$check_kind <- "record"
      }
    }

    capture_rule_core_inputs <- function() {
      rv$rule_id <- scalar_or(input$rule_id, rv$rule_id)
      rv$title <- scalar_or(input$rule_title, rv$title)
      rv$objective <- scalar_or(input$rule_objective, rv$objective)
      rv$primary_domain <- scalar_or(input$rule_primary_domain, rv$primary_domain)
      rv$domains_text <- scalar_or(input$rule_domains_text, rv$domains_text)
      rv$edge_cases <- scalar_or(input$rule_edge_cases, rv$edge_cases)
      rv$scope <- scalar_or(input$rule_scope, rv$scope)
      rv$intent <- scalar_or(input$rule_intent, rv$intent)
      rv$severity <- scalar_or(input$rule_severity, rv$severity)
      rv$owner_team <- scalar_or(input$rule_owner_team, rv$owner_team)
      rv$candidate_columns <- scalar_or(input$rule_candidate_columns, rv$candidate_columns)
      rv$pseudo_logic <- scalar_or(input$rule_pseudo_logic, rv$pseudo_logic)
      rv$topology_override_used <- isTRUE(input$adv_topology_override_used)
      rv$topology_override <- scalar_or(input$adv_topology_override, rv$topology_override)
      rv$opt_keys <- scalar_or(input$adv_opt_keys, rv$opt_keys)
      rv$opt_vars <- scalar_or(input$adv_opt_vars, rv$opt_vars)
      rv$opt_threshold <- numeric_or(input$adv_opt_threshold, rv$opt_threshold)
      rv$opt_window_days <- numeric_or(input$adv_opt_window_days, rv$opt_window_days)
      rv$opt_misc_json <- scalar_or(input$adv_opt_misc_json, rv$opt_misc_json)
    }

    topology_ui_state <- shiny::reactive({
      is_longitudinal <- if (!is.null(input$q_is_longitudinal)) {
        identical(scalar_or(input$q_is_longitudinal, if (rv$is_longitudinal) "yes" else "no"), "yes")
      } else {
        rv$is_longitudinal
      }

      n_domain_bucket <- if (!is.null(input$q_n_domain_bucket)) {
        scalar_or(input$q_n_domain_bucket, rv$n_domain_bucket)
      } else {
        rv$n_domain_bucket
      }

      check_kind <- if (!is.null(input$q_check_kind)) {
        scalar_or(input$q_check_kind, rv$check_kind)
      } else {
        rv$check_kind
      }

      # Keep check kind aligned to the currently valid decision branch so
      # topology resolves immediately while users click through options.
      kind_choices <- guided_check_kind_choices_fn(
        is_longitudinal = is_longitudinal,
        n_domain_bucket = n_domain_bucket
      )
      if (length(kind_choices)) {
        valid_values <- unname(kind_choices)
        if (!(check_kind %in% valid_values)) {
          check_kind <- valid_values[[1]]
        }
      }

      if (isTRUE(is_longitudinal) && identical(n_domain_bucket, "2+")) {
        check_kind <- "record"
      }

      topology_override_used <- if (!is.null(input$adv_topology_override_used)) {
        isTRUE(input$adv_topology_override_used)
      } else {
        rv$topology_override_used
      }

      topology_override <- if (!is.null(input$adv_topology_override)) {
        scalar_or(input$adv_topology_override, rv$topology_override)
      } else {
        rv$topology_override
      }

      list(
        is_longitudinal = is_longitudinal,
        n_domain_bucket = n_domain_bucket,
        check_kind = check_kind,
        topology_override_used = topology_override_used,
        topology_override = topology_override
      )
    })

    # Persist live topology answers so rerendered UI never falls back to stale
    # values when dynamic controls are temporarily re-instantiated.
    observeEvent(
      list(
        input$q_is_longitudinal,
        input$q_n_domain_bucket,
        input$q_check_kind,
        input$adv_topology_override_used,
        input$adv_topology_override
      ),
      {
        state <- topology_ui_state()
        rv$is_longitudinal <- isTRUE(state$is_longitudinal)
        rv$n_domain_bucket <- scalar_or(state$n_domain_bucket, rv$n_domain_bucket)
        rv$check_kind <- scalar_or(state$check_kind, rv$check_kind)
        rv$topology_override_used <- isTRUE(state$topology_override_used)
        rv$topology_override <- scalar_or(state$topology_override, rv$topology_override)
      },
      ignoreInit = TRUE
    )

    resolved_topology <- shiny::reactive({
      state <- topology_ui_state()
      tryCatch(
        guided_resolve_topology_fn(
          is_longitudinal = state$is_longitudinal,
          n_domain_bucket = state$n_domain_bucket,
          check_kind = state$check_kind,
          topology_override_used = state$topology_override_used,
          topology_override = state$topology_override
        ),
        error = function(e) paste("Invalid:", conditionMessage(e))
      )
    })

    draft_candidate <- shiny::reactive({
      tryCatch(
        guided_build_rule_candidate_fn(
          draft = list(
            is_longitudinal = rv$is_longitudinal,
            n_domain_bucket = rv$n_domain_bucket,
            check_kind = rv$check_kind,
            topology_override_used = rv$topology_override_used,
            topology_override = rv$topology_override,
            rule_id = rv$rule_id,
            title = rv$title,
            objective = rv$objective,
            primary_domain = rv$primary_domain,
            domains_text = rv$domains_text,
            edge_cases = rv$edge_cases,
            scope = rv$scope,
            intent = rv$intent,
            severity = rv$severity,
            owner_team = rv$owner_team,
            candidate_columns = rv$candidate_columns,
            pseudo_logic = rv$pseudo_logic,
            opt_keys = rv$opt_keys,
            opt_vars = rv$opt_vars,
            opt_threshold = rv$opt_threshold,
            opt_window_days = rv$opt_window_days,
            opt_misc_json = rv$opt_misc_json
          ),
          pack = workspace$pack,
          domain_registry = workspace$domain_registry
        ),
        error = function(e) e
      )
    })

    draft_validation <- shiny::reactive({
      candidate <- draft_candidate()
      if (inherits(candidate, "error")) {
        return(candidate)
      }

      tryCatch(
        aRtifacts::build_rule_pack_spec(
          pack = workspace$pack,
          rules = c(workspace$rules, list(candidate)),
          validate = TRUE
        ),
        error = function(e) e
      )
    })

    refresh_preview <- function() {
      built <- tryCatch(
        aRtifacts::build_rule_pack_spec(
          pack = workspace$pack,
          rules = workspace$rules,
          validate = TRUE
        ),
        error = function(e) e
      )

      if (inherits(built, "error")) {
        preview_state(list(spec = NULL, error = conditionMessage(built)))
        return()
      }

      preview_state(list(spec = built, error = NULL))
    }

    observe({
      if (!nzchar(rv$rule_id)) {
        rv$rule_id <- next_rule_id()
      }
    })

    observeEvent(workspace$rules, {
      if (rv$step >= 5L) {
        refresh_preview()
      }
    }, ignoreInit = TRUE)

    output$guided_stepper <- shiny::renderUI({
      shiny::tags$ol(
        class = "rb-stepper",
        lapply(seq_along(step_labels), function(idx) {
          cls <- if (idx < rv$step) {
            "rb-step done"
          } else if (idx == rv$step) {
            "rb-step active"
          } else {
            "rb-step"
          }
          shiny::tags$li(class = cls, paste0(idx, ". ", step_labels[[idx]]))
        })
      )
    })

    output$guided_debug_step <- shiny::renderText(as.character(rv$step))
    output$guided_debug_topology <- shiny::renderText(resolved_topology())
    output$guided_debug_rule_count <- shiny::renderText(as.character(length(workspace$rules)))

    output$guided_domain_registry <- shiny::renderTable({
      if (!length(workspace$domain_registry)) {
        return(data.frame())
      }

      data.frame(
        domain = names(workspace$domain_registry),
        columns = vapply(
          workspace$domain_registry,
          function(cols) paste(normalize_chr(cols), collapse = ", "),
          character(1)
        ),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$guided_confirm_summary <- shiny::renderPrint({
      candidate <- draft_candidate()
      if (inherits(candidate, "error")) {
        return(list(status = "invalid", issue = conditionMessage(candidate)))
      }

      validation <- draft_validation()
      if (inherits(validation, "error")) {
        return(list(
          status = "invalid",
          topology = candidate$topology,
          issue = conditionMessage(validation)
        ))
      }

      list(
        status = "valid",
        id = candidate$id,
        title = candidate$title,
        topology = candidate$topology,
        scope = candidate$scope,
        intent = candidate$intent,
        severity = candidate$severity,
        owner_team = candidate$owner_team,
        domain = candidate$domain,
        domains = candidate$domains,
        objective = candidate$handoff$objective
      )
    })

    output$guided_rules_table <- shiny::renderTable({
      if (!length(workspace$rules)) {
        return(data.frame())
      }

      data.frame(
        id = vapply(workspace$rules, function(rule) rule$id, character(1)),
        title = vapply(workspace$rules, function(rule) rule$title, character(1)),
        topology = vapply(workspace$rules, function(rule) rule$topology, character(1)),
        scope = vapply(workspace$rules, function(rule) rule$scope, character(1)),
        intent = vapply(workspace$rules, function(rule) rule$intent, character(1)),
        severity = vapply(workspace$rules, function(rule) rule$severity, character(1)),
        domains = vapply(
          workspace$rules,
          function(rule) {
            if (length(rule$domains)) {
              paste(rule$domains, collapse = ", ")
            } else {
              rule$domain
            }
          },
          character(1)
        ),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$guided_spec_preview <- shiny::renderText({
      state <- preview_state()
      if (!is.null(state$error)) {
        return(paste("Preview unavailable:", state$error))
      }
      if (is.null(state$spec)) {
        return("No preview generated yet.")
      }
      aRtifacts:::.rb_spec_yaml(state$spec)
    })

    output$guided_scaffold_preview <- shiny::renderText({
      state <- preview_state()
      if (!is.null(state$error)) {
        return(paste("Preview unavailable:", state$error))
      }
      if (is.null(state$spec)) {
        return("No preview generated yet.")
      }
      aRtifacts::render_rule_pack_scaffold(state$spec)
    })

    output$guided_prompt_preview <- shiny::renderText({
      state <- preview_state()
      if (!is.null(state$error)) {
        return(paste("Preview unavailable:", state$error))
      }
      if (is.null(state$spec)) {
        return("No preview generated yet.")
      }
      aRtifacts::render_rule_agent_prompt_bundle(state$spec)
    })

    output$guided_export_status <- shiny::renderPrint({
      state <- preview_state()
      list(
        dry_run = isTRUE(dry_run),
        write_enabled = isTRUE(input$guided_enable_write_mode),
        preview_error = state$error,
        output_dir = scalar_or(input$guided_output_dir, rv$output_dir),
        last_write = workspace$last_write
      )
    })

    output$guided_step_body <- shiny::renderUI({
      if (rv$step == 1L) {
        return(
          shiny::tagList(
            shiny::h3("1) Pack Setup"),
            shiny::p("Set reusable defaults once before creating rules."),
            ti("pack_name", "Pack name", value = scalar_or(input$pack_name, scalar_or(workspace$pack$pack_name, ""))),
            ti("owner_team", "Owner team", value = scalar_or(input$owner_team, scalar_or(workspace$pack$owner_team, "Data Operations"))),
            si(
              "default_severity",
              "Default severity",
              choices = c("info", "warning", "error", "critical"),
              selected = scalar_or(input$default_severity, scalar_or(workspace$pack$default_severity, "warning"))
            ),
            ti(
              "default_intent",
              "Default intent",
              value = scalar_or(input$default_intent, scalar_or(workspace$pack$default_intent, "quality_screening"))
            ),
            tai(
              "pack_notes",
              "Pack notes (optional)",
              value = scalar_or(input$pack_notes, scalar_or(workspace$pack$notes, "")),
              rows = 3
            ),
            shiny::tags$details(
              shiny::tags$summary("Advanced pack options"),
              shiny::tags$br(),
              ti(
                "pack_function_name",
                "Function name (optional override)",
                value = scalar_or(input$pack_function_name, scalar_or(workspace$pack$function_name, ""))
              )
            ),
            shiny::tags$br(),
            ab("start_rule_1", "Start Rule 1", class = "btn-primary")
          )
        )
      }

      if (rv$step == 2L) {
        state <- topology_ui_state()
        kind_choices <- guided_check_kind_choices_fn(
          is_longitudinal = state$is_longitudinal,
          n_domain_bucket = state$n_domain_bucket
        )
        selected_kind <- state$check_kind
        if (length(kind_choices)) {
          valid_values <- unname(kind_choices)
          if (!(selected_kind %in% valid_values)) {
            selected_kind <- valid_values[[1]]
          }
        }

        return(
          shiny::tagList(
            shiny::h3("2) Topology Questions"),
            ri(
              "q_is_longitudinal",
              "Does this compare current vs previous snapshots?",
              choices = c("No" = "no", "Yes" = "yes"),
              selected = if (state$is_longitudinal) "yes" else "no",
              inline = TRUE
            ),
            ri(
              "q_n_domain_bucket",
              "How many domains does this rule depend on?",
              choices = c("1 domain" = "1", "2 or more domains" = "2+"),
              selected = state$n_domain_bucket,
              inline = TRUE
            ),
            if (length(kind_choices)) {
              si(
                "q_check_kind",
                "What kind of check is this?",
                choices = kind_choices,
                selected = selected_kind
              )
            } else {
              shiny::wellPanel(
                shiny::strong("What kind of check is this?"),
                shiny::p("Longitudinal + 2+ domains automatically maps to topology A7.")
              )
            },
            shiny::div(
              class = "rb-topology-badge",
              shiny::strong("Resolved topology: "),
              shiny::span(resolved_topology())
            ),
            shiny::fluidRow(
              shiny::column(6, ab("topology_back", "Back")),
              shiny::column(6, ab("topology_next", "Next", class = "btn-primary"))
            )
          )
        )
      }

      if (rv$step == 3L) {
        topology <- resolved_topology()
        is_single_domain <- topology %in% c("A0", "A1", "A2", "A5", "A6")

        return(
          shiny::tagList(
            shiny::h3("3) Rule Core Metadata"),
            shiny::div(
              class = "rb-topology-badge",
              shiny::strong("Resolved topology: "),
              shiny::span(topology)
            ),
            ti("rule_id", "Rule ID", value = rv$rule_id),
            ti("rule_title", "Rule title", value = rv$title),
            tai("rule_objective", "Objective", value = rv$objective, rows = 3),
            if (is_single_domain) {
              ti("rule_primary_domain", "Primary domain", value = rv$primary_domain)
            } else {
              ti(
                "rule_domains_text",
                "Domains (comma separated, at least 2)",
                value = rv$domains_text
              )
            },
            ti(
              "rule_edge_cases",
              "Edge case notes (optional)",
              value = rv$edge_cases
            ),
            shiny::fluidRow(
              shiny::column(
                width = 3,
                si(
                  "rule_scope",
                  "Scope",
                  choices = c("record", "group", "snapshot"),
                  selected = rv$scope
                )
              ),
              shiny::column(3, ti("rule_intent", "Intent", value = rv$intent)),
              shiny::column(
                width = 3,
                si(
                  "rule_severity",
                  "Severity",
                  choices = c("info", "warning", "error", "critical"),
                  selected = rv$severity
                )
              ),
              shiny::column(3, ti("rule_owner_team", "Owner team", value = rv$owner_team))
            ),
            ti(
              "rule_candidate_columns",
              "Candidate columns (optional, comma separated)",
              value = rv$candidate_columns
            ),
            tai(
              "rule_pseudo_logic",
              "Pseudo-logic notes (optional)",
              value = rv$pseudo_logic,
              rows = 3
            ),
            shiny::tags$details(
              shiny::tags$summary("Optional Domain Context"),
              shiny::tags$br(),
              shiny::fluidRow(
                shiny::column(4, ti("ctx_domain_name", "Domain name", value = "")),
                shiny::column(4, ti("ctx_domain_columns", "Manual columns (comma separated)", value = "")),
                shiny::column(4, fi("ctx_domain_csv", "CSV upload", accept = ".csv"))
              ),
              ab("ctx_domain_add", "Add / Update Domain"),
              to("guided_domain_registry")
            ),
            shiny::tags$details(
              shiny::tags$summary("Advanced Rule Inputs"),
              shiny::tags$br(),
              ci(
                "adv_topology_override_used",
                "Manual topology override",
                value = rv$topology_override_used
              ),
              si(
                "adv_topology_override",
                "Override topology",
                choices = c("A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7"),
                selected = rv$topology_override
              ),
              shiny::fluidRow(
                shiny::column(6, ti("adv_opt_keys", "keys (comma separated)", value = rv$opt_keys)),
                shiny::column(6, ti("adv_opt_vars", "vars (comma separated)", value = rv$opt_vars))
              ),
              shiny::fluidRow(
                shiny::column(4, ni("adv_opt_threshold", "threshold", value = rv$opt_threshold)),
                shiny::column(4, ni("adv_opt_window_days", "window_days", value = rv$opt_window_days)),
                shiny::column(4, ti("adv_opt_misc_json", "misc JSON", value = rv$opt_misc_json))
              )
            ),
            shiny::fluidRow(
              shiny::column(6, ab("rule_core_back", "Back")),
              shiny::column(6, ab("rule_core_next", "Next", class = "btn-primary"))
            )
          )
        )
      }

      if (rv$step == 4L) {
        return(
          shiny::tagList(
            shiny::h3("4) Confirm + Add Rule"),
            shiny::p("Review the rule summary and add it to the rule pack."),
            vo("guided_confirm_summary"),
            shiny::fluidRow(
              shiny::column(6, ab("confirm_back", "Back")),
              shiny::column(6, ab("confirm_add_rule", "Add Rule to Pack", class = "btn-primary"))
            )
          )
        )
      }

      shiny::tagList(
        shiny::h3("5) Finish / Export"),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::div(
              class = "rb-pane-card",
              shiny::h4("Rule Summary + Quick Edit"),
              shiny::p(class = "rb-subtle", "Quick edit supports core fields only."),
              shiny::div(class = "rb-table-wrap", to("guided_rules_table")),
              si(
                "quick_rule_id",
                "Select rule",
                choices = if (length(workspace$rules)) {
                  stats::setNames(
                    vapply(workspace$rules, function(rule) rule$id, character(1)),
                    vapply(workspace$rules, function(rule) paste0(rule$id, " | ", rule$title), character(1))
                  )
                } else {
                  character()
                },
                selected = rv$quick_rule_id
              ),
              shiny::div(
                class = "rb-button-row",
                ab("quick_load", "Quick Edit"),
                ab("quick_delete", "Delete"),
                ab("quick_save", "Save Edit", class = "btn-primary")
              ),
              shiny::div(
                class = "rb-button-row",
                ab("quick_move_up", "Move Up"),
                ab("quick_move_down", "Move Down")
              ),
              shiny::tags$hr(),
              ti("quick_id", "Rule ID", value = rv$quick_id),
              ti("quick_title", "Rule title", value = rv$quick_title),
              tai("quick_objective", "Objective", value = rv$quick_objective, rows = 2),
              ti("quick_domain", "Primary domain (single-domain rules)", value = rv$quick_domain),
              ti("quick_domains_text", "Domains (cross-domain rules, comma separated)", value = rv$quick_domains_text),
              shiny::fluidRow(
                shiny::column(
                  width = 3,
                  si(
                    "quick_scope",
                    "Scope",
                    choices = c("record", "group", "snapshot"),
                    selected = rv$quick_scope
                  )
                ),
                shiny::column(3, ti("quick_intent", "Intent", value = rv$quick_intent)),
                shiny::column(
                  width = 3,
                  si(
                    "quick_severity",
                    "Severity",
                    choices = c("info", "warning", "error", "critical"),
                    selected = rv$quick_severity
                  )
                ),
                shiny::column(3, ti("quick_owner_team", "Owner team", value = rv$quick_owner_team))
              )
            )
          ),
          shiny::column(
            width = 6,
            shiny::div(
              class = "rb-pane-card",
              shiny::h4("Preview + Export"),
              ti("guided_output_dir", "Output root directory", value = scalar_or(input$guided_output_dir, rv$output_dir)),
              shiny::div(
                class = "rb-checkbox-stack",
                ci("guided_enable_write_mode", "Enable file writes for this session", value = FALSE),
                ci("guided_confirm_write", "I confirm writing files to a new timestamped folder", value = FALSE)
              ),
              shiny::div(
                class = "rb-button-row",
                ab("guided_refresh_preview", "Refresh Preview", class = "btn-primary"),
                ab("guided_write_files", "Write Files")
              ),
              shiny::div(
                class = "rb-preview-tabs",
                shiny::tabsetPanel(
                  type = "tabs",
                  shiny::tabPanel("YAML Spec", vo("guided_spec_preview", placeholder = TRUE)),
                  shiny::tabPanel("R Scaffold", vo("guided_scaffold_preview", placeholder = TRUE)),
                  shiny::tabPanel("Agent Prompt", vo("guided_prompt_preview", placeholder = TRUE))
                )
              ),
              shiny::tags$details(
                shiny::tags$summary("Export status"),
                vo("guided_export_status", placeholder = TRUE)
              )
            )
          )
        ),
        shiny::div(
          class = "rb-finish-actions",
          ab("finish_add_another", "Add Another Rule"),
          ab("finish_refresh", "Refresh Summary")
        )
      )
    })

    observeEvent(input$start_rule_1, {
      pack_candidate <- list(
        pack_name = input$pack_name,
        function_name = input$pack_function_name,
        owner_team = input$owner_team,
        default_severity = input$default_severity,
        default_intent = input$default_intent,
        notes = input$pack_notes
      )

      built <- tryCatch(
        aRtifacts::build_rule_pack_spec(
          pack = pack_candidate,
          rules = workspace$rules,
          validate = FALSE
        ),
        error = function(e) e
      )

      if (inherits(built, "error")) {
        shiny::showNotification(conditionMessage(built), type = "error")
        return()
      }

      workspace$pack <- built$pack
      rv$owner_team <- workspace$pack$owner_team
      rv$intent <- workspace$pack$default_intent
      rv$severity <- workspace$pack$default_severity
      if (!nzchar(rv$rule_id)) {
        rv$rule_id <- next_rule_id()
      }
      rv$step <- 2L
    })

    observeEvent(input$topology_back, {
      capture_topology_inputs()
      rv$step <- 1L
    })

    observeEvent(input$topology_next, {
      capture_topology_inputs()
      top <- resolved_topology()
      if (grepl("^Invalid:", top)) {
        shiny::showNotification(top, type = "error")
        return()
      }
      rv$step <- 3L
    })

    observeEvent(input$ctx_domain_add, {
      domain_name <- trimws(scalar_or(input$ctx_domain_name, ""))
      if (!nzchar(domain_name)) {
        shiny::showNotification("Domain name is required.", type = "error")
        return()
      }

      manual_cols <- parse_csv(input$ctx_domain_columns)
      csv_cols <- character()
      if (!is.null(input$ctx_domain_csv)) {
        csv_cols <- tryCatch(
          aRtifacts:::.rb_read_csv_header(input$ctx_domain_csv$datapath),
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error")
            return(NULL)
          }
        )
        if (is.null(csv_cols)) {
          return()
        }
      }

      existing <- workspace$domain_registry[[domain_name]]
      workspace$domain_registry[[domain_name]] <- unique(c(existing, manual_cols, csv_cols))
      shiny::showNotification("Domain context updated.", type = "message")
    })

    observeEvent(input$rule_core_back, {
      capture_rule_core_inputs()
      rv$step <- 2L
    })

    observeEvent(input$rule_core_next, {
      capture_rule_core_inputs()

      if (!nzchar(trimws(rv$rule_id)) || !nzchar(trimws(rv$title)) || !nzchar(trimws(rv$objective))) {
        shiny::showNotification("Rule ID, title, and objective are required.", type = "error")
        return()
      }

      validation <- draft_validation()
      if (inherits(validation, "error")) {
        shiny::showNotification(conditionMessage(validation), type = "error")
        return()
      }

      rv$step <- 4L
    })

    observeEvent(input$confirm_back, {
      rv$step <- 3L
    })

    observeEvent(input$confirm_add_rule, {
      validation <- draft_validation()
      if (inherits(validation, "error")) {
        shiny::showNotification(conditionMessage(validation), type = "error")
        return()
      }

      workspace$pack <- validation$pack
      workspace$rules <- validation$rules

      candidate <- draft_candidate()
      if (!inherits(candidate, "error")) {
        mapped <- unique(c(
          candidate$domain,
          candidate$domains
        ))
        mapped <- mapped[nzchar(mapped)]
        for (domain_name in mapped) {
          if (is.null(workspace$domain_registry[[domain_name]])) {
            workspace$domain_registry[[domain_name]] <- character()
          }
        }
      }

      shiny::showModal(
        shiny::modalDialog(
          title = "Rule added to pack",
          shiny::p("Would you like to add another rule or finish and export?"),
          easyClose = FALSE,
          footer = shiny::tagList(
            shiny::actionButton(ns("modal_add_another"), "Add Another Rule"),
            shiny::actionButton(ns("modal_finish_pack"), "Finish Pack", class = "btn-primary")
          )
        )
      )
    })

    observeEvent(input$modal_add_another, {
      shiny::removeModal()
      reset_rule_draft()
      rv$step <- 2L
    })

    observeEvent(input$modal_finish_pack, {
      shiny::removeModal()
      rv$step <- 5L
      refresh_preview()
    })

    observeEvent(input$finish_add_another, {
      reset_rule_draft()
      rv$step <- 2L
    })

    observeEvent(input$finish_refresh, {
      refresh_preview()
    })

    observeEvent(workspace$rules, {
      ids <- vapply(workspace$rules, function(rule) rule$id, character(1))
      if (!length(ids)) {
        rv$quick_rule_id <- ""
      } else if (!(rv$quick_rule_id %in% ids)) {
        rv$quick_rule_id <- ids[[1]]
      }
    }, ignoreInit = FALSE)

    observeEvent(input$quick_rule_id, {
      rv$quick_rule_id <- scalar_or(input$quick_rule_id, rv$quick_rule_id)
    }, ignoreInit = TRUE)

    observeEvent(input$quick_load, {
      if (!length(workspace$rules)) return()
      selected <- scalar_or(input$quick_rule_id, rv$quick_rule_id)
      idx <- match(selected, vapply(workspace$rules, function(rule) rule$id, character(1)))
      if (is.na(idx)) return()

      rule <- workspace$rules[[idx]]
      rv$quick_rule_id <- rule$id
      rv$quick_id <- rule$id
      rv$quick_title <- rule$title
      rv$quick_objective <- scalar_or(rule$handoff$objective, "")
      rv$quick_domain <- rule$domain
      rv$quick_domains_text <- paste(rule$domains, collapse = ", ")
      rv$quick_scope <- rule$scope
      rv$quick_intent <- rule$intent
      rv$quick_severity <- rule$severity
      rv$quick_owner_team <- rule$owner_team

      shiny::updateTextInput(session, "quick_id", value = rv$quick_id)
      shiny::updateTextInput(session, "quick_title", value = rv$quick_title)
      shiny::updateTextAreaInput(session, "quick_objective", value = rv$quick_objective)
      shiny::updateTextInput(session, "quick_domain", value = rv$quick_domain)
      shiny::updateTextInput(session, "quick_domains_text", value = rv$quick_domains_text)
      shiny::updateSelectInput(session, "quick_scope", selected = rv$quick_scope)
      shiny::updateTextInput(session, "quick_intent", value = rv$quick_intent)
      shiny::updateSelectInput(session, "quick_severity", selected = rv$quick_severity)
      shiny::updateTextInput(session, "quick_owner_team", value = rv$quick_owner_team)
    })

    observeEvent(input$quick_save, {
      if (!length(workspace$rules)) return()
      selected <- scalar_or(input$quick_rule_id, rv$quick_rule_id)
      idx <- match(selected, vapply(workspace$rules, function(rule) rule$id, character(1)))
      if (is.na(idx)) return()

      nonempty_or <- function(x, default) {
        value <- trimws(scalar_or(x, ""))
        if (nzchar(value)) value else default
      }

      rule <- workspace$rules[[idx]]
      rule$id <- nonempty_or(input$quick_id, rule$id)
      rule$title <- nonempty_or(input$quick_title, rule$title)
      rule$scope <- nonempty_or(input$quick_scope, rule$scope)
      rule$intent <- nonempty_or(input$quick_intent, rule$intent)
      rule$severity <- nonempty_or(input$quick_severity, rule$severity)
      rule$owner_team <- nonempty_or(input$quick_owner_team, rule$owner_team)
      rule$handoff$objective <- nonempty_or(input$quick_objective, scalar_or(rule$handoff$objective, ""))

      if (rule$topology %in% c("A0", "A1", "A2", "A5", "A6")) {
        rule$domain <- nonempty_or(input$quick_domain, rule$domain)
        rule$domains <- character()
      } else {
        rule$domain <- ""
        parsed_domains <- parse_csv(input$quick_domains_text)
        if (length(parsed_domains)) {
          rule$domains <- parsed_domains
        }
      }

      staged <- workspace$rules
      staged[[idx]] <- rule
      built <- tryCatch(
        aRtifacts::build_rule_pack_spec(
          pack = workspace$pack,
          rules = staged,
          validate = TRUE
        ),
        error = function(e) e
      )

      if (inherits(built, "error")) {
        shiny::showNotification(conditionMessage(built), type = "error")
        return()
      }

      workspace$pack <- built$pack
      workspace$rules <- built$rules
      rv$quick_rule_id <- rule$id
      shiny::showNotification("Rule updated.", type = "message")
      refresh_preview()
    })

    observeEvent(input$quick_delete, {
      selected <- scalar_or(input$quick_rule_id, rv$quick_rule_id)
      workspace$rules <- aRtifacts:::.rb_delete_workspace_rule(workspace$rules, selected)
      refresh_preview()
    })

    observeEvent(input$quick_move_up, {
      selected <- scalar_or(input$quick_rule_id, rv$quick_rule_id)
      workspace$rules <- aRtifacts:::.rb_move_workspace_rule(workspace$rules, selected, direction = "up")
      refresh_preview()
    })

    observeEvent(input$quick_move_down, {
      selected <- scalar_or(input$quick_rule_id, rv$quick_rule_id)
      workspace$rules <- aRtifacts:::.rb_move_workspace_rule(workspace$rules, selected, direction = "down")
      refresh_preview()
    })

    observeEvent(input$guided_refresh_preview, {
      rv$output_dir <- scalar_or(input$guided_output_dir, rv$output_dir)
      workspace$output_dir <- rv$output_dir
      refresh_preview()
    })

    observeEvent(input$guided_write_files, {
      state <- preview_state()
      if (is.null(state$spec) || !is.null(state$error)) {
        shiny::showNotification("Resolve preview errors before writing files.", type = "error")
        return()
      }

      if (!isTRUE(input$guided_confirm_write)) {
        shiny::showNotification("Write confirmation checkbox is required.", type = "error")
        return()
      }

      if (isTRUE(dry_run) && !isTRUE(input$guided_enable_write_mode)) {
        shiny::showNotification("Dry-run session: enable write mode before exporting files.", type = "error")
        return()
      }

      target_dir <- scalar_or(input$guided_output_dir, rv$output_dir)
      result <- tryCatch(
        aRtifacts:::.rb_write_rule_builder_outputs(
          spec = state$spec,
          output_dir = target_dir,
          confirm = TRUE
        ),
        error = function(e) e
      )

      if (inherits(result, "error")) {
        shiny::showNotification(conditionMessage(result), type = "error")
        return()
      }

      workspace$last_write <- result
      rv$output_dir <- target_dir
      shiny::showNotification("Rule pack artifacts written.", type = "message")
    })

    # Return minimal server state to make guided flow unit-testable.
    list(
      wizard_step = shiny::reactive(rv$step),
      wizard_topology = shiny::reactive(resolved_topology()),
      wizard_rule_count = shiny::reactive(length(workspace$rules))
    )
  })
}
