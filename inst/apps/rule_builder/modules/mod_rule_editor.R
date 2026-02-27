mod_rule_editor_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::h3("Rule Workspace"),
        shiny::selectInput(ns("selected_rule"), "Existing rule", choices = c("<new>" = "")),
        shiny::fluidRow(
          shiny::column(4, shiny::actionButton(ns("new_rule"), "New")),
          shiny::column(4, shiny::actionButton(ns("delete_rule"), "Delete")),
          shiny::column(4, shiny::actionButton(ns("save_rule"), "Save", class = "btn-primary"))
        ),
        shiny::tags$br(),
        shiny::fluidRow(
          shiny::column(6, shiny::actionButton(ns("move_up"), "Move Up")),
          shiny::column(6, shiny::actionButton(ns("move_down"), "Move Down"))
        ),
        shiny::tags$hr(),
        shiny::h4("Topology Decision"),
        shiny::checkboxInput(ns("is_longitudinal"), "Longitudinal comparison", value = FALSE),
        shiny::numericInput(ns("n_domains"), "Number of domains", value = 1, min = 1, step = 1),
        shiny::selectInput(
          ns("level"),
          "Rule level",
          choices = c("schema", "record", "aggregate", "relational"),
          selected = "record"
        ),
        shiny::checkboxInput(ns("override_topology"), "Manual topology override", value = FALSE),
        shiny::selectInput(
          ns("topology_override"),
          "Topology override",
          choices = c("A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7"),
          selected = "A1"
        ),
        shiny::verbatimTextOutput(ns("resolved_topology"), placeholder = TRUE)
      ),
      shiny::column(
        width = 8,
        shiny::h3("Schema / Domain Context"),
        shiny::fluidRow(
          shiny::column(4, shiny::textInput(ns("registry_domain_name"), "Domain name", value = "")),
          shiny::column(4, shiny::textInput(ns("registry_columns_manual"), "Manual columns (comma separated)", value = "")),
          shiny::column(4, shiny::fileInput(ns("registry_csv"), "Optional CSV upload", accept = ".csv"))
        ),
        shiny::actionButton(ns("add_registry_domain"), "Add / Update Domain Registry"),
        shiny::helpText("CSV upload must include a header row. Uploaded columns are merged with manual columns."),
        shiny::tableOutput(ns("domain_registry_table")),
        shiny::tags$hr(),
        shiny::h3("Rule Metadata"),
        shiny::fluidRow(
          shiny::column(4, shiny::textInput(ns("rule_id"), "Rule ID", value = "")),
          shiny::column(8, shiny::textInput(ns("rule_title"), "Rule title", value = ""))
        ),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::selectInput(
              ns("rule_scope"),
              "Scope",
              choices = c("record", "group", "snapshot"),
              selected = "record"
            )
          ),
          shiny::column(4, shiny::textInput(ns("rule_intent"), "Intent", value = "quality_screening")),
          shiny::column(
            width = 4,
            shiny::selectInput(
              ns("rule_severity"),
              "Severity",
              choices = c("info", "warning", "error", "critical"),
              selected = "warning"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(6, shiny::textInput(ns("rule_owner_team"), "Owner team", value = "Data Operations")),
          shiny::column(6, shiny::selectInput(ns("rule_domain"), "Primary domain", choices = character()))
        ),
        shiny::selectizeInput(
          ns("rule_domains"),
          "Cross-domain set",
          choices = character(),
          selected = NULL,
          multiple = TRUE
        ),
        shiny::tags$hr(),
        shiny::h3("Optional Arguments"),
        shiny::fluidRow(
          shiny::column(6, shiny::textInput(ns("opt_keys"), "keys (comma separated)", value = "")),
          shiny::column(6, shiny::textInput(ns("opt_vars"), "vars (comma separated)", value = ""))
        ),
        shiny::fluidRow(
          shiny::column(4, shiny::numericInput(ns("opt_threshold"), "threshold", value = NA_real_)),
          shiny::column(4, shiny::numericInput(ns("opt_window"), "window_days", value = NA_real_)),
          shiny::column(4, shiny::textInput(ns("opt_misc_json"), "misc JSON (optional)", value = ""))
        ),
        shiny::tags$hr(),
        shiny::h3("FN Handoff Context"),
        shiny::textAreaInput(ns("handoff_objective"), "Objective", value = "", rows = 3),
        shiny::textInput(ns("handoff_columns"), "Candidate columns (comma separated)", value = ""),
        shiny::textAreaInput(ns("handoff_pseudo"), "Pseudo-logic notes", value = "", rows = 4),
        shiny::textInput(ns("handoff_edge_cases"), "Edge cases (comma separated)", value = "0, blank, whitespace, NA"),
        shiny::verbatimTextOutput(ns("rule_status"), placeholder = TRUE)
      )
    )
  )
}

mod_rule_editor_server <- function(id, workspace) {
  shiny::moduleServer(id, function(input, output, session) {
    scalar_or <- function(x, default = "") {
      if (is.null(x) || !length(x)) return(default)
      value <- as.character(x[[1]])
      if (is.na(value)) return(default)
      value
    }

    to_rule_choices <- function(rules) {
      if (!length(rules)) {
        return(c("<new>" = ""))
      }

      ids <- vapply(rules, function(item) scalar_or(item$id, ""), character(1))
      labels <- vapply(
        rules,
        function(item) {
          paste0(scalar_or(item$id, ""), " | ", scalar_or(item$title, ""))
        },
        character(1)
      )
      c("<new>" = "", stats::setNames(ids, labels))
    }

    selected_rule <- shiny::reactive({
      sel <- scalar_or(input$selected_rule, "")
      if (!nzchar(sel)) {
        return(NULL)
      }

      idx <- match(sel, vapply(workspace$rules, function(item) scalar_or(item$id, ""), character(1)))
      if (is.na(idx)) {
        return(NULL)
      }

      workspace$rules[[idx]]
    })

    selected_topology <- shiny::reactive({
      if (isTRUE(input$override_topology)) {
        return(input$topology_override)
      }

      tryCatch(
        aRtifacts::choose_topology(
          is_longitudinal = isTRUE(input$is_longitudinal),
          n_domains = as.integer(input$n_domains),
          level = input$level
        ),
        error = function(e) paste("Invalid shape:", conditionMessage(e))
      )
    })

    observe({
      choices <- to_rule_choices(workspace$rules)
      shiny::updateSelectInput(session, "selected_rule", choices = choices)
    })

    observe({
      domains <- names(workspace$domain_registry)
      shiny::updateSelectInput(session, "rule_domain", choices = domains)
      shiny::updateSelectizeInput(session, "rule_domains", choices = domains, server = TRUE)
    })

    observe({
      pack <- workspace$pack
      if (nzchar(scalar_or(pack$owner_team, ""))) {
        shiny::updateTextInput(session, "rule_owner_team", value = pack$owner_team)
      }
      if (nzchar(scalar_or(pack$default_intent, ""))) {
        shiny::updateTextInput(session, "rule_intent", value = pack$default_intent)
      }
      if (nzchar(scalar_or(pack$default_severity, ""))) {
        shiny::updateSelectInput(session, "rule_severity", selected = pack$default_severity)
      }
    })

    observeEvent(input$new_rule, {
      shiny::updateSelectInput(session, "selected_rule", selected = "")
      shiny::updateTextInput(session, "rule_id", value = "")
      shiny::updateTextInput(session, "rule_title", value = "")
      shiny::updateSelectInput(session, "rule_scope", selected = "record")
      shiny::updateTextInput(session, "opt_keys", value = "")
      shiny::updateTextInput(session, "opt_vars", value = "")
      shiny::updateNumericInput(session, "opt_threshold", value = NA_real_)
      shiny::updateNumericInput(session, "opt_window", value = NA_real_)
      shiny::updateTextInput(session, "opt_misc_json", value = "")
      shiny::updateTextAreaInput(session, "handoff_objective", value = "")
      shiny::updateTextInput(session, "handoff_columns", value = "")
      shiny::updateTextAreaInput(session, "handoff_pseudo", value = "")
    })

    observeEvent(input$selected_rule, {
      rule <- selected_rule()
      if (is.null(rule)) {
        return()
      }

      # Keep edit behavior explicit: selecting a rule loads every editable
      # field so users can safely iterate without hidden defaults.
      shiny::updateTextInput(session, "rule_id", value = rule$id)
      shiny::updateTextInput(session, "rule_title", value = rule$title)
      shiny::updateSelectInput(session, "rule_scope", selected = rule$scope)
      shiny::updateTextInput(session, "rule_intent", value = rule$intent)
      shiny::updateSelectInput(session, "rule_severity", selected = rule$severity)
      shiny::updateTextInput(session, "rule_owner_team", value = rule$owner_team)
      shiny::updateSelectInput(session, "rule_domain", selected = rule$domain)
      shiny::updateSelectizeInput(session, "rule_domains", selected = rule$domains, server = TRUE)

      decision <- rule$decision
      shiny::updateCheckboxInput(session, "is_longitudinal", value = isTRUE(decision$is_longitudinal))
      shiny::updateNumericInput(session, "n_domains", value = as.integer(decision$n_domains))
      shiny::updateSelectInput(session, "level", selected = decision$level)
      shiny::updateCheckboxInput(session, "override_topology", value = isTRUE(decision$topology_override_used))
      shiny::updateSelectInput(session, "topology_override", selected = rule$topology)

      opt <- rule$optional_args
      shiny::updateTextInput(
        session,
        "opt_keys",
        value = paste(aRtifacts:::.rb_normalize_chr(opt$keys), collapse = ", ")
      )
      shiny::updateTextInput(
        session,
        "opt_vars",
        value = paste(aRtifacts:::.rb_normalize_chr(opt$vars), collapse = ", ")
      )
      shiny::updateNumericInput(
        session,
        "opt_threshold",
        value = if (!is.null(opt$threshold) && length(opt$threshold)) as.numeric(opt$threshold[[1]]) else NA_real_
      )
      shiny::updateNumericInput(
        session,
        "opt_window",
        value = if (!is.null(opt$window_days) && length(opt$window_days)) as.numeric(opt$window_days[[1]]) else NA_real_
      )

      misc <- opt[setdiff(names(opt), c("keys", "vars", "threshold", "window_days"))]
      shiny::updateTextInput(
        session,
        "opt_misc_json",
        value = if (length(misc)) jsonlite::toJSON(misc, auto_unbox = TRUE, pretty = FALSE) else ""
      )

      handoff <- rule$handoff
      shiny::updateTextAreaInput(session, "handoff_objective", value = scalar_or(handoff$objective, ""))
      shiny::updateTextInput(
        session,
        "handoff_columns",
        value = paste(aRtifacts:::.rb_normalize_chr(handoff$candidate_columns), collapse = ", ")
      )
      shiny::updateTextAreaInput(session, "handoff_pseudo", value = scalar_or(handoff$pseudo_logic, ""))
      shiny::updateTextInput(
        session,
        "handoff_edge_cases",
        value = paste(aRtifacts:::.rb_normalize_chr(handoff$edge_cases), collapse = ", ")
      )
    }, ignoreInit = TRUE)

    observeEvent(input$add_registry_domain, {
      domain_name <- trimws(scalar_or(input$registry_domain_name, ""))
      if (!nzchar(domain_name)) {
        shiny::showNotification("Domain name is required.", type = "error")
        return()
      }

      manual_cols <- aRtifacts:::.rb_split_csv_values(input$registry_columns_manual)
      csv_cols <- character()
      if (!is.null(input$registry_csv)) {
        csv_cols <- tryCatch(
          aRtifacts:::.rb_read_csv_header(input$registry_csv$datapath),
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
      shiny::showNotification(
        paste0("Domain registry updated for ", domain_name, "."),
        type = "message"
      )
    })

    observeEvent(input$delete_rule, {
      sel <- scalar_or(input$selected_rule, "")
      if (!nzchar(sel)) {
        return()
      }
      workspace$rules <- aRtifacts:::.rb_delete_workspace_rule(workspace$rules, sel)
      shiny::updateSelectInput(session, "selected_rule", selected = "")
      shiny::showNotification("Rule removed from workspace.", type = "message")
    })

    observeEvent(input$move_up, {
      sel <- scalar_or(input$selected_rule, "")
      workspace$rules <- aRtifacts:::.rb_move_workspace_rule(workspace$rules, sel, direction = "up")
    })

    observeEvent(input$move_down, {
      sel <- scalar_or(input$selected_rule, "")
      workspace$rules <- aRtifacts:::.rb_move_workspace_rule(workspace$rules, sel, direction = "down")
    })

    observeEvent(input$save_rule, {
      if (!nzchar(trimws(scalar_or(workspace$pack$pack_name, "")))) {
        shiny::showNotification("Set Pack Setup first (pack_name is required).", type = "error")
        return()
      }

      topology <- selected_topology()
      if (!topology %in% c("A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7")) {
        shiny::showNotification("Topology decision is invalid. Resolve shape or use override.", type = "error")
        return()
      }

      optional_args <- list()
      keys <- aRtifacts:::.rb_split_csv_values(input$opt_keys)
      vars <- aRtifacts:::.rb_split_csv_values(input$opt_vars)
      if (length(keys)) optional_args$keys <- keys
      if (length(vars)) optional_args$vars <- vars
      if (!is.na(input$opt_threshold)) optional_args$threshold <- as.numeric(input$opt_threshold)
      if (!is.na(input$opt_window)) optional_args$window_days <- as.numeric(input$opt_window)

      misc_json <- trimws(scalar_or(input$opt_misc_json, ""))
      if (nzchar(misc_json)) {
        parsed <- tryCatch(
          jsonlite::fromJSON(misc_json, simplifyVector = FALSE),
          error = function(e) e
        )
        if (inherits(parsed, "error")) {
          shiny::showNotification("Invalid misc JSON payload.", type = "error")
          return()
        }
        if (is.list(parsed)) {
          for (name in names(parsed)) {
            optional_args[[name]] <- parsed[[name]]
          }
        }
      }

      primary_domain <- scalar_or(input$rule_domain, "")
      cross_domains <- aRtifacts:::.rb_normalize_chr(input$rule_domains)
      mapped_domains <- unique(c(primary_domain, cross_domains))
      mapped_domains <- mapped_domains[nzchar(mapped_domains)]
      domain_columns <- workspace$domain_registry[mapped_domains]

      rule <- list(
        id = input$rule_id,
        title = input$rule_title,
        topology = topology,
        scope = input$rule_scope,
        intent = input$rule_intent,
        severity = input$rule_severity,
        owner_team = input$rule_owner_team,
        domain = primary_domain,
        domains = cross_domains,
        decision = list(
          is_longitudinal = isTRUE(input$is_longitudinal),
          n_domains = as.integer(input$n_domains),
          level = input$level,
          topology_override_used = isTRUE(input$override_topology)
        ),
        domain_mapping = list(
          domain = primary_domain,
          domains = cross_domains,
          columns = domain_columns
        ),
        optional_args = optional_args,
        handoff = list(
          objective = input$handoff_objective,
          candidate_columns = aRtifacts:::.rb_split_csv_values(input$handoff_columns),
          pseudo_logic = input$handoff_pseudo,
          edge_cases = aRtifacts:::.rb_split_csv_values(input$handoff_edge_cases)
        )
      )

      sel <- scalar_or(input$selected_rule, "")
      staged_rules <- tryCatch(
        aRtifacts:::.rb_upsert_workspace_rule(
          rules = workspace$rules,
          rule = rule,
          edit_id = if (nzchar(sel)) sel else NULL
        ),
        error = function(e) e
      )
      if (inherits(staged_rules, "error")) {
        shiny::showNotification(conditionMessage(staged_rules), type = "error")
        return()
      }

      built <- tryCatch(
        aRtifacts::build_rule_pack_spec(
          pack = workspace$pack,
          rules = staged_rules,
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
      shiny::updateSelectInput(session, "selected_rule", selected = input$rule_id)
      shiny::showNotification("Rule saved to workspace.", type = "message")
    })

    output$resolved_topology <- shiny::renderText({
      top <- selected_topology()
      paste("Resolved topology:", top)
    })

    output$domain_registry_table <- shiny::renderTable({
      if (!length(workspace$domain_registry)) {
        return(data.frame())
      }

      data.frame(
        domain = names(workspace$domain_registry),
        columns = vapply(
          workspace$domain_registry,
          function(cols) paste(aRtifacts:::.rb_normalize_chr(cols), collapse = ", "),
          character(1)
        ),
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$rule_status <- shiny::renderPrint({
      list(
        total_rules = length(workspace$rules),
        selected_rule = scalar_or(input$selected_rule, "<new>"),
        last_topology = selected_topology()
      )
    })
  })
}
