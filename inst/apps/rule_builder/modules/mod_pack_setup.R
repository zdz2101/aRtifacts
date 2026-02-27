mod_pack_setup_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 7,
        shiny::h3("Pack Setup"),
        shiny::p("Define reusable defaults that prefill each rule entry."),
        shiny::textInput(ns("pack_name"), "Pack name", value = ""),
        shiny::textInput(ns("function_name"), "Function name (optional)", value = ""),
        shiny::textInput(ns("owner_team"), "Owner team", value = "Data Operations"),
        shiny::selectInput(
          ns("default_severity"),
          "Default severity",
          choices = c("info", "warning", "error", "critical"),
          selected = "warning"
        ),
        shiny::textInput(ns("default_intent"), "Default intent", value = "quality_screening"),
        shiny::textAreaInput(ns("notes"), "Pack notes", value = "", rows = 4),
        shiny::actionButton(ns("save_pack"), "Save Pack Setup", class = "btn-primary")
      ),
      shiny::column(
        width = 5,
        shiny::h4("Current Pack State"),
        shiny::verbatimTextOutput(ns("pack_preview"), placeholder = TRUE)
      )
    )
  )
}

mod_pack_setup_server <- function(id, workspace) {
  shiny::moduleServer(id, function(input, output, session) {
    scalar_or <- function(x, default = "") {
      if (is.null(x) || !length(x)) return(default)
      value <- as.character(x[[1]])
      if (is.na(value)) return(default)
      value
    }

    observe({
      pack <- workspace$pack
      shiny::updateTextInput(session, "pack_name", value = scalar_or(pack$pack_name, ""))
      shiny::updateTextInput(session, "function_name", value = scalar_or(pack$function_name, ""))
      shiny::updateTextInput(session, "owner_team", value = scalar_or(pack$owner_team, "Data Operations"))
      shiny::updateSelectInput(
        session,
        "default_severity",
        selected = scalar_or(pack$default_severity, "warning")
      )
      shiny::updateTextInput(
        session,
        "default_intent",
        value = scalar_or(pack$default_intent, "quality_screening")
      )
      shiny::updateTextAreaInput(session, "notes", value = scalar_or(pack$notes, ""))
    })

    observeEvent(input$save_pack, {
      pack_candidate <- list(
        pack_name = input$pack_name,
        function_name = input$function_name,
        owner_team = input$owner_team,
        default_severity = input$default_severity,
        default_intent = input$default_intent,
        notes = input$notes
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
      shiny::showNotification("Pack setup saved.", type = "message")
    })

    output$pack_preview <- shiny::renderPrint({
      pack <- workspace$pack
      list(
        pack_name = pack$pack_name,
        pack_slug = pack$pack_slug,
        function_name = pack$function_name,
        owner_team = pack$owner_team,
        default_severity = pack$default_severity,
        default_intent = pack$default_intent,
        notes = pack$notes
      )
    })
  })
}
