mod_generate_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::h3("Generate / Export"),
        shiny::textInput(ns("output_dir"), "Output root directory", value = getwd()),
        shiny::checkboxInput(ns("enable_write_mode"), "Enable file writes for this session", value = FALSE),
        shiny::checkboxInput(ns("confirm_write"), "I confirm writing files to a new timestamped folder", value = FALSE),
        shiny::fluidRow(
          shiny::column(6, shiny::actionButton(ns("refresh_preview"), "Refresh Preview", class = "btn-primary")),
          shiny::column(6, shiny::actionButton(ns("write_files"), "Write Files"))
        ),
        shiny::tags$br(),
        shiny::verbatimTextOutput(ns("export_status"), placeholder = TRUE)
      ),
      shiny::column(
        width = 6,
        shiny::h4("Output Preview"),
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel("YAML Spec", shiny::verbatimTextOutput(ns("spec_preview"), placeholder = TRUE)),
          shiny::tabPanel("R Scaffold", shiny::verbatimTextOutput(ns("scaffold_preview"), placeholder = TRUE)),
          shiny::tabPanel("Agent Prompt", shiny::verbatimTextOutput(ns("prompt_preview"), placeholder = TRUE))
        )
      )
    )
  )
}

mod_generate_server <- function(id, workspace, dry_run = TRUE) {
  shiny::moduleServer(id, function(input, output, session) {
    preview_state <- shiny::reactiveVal(list(spec = NULL, error = NULL))

    observe({
      shiny::updateTextInput(session, "output_dir", value = workspace$output_dir)
    })

    observe({
      if (!isTRUE(dry_run)) {
        shiny::updateCheckboxInput(session, "enable_write_mode", value = TRUE)
      }
    })

    build_preview <- function() {
      spec <- tryCatch(
        aRtifacts::build_rule_pack_spec(
          pack = workspace$pack,
          rules = workspace$rules,
          validate = TRUE
        ),
        error = function(e) e
      )

      if (inherits(spec, "error")) {
        preview_state(list(spec = NULL, error = conditionMessage(spec)))
        return()
      }

      preview_state(list(spec = spec, error = NULL))
    }

    observeEvent(input$refresh_preview, {
      workspace$output_dir <- input$output_dir
      build_preview()
    })

    observeEvent(TRUE, {
      build_preview()
    }, once = TRUE)

    output$spec_preview <- shiny::renderText({
      state <- preview_state()
      if (!is.null(state$error)) {
        return(paste("Preview unavailable:", state$error))
      }
      if (is.null(state$spec)) {
        return("No preview generated yet.")
      }

      aRtifacts:::.rb_spec_yaml(state$spec)
    })

    output$scaffold_preview <- shiny::renderText({
      state <- preview_state()
      if (!is.null(state$error)) {
        return(paste("Preview unavailable:", state$error))
      }
      if (is.null(state$spec)) {
        return("No preview generated yet.")
      }

      aRtifacts::render_rule_pack_scaffold(state$spec)
    })

    output$prompt_preview <- shiny::renderText({
      state <- preview_state()
      if (!is.null(state$error)) {
        return(paste("Preview unavailable:", state$error))
      }
      if (is.null(state$spec)) {
        return("No preview generated yet.")
      }

      aRtifacts::render_rule_agent_prompt_bundle(state$spec)
    })

    observeEvent(input$write_files, {
      state <- preview_state()
      if (is.null(state$spec) || !is.null(state$error)) {
        shiny::showNotification("Resolve preview errors before writing files.", type = "error")
        return()
      }

      if (!isTRUE(input$confirm_write)) {
        shiny::showNotification("Write confirmation checkbox is required.", type = "error")
        return()
      }

      if (isTRUE(dry_run) && !isTRUE(input$enable_write_mode)) {
        shiny::showNotification(
          "Dry-run session: enable write mode before exporting files.",
          type = "error"
        )
        return()
      }

      result <- tryCatch(
        aRtifacts:::.rb_write_rule_builder_outputs(
          spec = state$spec,
          output_dir = input$output_dir,
          confirm = TRUE
        ),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        shiny::showNotification(conditionMessage(result), type = "error")
        return()
      }

      workspace$last_write <- result
      shiny::showNotification("Rule pack artifacts written.", type = "message")
    })

    output$export_status <- shiny::renderPrint({
      state <- preview_state()
      list(
        dry_run = isTRUE(dry_run),
        write_enabled = isTRUE(input$enable_write_mode),
        preview_error = state$error,
        output_dir = input$output_dir,
        last_write = workspace$last_write
      )
    })
  })
}
