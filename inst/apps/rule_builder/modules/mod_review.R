mod_review_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::h3("Review Workspace"),
    shiny::p("Inspect rule metadata and validation status before generating files."),
    shiny::tableOutput(ns("review_table")),
    shiny::h4("Validation Summary"),
    shiny::verbatimTextOutput(ns("review_summary"), placeholder = TRUE)
  )
}

mod_review_server <- function(id, workspace) {
  shiny::moduleServer(id, function(input, output, session) {
    validate_single_rule <- function(rule) {
      out <- tryCatch(
        aRtifacts::build_rule_pack_spec(
          pack = workspace$pack,
          rules = list(rule),
          validate = TRUE
        ),
        error = function(e) e
      )

      if (inherits(out, "error")) {
        return(list(status = "invalid", issue = conditionMessage(out)))
      }

      list(status = "valid", issue = "")
    }

    output$review_table <- shiny::renderTable({
      if (!length(workspace$rules)) {
        return(data.frame())
      }

      rows <- lapply(workspace$rules, function(rule) {
        checked <- validate_single_rule(rule)
        domains <- if (length(rule$domains)) {
          paste(rule$domains, collapse = ", ")
        } else {
          rule$domain
        }

        data.frame(
          id = rule$id,
          topology = rule$topology,
          scope = rule$scope,
          intent = rule$intent,
          severity = rule$severity,
          domains = domains,
          status = checked$status,
          issue = checked$issue,
          stringsAsFactors = FALSE
        )
      })

      do.call(rbind, rows)
    }, striped = TRUE, bordered = TRUE, width = "100%")

    output$review_summary <- shiny::renderPrint({
      if (!length(workspace$rules)) {
        return(list(message = "No rules in workspace."))
      }

      workspace_check <- tryCatch(
        aRtifacts::build_rule_pack_spec(
          pack = workspace$pack,
          rules = workspace$rules,
          validate = TRUE
        ),
        error = function(e) e
      )

      if (inherits(workspace_check, "error")) {
        return(list(
          status = "invalid",
          unresolved_issue = conditionMessage(workspace_check)
        ))
      }

      list(
        status = "valid",
        total_rules = length(workspace$rules),
        pack_name = workspace$pack$pack_name
      )
    })
  })
}
