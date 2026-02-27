.rule_builder_app_dir <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (dir.exists(file.path(wd, "modules"))) {
    return(wd)
  }

  src_file <- tryCatch(normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = FALSE), error = function(e) "")
  if (nzchar(src_file) && file.exists(src_file)) {
    return(dirname(src_file))
  }
  wd
}

.rb_app_dir <- .rule_builder_app_dir()

source(file.path(.rb_app_dir, "modules", "mod_pack_setup.R"), local = TRUE)
source(file.path(.rb_app_dir, "modules", "mod_rule_editor.R"), local = TRUE)
source(file.path(.rb_app_dir, "modules", "mod_review.R"), local = TRUE)
source(file.path(.rb_app_dir, "modules", "mod_generate.R"), local = TRUE)

.rb_default_output_dir <- getOption("aRtifacts.rule_builder.output_dir", getwd())
.rb_default_dry_run <- isTRUE(getOption("aRtifacts.rule_builder.dry_run", TRUE))

ui <- shiny::navbarPage(
  title = "aRtifacts Rule Builder",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  header = shiny::tags$head(
    shiny::tags$link(rel = "stylesheet", type = "text/css", href = "rule_builder.css")
  ),
  shiny::tabPanel("1) Pack Setup", mod_pack_setup_ui("pack_setup")),
  shiny::tabPanel("2) Add/Edit Rule", mod_rule_editor_ui("rule_editor")),
  shiny::tabPanel("3) Review", mod_review_ui("review")),
  shiny::tabPanel("4) Generate/Export", mod_generate_ui("generate"))
)

server <- function(input, output, session) {
  workspace <- shiny::reactiveValues(
    pack = list(
      pack_name = "",
      function_name = "",
      owner_team = "Data Operations",
      default_severity = "warning",
      default_intent = "quality_screening",
      notes = ""
    ),
    rules = list(),
    domain_registry = list(),
    output_dir = .rb_default_output_dir,
    last_write = NULL
  )

  mod_pack_setup_server("pack_setup", workspace = workspace)
  mod_rule_editor_server("rule_editor", workspace = workspace)
  mod_review_server("review", workspace = workspace)
  mod_generate_server("generate", workspace = workspace, dry_run = .rb_default_dry_run)
}

shiny::shinyApp(ui = ui, server = server)
