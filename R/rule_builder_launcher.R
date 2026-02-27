.rb_require_suggested <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "Package `", pkg, "` is required for the rule builder app. ",
      "Install it first (for example, install.packages(\"", pkg, "\")).",
      call. = FALSE
    )
  }
}

.rb_resolve_app_dir <- function() {
  app_dir <- system.file("apps", "rule_builder", package = "aRtifacts")
  if (nzchar(app_dir) && dir.exists(app_dir)) {
    return(app_dir)
  }

  # Development fallback for `devtools::load_all()` style sessions.
  pkg_path <- tryCatch(getNamespaceInfo("aRtifacts", "path"), error = function(e) "")
  if (nzchar(pkg_path)) {
    dev_dir <- file.path(pkg_path, "inst", "apps", "rule_builder")
    if (dir.exists(dev_dir)) {
      return(dev_dir)
    }
  }

  stop("Could not locate `inst/apps/rule_builder`.", call. = FALSE)
}

#' Launch the interactive rule-builder funnel app
#'
#' Opens a local Shiny app that guides users through pack setup, topology
#' decision capture, rule metadata entry, and preview/export of generated
#' artifacts.
#'
#' @param output_dir Root output directory for generated artifacts.
#' @param dry_run Logical scalar. When `TRUE`, app starts in preview-only mode.
#'
#' @return The value returned by `shiny::runApp()` (invisibly).
#' @export
launch_rule_builder <- function(output_dir = getwd(), dry_run = TRUE) {
  if (!is.character(output_dir) || length(output_dir) != 1L || !nzchar(trimws(output_dir))) {
    stop("`output_dir` must be a non-empty path string.", call. = FALSE)
  }
  if (!is.logical(dry_run) || length(dry_run) != 1L || is.na(dry_run)) {
    stop("`dry_run` must be a single non-missing logical scalar.", call. = FALSE)
  }

  .rb_require_suggested("shiny")
  .rb_require_suggested("bslib")
  .rb_require_suggested("yaml")
  .rb_require_suggested("jsonlite")

  app_dir <- .rb_resolve_app_dir()

  old_opts <- options(
    aRtifacts.rule_builder.output_dir = normalizePath(output_dir, mustWork = FALSE),
    aRtifacts.rule_builder.dry_run = isTRUE(dry_run)
  )
  on.exit(options(old_opts), add = TRUE)

  shiny::runApp(
    appDir = app_dir,
    launch.browser = interactive()
  )
}
