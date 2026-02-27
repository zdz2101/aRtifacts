test_that("rule builder app sources to a shiny app object", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  app_dir <- aRtifacts:::.rb_resolve_app_dir()
  expect_true(dir.exists(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(app_dir)

  app_obj <- source("app.R", local = new.env(parent = baseenv()))$value
  expect_true(shiny::is.shiny.appobj(app_obj))

  app_src <- paste(readLines("app.R", warn = FALSE), collapse = "\n")
  expect_true(grepl("Guided Builder", app_src, fixed = TRUE))
  expect_true(grepl("Advanced Workspace", app_src, fixed = TRUE))
})

test_that("launch_rule_builder validates arguments before runtime launch", {
  expect_error(
    launch_rule_builder(output_dir = "", dry_run = TRUE),
    "output_dir"
  )
  expect_error(
    launch_rule_builder(output_dir = tempdir(), dry_run = NA),
    "dry_run"
  )
})
