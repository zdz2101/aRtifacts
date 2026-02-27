load_guided_module <- function() {
  app_dir <- aRtifacts:::.rb_resolve_app_dir()
  source(file.path(app_dir, "modules", "mod_guided_builder.R"), local = parent.frame())
}

test_that("guided topology helpers map decision answers to A0-A7", {
  expect_identical(
    aRtifacts:::.rb_guided_resolve_topology(FALSE, "1", "schema"),
    "A0"
  )
  expect_identical(
    aRtifacts:::.rb_guided_resolve_topology(FALSE, "1", "record"),
    "A1"
  )
  expect_identical(
    aRtifacts:::.rb_guided_resolve_topology(FALSE, "1", "aggregate"),
    "A2"
  )
  expect_identical(
    aRtifacts:::.rb_guided_resolve_topology(FALSE, "2+", "relational"),
    "A3"
  )
  expect_identical(
    aRtifacts:::.rb_guided_resolve_topology(FALSE, "2+", "aggregate"),
    "A4"
  )
  expect_identical(
    aRtifacts:::.rb_guided_resolve_topology(TRUE, "1", "record"),
    "A5"
  )
  expect_identical(
    aRtifacts:::.rb_guided_resolve_topology(TRUE, "1", "aggregate"),
    "A6"
  )
  expect_identical(
    aRtifacts:::.rb_guided_resolve_topology(TRUE, "2+", ""),
    "A7"
  )
})

test_that("guided helper supports manual override and next rule IDs", {
  expect_identical(
    aRtifacts:::.rb_guided_resolve_topology(
      is_longitudinal = FALSE,
      n_domain_bucket = "1",
      check_kind = "record",
      topology_override_used = TRUE,
      topology_override = "A6"
    ),
    "A6"
  )

  expect_identical(
    aRtifacts:::.rb_guided_next_rule_id(list()),
    "RB-001"
  )
  expect_identical(
    aRtifacts:::.rb_guided_next_rule_id(list(list(id = "RB-001"), list(id = "RB-009"))),
    "RB-010"
  )
  expect_identical(
    aRtifacts:::.rb_guided_next_rule_id(list(list(id = "X-1"), list(id = "ABC"))),
    "RB-003"
  )
})

test_that("guided module updates resolved topology live in step 2", {
  skip_if_not_installed("shiny")
  load_guided_module()

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
    output_dir = tempdir(),
    last_write = NULL
  )

  shiny::testServer(
    app = mod_guided_builder_server,
    args = list(workspace = workspace, dry_run = TRUE),
    expr = {
      session$setInputs(
        pack_name = "Topology Mapping Pack",
        owner_team = "Data Operations",
        default_severity = "warning",
        default_intent = "quality_screening",
        pack_notes = "",
        pack_function_name = ""
      )
      session$setInputs(start_rule_1 = 1)
      expect_identical(as.integer(output$guided_debug_step), 2L)

      session$setInputs(q_is_longitudinal = "no", q_n_domain_bucket = "1", q_check_kind = "schema")
      expect_identical(as.character(output$guided_debug_topology), "A0")

      session$setInputs(q_n_domain_bucket = "2+")
      expect_identical(as.character(output$guided_debug_topology), "A3")

      session$setInputs(q_check_kind = "aggregate")
      expect_identical(as.character(output$guided_debug_topology), "A4")

      session$setInputs(q_is_longitudinal = "yes", q_n_domain_bucket = "1", q_check_kind = "record")
      expect_identical(as.character(output$guided_debug_topology), "A5")

      session$setInputs(q_n_domain_bucket = "2+")
      expect_identical(as.character(output$guided_debug_topology), "A7")
    }
  )
})

test_that("guided module enforces pack gate and supports add-another loop", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("yaml")
  load_guided_module()

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
    output_dir = tempdir(),
    last_write = NULL
  )

  shiny::testServer(
    app = mod_guided_builder_server,
    args = list(workspace = workspace, dry_run = TRUE),
    expr = {
      expect_identical(as.integer(output$guided_debug_step), 1L)

      session$setInputs(start_rule_1 = 1)
      expect_identical(as.integer(output$guided_debug_step), 1L)

      session$setInputs(
        pack_name = "Drug Induced Liver Injury Checks",
        owner_team = "Clinical Safety",
        default_severity = "warning",
        default_intent = "quality_screening",
        pack_notes = "guided test",
        pack_function_name = ""
      )
      session$setInputs(start_rule_1 = 2)
      expect_identical(as.integer(output$guided_debug_step), 2L)

      session$setInputs(q_is_longitudinal = "yes", q_n_domain_bucket = "2+")
      session$setInputs(topology_next = 1)
      expect_identical(as.integer(output$guided_debug_step), 3L)
      expect_identical(as.character(output$guided_debug_topology), "A7")

      step3_ui <- as.character(output$guided_step_body)
      expect_true(any(grepl("Optional Domain Context", step3_ui, fixed = TRUE)))
      expect_true(any(grepl("Advanced Rule Inputs", step3_ui, fixed = TRUE)))

      session$setInputs(
        rule_id = "RB-001",
        rule_title = "Cross-domain longitudinal consistency",
        rule_objective = "Flag inconsistent state changes across domains.",
        rule_domains_text = "PRIMARY,REFERENCE",
        rule_edge_cases = "0, blank, whitespace, NA",
        rule_scope = "record",
        rule_intent = "quality_screening",
        rule_severity = "warning",
        rule_owner_team = "Clinical Safety"
      )
      session$setInputs(rule_core_next = 1)
      expect_identical(as.integer(output$guided_debug_step), 4L)

      session$setInputs(confirm_add_rule = 1)
      expect_identical(as.integer(output$guided_debug_rule_count), 1L)

      session$setInputs(modal_add_another = 1)
      expect_identical(as.integer(output$guided_debug_step), 2L)
    }
  )
})

test_that("guided module can finish pack and quick-edit core fields", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("yaml")
  load_guided_module()

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
    output_dir = tempdir(),
    last_write = NULL
  )

  shiny::testServer(
    app = mod_guided_builder_server,
    args = list(workspace = workspace, dry_run = TRUE),
    expr = {
      session$setInputs(
        pack_name = "DILI Checks",
        owner_team = "Safety",
        default_severity = "warning",
        default_intent = "quality_screening",
        pack_notes = "",
        pack_function_name = ""
      )
      session$setInputs(start_rule_1 = 1)

      session$setInputs(q_is_longitudinal = "no", q_n_domain_bucket = "1", q_check_kind = "record")
      session$setInputs(topology_next = 1)

      session$setInputs(
        rule_id = "RB-001",
        rule_title = "Missing keys",
        rule_objective = "Find missing key rows",
        rule_primary_domain = "PRIMARY",
        rule_edge_cases = "0, blank, NA",
        rule_scope = "record",
        rule_intent = "quality_screening",
        rule_severity = "warning",
        rule_owner_team = "Safety"
      )
      session$setInputs(rule_core_next = 1)
      session$setInputs(confirm_add_rule = 1)
      session$setInputs(modal_finish_pack = 1)
      expect_identical(as.integer(output$guided_debug_step), 5L)

      session$setInputs(quick_rule_id = "RB-001")
      session$setInputs(quick_load = 1)
      session$setInputs(quick_title = "Missing key fields (edited)")
      session$setInputs(quick_save = 1)

      expect_identical(workspace$rules[[1]]$title, "Missing key fields (edited)")
    }
  )
})
