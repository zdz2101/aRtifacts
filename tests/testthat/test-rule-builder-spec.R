make_rb_pack <- function() {
  list(
    pack_name = "Clinical Snapshot Checks",
    function_name = "",
    owner_team = "Data Ops",
    default_severity = "warning",
    default_intent = "quality_screening",
    notes = "Generated from tests"
  )
}

make_rb_rule <- function(id = "RB-001") {
  list(
    id = id,
    title = "Missing key",
    topology = "A1",
    scope = "record",
    intent = "quality_screening",
    domain = "PRIMARY",
    decision = list(
      is_longitudinal = FALSE,
      n_domains = 1,
      level = "record",
      topology_override_used = FALSE
    ),
    handoff = list(
      objective = "Flag rows with missing key values.",
      candidate_columns = c("entity_id"),
      pseudo_logic = "Detect blanks and NA in entity_id.",
      edge_cases = c("0", "blank", "NA")
    )
  )
}

test_that("build_rule_pack_spec resolves topology from decision shape", {
  rule <- make_rb_rule()
  rule$topology <- NULL

  spec <- build_rule_pack_spec(
    pack = make_rb_pack(),
    rules = list(rule)
  )

  expect_identical(spec$rules[[1]]$topology, "A1")
  expect_identical(spec$pack$pack_slug, "clinical-snapshot-checks")
  expect_true(grepl("^make_rule_pack_", spec$pack$function_name))
})

test_that("manual topology override is retained and marked in spec", {
  rule <- make_rb_rule()
  rule$topology <- "A7"
  rule$scope <- "snapshot"
  rule$domain <- ""
  rule$domains <- c("PRIMARY", "REFERENCE")
  rule$decision <- list(
    is_longitudinal = FALSE,
    n_domains = 1,
    level = "record",
    topology_override_used = TRUE
  )

  spec <- build_rule_pack_spec(
    pack = make_rb_pack(),
    rules = list(rule)
  )

  expect_identical(spec$rules[[1]]$topology, "A7")
  expect_true(isTRUE(spec$rules[[1]]$decision$topology_override_used))
})

test_that("build_rule_pack_spec enforces required validation rules", {
  rule <- make_rb_rule()

  dup <- make_rb_rule(id = "RB-001")
  expect_error(
    build_rule_pack_spec(pack = make_rb_pack(), rules = list(rule, dup)),
    "unique"
  )

  missing_domain <- make_rb_rule(id = "RB-002")
  missing_domain$domain <- ""
  expect_error(
    build_rule_pack_spec(pack = make_rb_pack(), rules = list(missing_domain)),
    "requires `domain`"
  )

  bad_cross <- make_rb_rule(id = "RB-003")
  bad_cross$topology <- "A3"
  bad_cross$domain <- ""
  bad_cross$domains <- "PRIMARY"
  bad_cross$decision <- list(
    is_longitudinal = FALSE,
    n_domains = 2,
    level = "relational",
    topology_override_used = FALSE
  )
  expect_error(
    build_rule_pack_spec(pack = make_rb_pack(), rules = list(bad_cross)),
    "at least two entries in `domains`"
  )

  bad_scope <- make_rb_rule(id = "RB-004")
  bad_scope$scope <- "row"
  expect_error(
    build_rule_pack_spec(pack = make_rb_pack(), rules = list(bad_scope)),
    "invalid scope"
  )

  a0_missing_domain <- make_rb_rule(id = "RB-006")
  a0_missing_domain$topology <- "A0"
  a0_missing_domain$domain <- ""
  a0_missing_domain$decision <- list(
    is_longitudinal = FALSE,
    n_domains = 1,
    level = "schema",
    topology_override_used = FALSE
  )
  expect_error(
    build_rule_pack_spec(pack = make_rb_pack(), rules = list(a0_missing_domain)),
    "requires `domain`"
  )
})

test_that("workspace helpers support add/edit/delete/reorder with ID protection", {
  r1 <- make_rb_rule("RB-001")
  r2 <- make_rb_rule("RB-002")
  r2$title <- "Second rule"

  rules <- aRtifacts:::.rb_upsert_workspace_rule(list(), r1)
  rules <- aRtifacts:::.rb_upsert_workspace_rule(rules, r2)
  expect_equal(length(rules), 2L)

  edited <- r2
  edited$title <- "Updated second"
  rules <- aRtifacts:::.rb_upsert_workspace_rule(rules, edited, edit_id = "RB-002")
  expect_identical(rules[[2]]$title, "Updated second")

  expect_error(
    aRtifacts:::.rb_upsert_workspace_rule(rules, make_rb_rule("RB-001")),
    "already exists"
  )

  moved <- aRtifacts:::.rb_move_workspace_rule(rules, "RB-002", direction = "up")
  expect_identical(moved[[1]]$id, "RB-002")

  trimmed <- aRtifacts:::.rb_delete_workspace_rule(moved, "RB-001")
  expect_identical(length(trimmed), 1L)
  expect_identical(trimmed[[1]]$id, "RB-002")
})

test_that("CSV header helper validates header presence", {
  with_header <- tempfile(fileext = ".csv")
  writeLines(c("entity_id,value", "E1,1"), con = with_header)
  expect_identical(
    aRtifacts:::.rb_read_csv_header(with_header),
    c("entity_id", "value")
  )

  no_header <- tempfile(fileext = ".csv")
  writeLines(c("1,2", "3,4"), con = no_header)
  expect_error(
    aRtifacts:::.rb_read_csv_header(no_header),
    "header row"
  )
})

test_that("domain mapping columns are retained in normalized spec", {
  rule <- make_rb_rule("RB-005")
  rule$domain_mapping <- list(
    domain = "PRIMARY",
    domains = c("PRIMARY", "REFERENCE"),
    columns = list(
      PRIMARY = c("entity_id", "value"),
      REFERENCE = c("entity_id", "group_id")
    )
  )

  spec <- build_rule_pack_spec(
    pack = make_rb_pack(),
    rules = list(rule)
  )

  expect_named(spec$rules[[1]]$domain_mapping$columns, c("PRIMARY", "REFERENCE"))
  expect_identical(
    spec$rules[[1]]$domain_mapping$columns$PRIMARY,
    c("entity_id", "value")
  )
})
