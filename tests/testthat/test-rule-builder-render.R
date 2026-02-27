make_rb_render_rule <- function(id, topology) {
  base <- list(
    id = id,
    title = paste("Rule", id),
    topology = topology,
    scope = "record",
    intent = "quality_screening",
    severity = "warning",
    owner_team = "Data Ops",
    handoff = list(
      objective = paste("Objective for", id),
      candidate_columns = c("entity_id", "value"),
      pseudo_logic = "Compare edge transitions.",
      edge_cases = c("0", "blank", "NA")
    )
  )

  switch(
    topology,
    A0 = c(base, list(
      domain = "PRIMARY",
      decision = list(is_longitudinal = FALSE, n_domains = 1, level = "schema", topology_override_used = FALSE)
    )),
    A1 = c(base, list(
      domain = "PRIMARY",
      decision = list(is_longitudinal = FALSE, n_domains = 1, level = "record", topology_override_used = FALSE)
    )),
    A2 = c(base, list(
      domain = "PRIMARY",
      decision = list(is_longitudinal = FALSE, n_domains = 1, level = "aggregate", topology_override_used = FALSE)
    )),
    A3 = c(base, list(
      domain = "",
      domains = c("PRIMARY", "REFERENCE"),
      decision = list(is_longitudinal = FALSE, n_domains = 2, level = "relational", topology_override_used = FALSE)
    )),
    A4 = c(base, list(
      domain = "",
      domains = c("PRIMARY", "REFERENCE"),
      decision = list(is_longitudinal = FALSE, n_domains = 2, level = "aggregate", topology_override_used = FALSE)
    )),
    A5 = c(base, list(
      domain = "PRIMARY",
      decision = list(is_longitudinal = TRUE, n_domains = 1, level = "record", topology_override_used = FALSE)
    )),
    A6 = c(base, list(
      domain = "PRIMARY",
      decision = list(is_longitudinal = TRUE, n_domains = 1, level = "aggregate", topology_override_used = FALSE)
    )),
    A7 = c(base, list(
      domain = "",
      domains = c("PRIMARY", "REFERENCE"),
      decision = list(is_longitudinal = TRUE, n_domains = 2, level = "record", topology_override_used = FALSE)
    )),
    stop("Unknown topology in test helper.")
  )
}

make_rb_render_spec <- function() {
  pack <- list(
    pack_name = "Render Smoke Pack",
    owner_team = "Data Ops",
    default_severity = "warning",
    default_intent = "quality_screening"
  )
  rules <- lapply(aRtifacts:::.rb_supported_topologies(), function(top) {
    make_rb_render_rule(paste0("RB-", top), top)
  })
  build_rule_pack_spec(pack = pack, rules = rules, validate = TRUE)
}

test_that("render_rule_pack_scaffold maps topology to exact fn signatures", {
  spec <- make_rb_render_spec()
  src <- render_rule_pack_scaffold(spec)

  expect_match(src, "fn = function\\(domain, context, rule\\)")
  expect_match(src, "fn = function\\(data, context, rule\\)")
  expect_match(src, "fn = function\\(domains, context, rule\\)")
  expect_match(src, "fn = function\\(previous, current, context, rule\\)")

  expect_match(src, "RB-A0")
  expect_match(src, "RB-A7")
})

test_that("render_rule_agent_prompt_bundle includes handoff context", {
  spec <- make_rb_render_spec()
  bundle <- render_rule_agent_prompt_bundle(spec)

  expect_match(bundle, "Agent Prompt Bundle")
  expect_match(bundle, "### Objective")
  expect_match(bundle, "### Candidate Columns")
  expect_match(bundle, "### Pseudo-Logic Notes")
  expect_match(bundle, "### Edge Cases To Cover")
  expect_match(bundle, "Payload:")
})

test_that("write helper requires confirmation and avoids overwrite collisions", {
  skip_if_not_installed("yaml")

  spec <- make_rb_render_spec()
  out_root <- tempfile("rule-builder-out-")
  dir.create(out_root, recursive = TRUE)

  expect_error(
    aRtifacts:::.rb_write_rule_builder_outputs(spec, out_root, confirm = FALSE),
    "explicit confirmation"
  )

  first <- aRtifacts:::.rb_write_rule_builder_outputs(spec, out_root, confirm = TRUE)
  expect_true(file.exists(first$spec_file))
  expect_true(file.exists(first$scaffold_file))
  expect_true(file.exists(first$prompt_file))

  second <- aRtifacts:::.rb_write_rule_builder_outputs(spec, out_root, confirm = TRUE)
  expect_false(identical(first$output_dir, second$output_dir))
  expect_true(file.exists(second$spec_file))
})
