# Edge test structure:
# 1) positive anomaly detection
# 2) boundary/non-trigger expectation
# 3) dirty input robustness (no runtime error)

test_that("REL-002 flags orphan child keys", {
  rule <- rule_rel_002_orphan_child_records(
    child_domain = "CHILD",
    parent_domain = "PARENT",
    parent_keys = "entity_id"
  )

  child <- data.frame(id = 1:2, entity_id = c("E1", "E2"), stringsAsFactors = FALSE)
  parent <- data.frame(id = 1, entity_id = "E1", stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(CHILD = child, PARENT = parent),
    current_key_vars = list(CHILD = "id", PARENT = "id")
  )

  out <- run_rules(ctx, list(rule))
  edge_expect_rule_output(out, expected_id = "REL-002", require_rows = TRUE)
  expect_true(any(out$entity_id == "E2"))
})

test_that("REL-002 returns empty when all children have parents", {
  rule <- rule_rel_002_orphan_child_records(
    child_domain = "CHILD",
    parent_domain = "PARENT",
    parent_keys = "entity_id"
  )

  child <- data.frame(id = 1:2, entity_id = c("E1", "E2"), stringsAsFactors = FALSE)
  parent <- data.frame(id = 1:2, entity_id = c("E1", "E2"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(CHILD = child, PARENT = parent),
    current_key_vars = list(CHILD = "id", PARENT = "id")
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("REL-002 handles blank/NA keys without crashing", {
  rule <- rule_rel_002_orphan_child_records(
    child_domain = "CHILD",
    parent_domain = "PARENT",
    parent_keys = "entity_id"
  )

  child <- data.frame(id = 1:3, entity_id = c("E1", "", NA_character_), stringsAsFactors = FALSE)
  parent <- data.frame(id = 1:2, entity_id = c("E1", " "), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(CHILD = child, PARENT = parent),
    current_key_vars = list(CHILD = "id", PARENT = "id")
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = "REL-002")
})
