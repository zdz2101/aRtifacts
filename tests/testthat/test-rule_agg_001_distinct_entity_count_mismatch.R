# Edge test structure:
# 1) positive anomaly detection
# 2) boundary/non-trigger expectation
# 3) dirty input robustness (no runtime error)

test_that("AGG-001 flags distinct entity count mismatch", {
  rule <- rule_agg_001_distinct_entity_count_mismatch(
    left_domain = "LEFT",
    right_domain = "RIGHT",
    entity_var = "entity_id",
    tolerance = 0
  )

  left <- data.frame(id = 1:2, entity_id = c("E1", "E2"), stringsAsFactors = FALSE)
  right <- data.frame(id = 1, entity_id = "E1", stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(LEFT = left, RIGHT = right),
    current_key_vars = list(LEFT = "id", RIGHT = "id")
  )

  out <- run_rules(ctx, list(rule))
  edge_expect_rule_output(out, expected_id = "AGG-001", require_rows = TRUE)
  expect_true(any(out$delta_abs > 0))
})

test_that("AGG-001 returns empty within tolerance", {
  rule <- rule_agg_001_distinct_entity_count_mismatch(
    left_domain = "LEFT",
    right_domain = "RIGHT",
    entity_var = "entity_id",
    tolerance = 0
  )

  left <- data.frame(id = 1:2, entity_id = c("E1", "E2"), stringsAsFactors = FALSE)
  right <- data.frame(id = 1:2, entity_id = c("E1", "E2"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(LEFT = left, RIGHT = right),
    current_key_vars = list(LEFT = "id", RIGHT = "id")
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("AGG-001 handles blanks/NA without crashing", {
  rule <- rule_agg_001_distinct_entity_count_mismatch(
    left_domain = "LEFT",
    right_domain = "RIGHT",
    entity_var = "entity_id",
    tolerance = 0
  )

  left <- data.frame(id = 1:3, entity_id = c("E1", "", NA_character_), stringsAsFactors = FALSE)
  right <- data.frame(id = 1:2, entity_id = c("E1", " "), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(LEFT = left, RIGHT = right),
    current_key_vars = list(LEFT = "id", RIGHT = "id")
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = "AGG-001")
})
