# Edge test structure:
# 1) positive anomaly detection
# 2) boundary/non-trigger expectation
# 3) dirty input robustness (no runtime error)

test_that("AGG-002 flags group count mismatch", {
  rule <- rule_agg_002_group_count_mismatch(
    left_domain = "LEFT",
    right_domain = "RIGHT",
    group_var = "group_id",
    tolerance = 0
  )

  left <- data.frame(id = 1:3, group_id = c("A", "A", "B"), stringsAsFactors = FALSE)
  right <- data.frame(id = 1:3, group_id = c("A", "B", "B"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(LEFT = left, RIGHT = right),
    current_key_vars = list(LEFT = "id", RIGHT = "id")
  )

  out <- run_rules(ctx, list(rule))
  edge_expect_rule_output(out, expected_id = "AGG-002", require_rows = TRUE)
  expect_true(any(out$delta_abs > 0))
})

test_that("AGG-002 returns empty when group counts align", {
  rule <- rule_agg_002_group_count_mismatch(
    left_domain = "LEFT",
    right_domain = "RIGHT",
    group_var = "group_id",
    tolerance = 0
  )

  left <- data.frame(id = 1:3, group_id = c("A", "A", "B"), stringsAsFactors = FALSE)
  right <- data.frame(id = 1:3, group_id = c("A", "A", "B"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(LEFT = left, RIGHT = right),
    current_key_vars = list(LEFT = "id", RIGHT = "id")
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("AGG-002 handles blank/NA groups without crashing", {
  rule <- rule_agg_002_group_count_mismatch(
    left_domain = "LEFT",
    right_domain = "RIGHT",
    group_var = "group_id",
    tolerance = 0
  )

  left <- data.frame(id = 1:3, group_id = c("A", "", NA_character_), stringsAsFactors = FALSE)
  right <- data.frame(id = 1:3, group_id = c("A", "A", "B"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(LEFT = left, RIGHT = right),
    current_key_vars = list(LEFT = "id", RIGHT = "id")
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = "AGG-002")
})
