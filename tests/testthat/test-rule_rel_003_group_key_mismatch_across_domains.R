# Edge test structure:
# 1) positive anomaly detection
# 2) boundary/non-trigger expectation
# 3) dirty input robustness (no runtime error)

test_that("REL-003 flags entity/group mismatch across domains", {
  rule <- rule_rel_003_group_key_mismatch_across_domains(
    left_domain = "LEFT",
    right_domain = "RIGHT",
    entity_var = "entity_id",
    left_group_var = "group_id",
    right_group_var = "group_id"
  )

  left <- data.frame(id = 1:2, entity_id = c("E1", "E2"), group_id = c("A", "B"), stringsAsFactors = FALSE)
  right <- data.frame(id = 1:2, entity_id = c("E1", "E2"), group_id = c("A", "C"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(LEFT = left, RIGHT = right),
    current_key_vars = list(LEFT = "id", RIGHT = "id")
  )

  out <- run_rules(ctx, list(rule))
  edge_expect_rule_output(out, expected_id = "REL-003", require_rows = TRUE)
  expect_true(any(out$entity_id == "E2"))
})

test_that("REL-003 returns empty when pairs are aligned", {
  rule <- rule_rel_003_group_key_mismatch_across_domains(
    left_domain = "LEFT",
    right_domain = "RIGHT",
    entity_var = "entity_id",
    left_group_var = "group_id",
    right_group_var = "group_id"
  )

  left <- data.frame(id = 1:2, entity_id = c("E1", "E2"), group_id = c("A", "B"), stringsAsFactors = FALSE)
  right <- data.frame(id = 1:2, entity_id = c("E1", "E2"), group_id = c("A", "B"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(LEFT = left, RIGHT = right),
    current_key_vars = list(LEFT = "id", RIGHT = "id")
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("REL-003 handles blank/NA keys without crashing", {
  rule <- rule_rel_003_group_key_mismatch_across_domains(
    left_domain = "LEFT",
    right_domain = "RIGHT",
    entity_var = "entity_id",
    left_group_var = "group_id",
    right_group_var = "group_id"
  )

  left <- data.frame(id = 1:3, entity_id = c("E1", "", NA_character_), group_id = c("A", "B", "C"), stringsAsFactors = FALSE)
  right <- data.frame(id = 1:3, entity_id = c("E1", " ", NA_character_), group_id = c("A", "D", "E"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(LEFT = left, RIGHT = right),
    current_key_vars = list(LEFT = "id", RIGHT = "id")
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = "REL-003")
})
