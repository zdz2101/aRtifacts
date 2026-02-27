# Edge test structure:
# 1) positive anomaly detection
# 2) boundary/non-trigger expectation
# 3) dirty input robustness (no runtime error)

test_that("HYB-003 flags entities removed from primary but still present elsewhere", {
  rule <- rule_hyb_003_entity_removed_but_present_elsewhere(
    primary_domain = "PRIMARY",
    other_domain = "OTHER",
    entity_var = "entity_id"
  )

  primary_prev <- data.frame(id = 1:2, entity_id = c("E1", "E2"), stringsAsFactors = FALSE)
  primary_cur <- data.frame(id = 1, entity_id = "E1", stringsAsFactors = FALSE)
  other_cur <- data.frame(id = 1, entity_id = "E2", stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(PRIMARY = primary_cur, OTHER = other_cur),
    previous_domains = list(PRIMARY = primary_prev, OTHER = other_cur),
    current_key_vars = list(PRIMARY = "id", OTHER = "id"),
    previous_key_vars = list(PRIMARY = "id", OTHER = "id")
  )

  out <- run_rules(ctx, list(rule))
  edge_expect_rule_output(out, expected_id = "HYB-003", require_rows = TRUE)
  expect_true(any(out$entity_id == "E2"))
})

test_that("HYB-003 returns empty when removed entities are not elsewhere", {
  rule <- rule_hyb_003_entity_removed_but_present_elsewhere(
    primary_domain = "PRIMARY",
    other_domain = "OTHER",
    entity_var = "entity_id"
  )

  primary_prev <- data.frame(id = 1:2, entity_id = c("E1", "E2"), stringsAsFactors = FALSE)
  primary_cur <- data.frame(id = 1, entity_id = "E1", stringsAsFactors = FALSE)
  other_cur <- data.frame(id = 1, entity_id = "E1", stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(PRIMARY = primary_cur, OTHER = other_cur),
    previous_domains = list(PRIMARY = primary_prev, OTHER = other_cur),
    current_key_vars = list(PRIMARY = "id", OTHER = "id"),
    previous_key_vars = list(PRIMARY = "id", OTHER = "id")
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("HYB-003 handles blank/NA entities without crashing", {
  rule <- rule_hyb_003_entity_removed_but_present_elsewhere(
    primary_domain = "PRIMARY",
    other_domain = "OTHER",
    entity_var = "entity_id"
  )

  primary_prev <- data.frame(id = 1:3, entity_id = c("E1", "", NA_character_), stringsAsFactors = FALSE)
  primary_cur <- data.frame(id = 1, entity_id = "E1", stringsAsFactors = FALSE)
  other_cur <- data.frame(id = 1:2, entity_id = c(" ", NA_character_), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(PRIMARY = primary_cur, OTHER = other_cur),
    previous_domains = list(PRIMARY = primary_prev, OTHER = other_cur),
    current_key_vars = list(PRIMARY = "id", OTHER = "id"),
    previous_key_vars = list(PRIMARY = "id", OTHER = "id")
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = "HYB-003")
})
