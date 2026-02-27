# Edge test structure:
# 1) positive anomaly detection
# 2) boundary/non-trigger expectation
# 3) dirty input robustness (no runtime error)

test_that("HYB-001 flags new primary rows without support", {
  rule <- rule_hyb_001_new_primary_without_new_support(
    primary_domain = "PRIMARY",
    support_domain = "SUPPORT",
    primary_keys = c("entity_id", "record_id"),
    match_keys = "entity_id"
  )

  prev_primary <- data.frame(id = 1, entity_id = "E1", record_id = 1, stringsAsFactors = FALSE)
  cur_primary <- data.frame(id = 1:2, entity_id = c("E1", "E2"), record_id = c(1, 2), stringsAsFactors = FALSE)
  cur_support <- data.frame(id = 1, entity_id = "E1", support_id = 1, stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(PRIMARY = cur_primary, SUPPORT = cur_support),
    previous_domains = list(PRIMARY = prev_primary, SUPPORT = cur_support),
    current_key_vars = list(PRIMARY = "id", SUPPORT = "id"),
    previous_key_vars = list(PRIMARY = "id", SUPPORT = "id")
  )

  out <- run_rules(ctx, list(rule))
  edge_expect_rule_output(out, expected_id = "HYB-001", require_rows = TRUE)
  expect_true(any(out$entity_id == "E2"))
})

test_that("HYB-001 returns empty when support co-evolves", {
  rule <- rule_hyb_001_new_primary_without_new_support(
    primary_domain = "PRIMARY",
    support_domain = "SUPPORT",
    primary_keys = c("entity_id", "record_id"),
    match_keys = "entity_id"
  )

  prev_primary <- data.frame(id = 1, entity_id = "E1", record_id = 1, stringsAsFactors = FALSE)
  cur_primary <- data.frame(id = 1:2, entity_id = c("E1", "E2"), record_id = c(1, 2), stringsAsFactors = FALSE)
  cur_support <- data.frame(id = 1:2, entity_id = c("E1", "E2"), support_id = c(1, 2), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(PRIMARY = cur_primary, SUPPORT = cur_support),
    previous_domains = list(PRIMARY = prev_primary, SUPPORT = cur_support[1, , drop = FALSE]),
    current_key_vars = list(PRIMARY = "id", SUPPORT = "id"),
    previous_key_vars = list(PRIMARY = "id", SUPPORT = "id")
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("HYB-001 handles blank/NA keys without crashing", {
  rule <- rule_hyb_001_new_primary_without_new_support(
    primary_domain = "PRIMARY",
    support_domain = "SUPPORT",
    primary_keys = c("entity_id", "record_id"),
    match_keys = "entity_id"
  )

  prev_primary <- data.frame(id = 1, entity_id = "E1", record_id = 1, stringsAsFactors = FALSE)
  cur_primary <- data.frame(id = 1:3, entity_id = c("E1", "", NA_character_), record_id = c(1, 2, 3), stringsAsFactors = FALSE)
  cur_support <- data.frame(id = 1:2, entity_id = c("E1", " "), support_id = c(1, 2), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(PRIMARY = cur_primary, SUPPORT = cur_support),
    previous_domains = list(PRIMARY = prev_primary, SUPPORT = cur_support[1, , drop = FALSE]),
    current_key_vars = list(PRIMARY = "id", SUPPORT = "id"),
    previous_key_vars = list(PRIMARY = "id", SUPPORT = "id")
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = "HYB-001")
})
