# Edge test structure:
# 1) positive anomaly detection
# 2) boundary/non-trigger expectation
# 3) dirty input robustness (no runtime error)

test_that("AGG-004 flags high event/low support discrepancy", {
  rule <- rule_agg_004_event_support_rate_discrepancy(
    event_domain = "EVENTS",
    support_domain = "SUPPORT",
    group_var = "group_id",
    entity_var = "entity_id",
    event_high_threshold = 1,
    support_low_threshold = 0.5
  )

  events <- data.frame(
    id = 1:3,
    group_id = c("A", "A", "A"),
    entity_id = c("E1", "E1", "E1"),
    stringsAsFactors = FALSE
  )
  support <- data.frame(
    id = 1,
    group_id = "B",
    entity_id = "E2",
    stringsAsFactors = FALSE
  )

  ctx <- edge_make_context(
    current_domains = list(EVENTS = events, SUPPORT = support),
    current_key_vars = list(EVENTS = "id", SUPPORT = "id")
  )

  out <- run_rules(ctx, list(rule))
  edge_expect_rule_output(out, expected_id = "AGG-004", require_rows = TRUE)
  expect_true(any(out$rate > 1))
})

test_that("AGG-004 returns empty when support coverage is adequate", {
  rule <- rule_agg_004_event_support_rate_discrepancy(
    event_domain = "EVENTS",
    support_domain = "SUPPORT",
    group_var = "group_id",
    entity_var = "entity_id",
    event_high_threshold = 1,
    support_low_threshold = 0.5
  )

  events <- data.frame(
    id = 1:2,
    group_id = c("A", "A"),
    entity_id = c("E1", "E2"),
    stringsAsFactors = FALSE
  )
  support <- data.frame(
    id = 1:2,
    group_id = c("A", "A"),
    entity_id = c("E1", "E2"),
    stringsAsFactors = FALSE
  )

  ctx <- edge_make_context(
    current_domains = list(EVENTS = events, SUPPORT = support),
    current_key_vars = list(EVENTS = "id", SUPPORT = "id")
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("AGG-004 handles blank/NA keys without crashing", {
  rule <- rule_agg_004_event_support_rate_discrepancy(
    event_domain = "EVENTS",
    support_domain = "SUPPORT",
    group_var = "group_id",
    entity_var = "entity_id",
    event_high_threshold = 0,
    support_low_threshold = 1
  )

  events <- data.frame(
    id = 1:3,
    group_id = c("A", "", NA_character_),
    entity_id = c("E1", "", NA_character_),
    stringsAsFactors = FALSE
  )
  support <- data.frame(
    id = 1:2,
    group_id = c("A", " "),
    entity_id = c("E1", NA_character_),
    stringsAsFactors = FALSE
  )

  ctx <- edge_make_context(
    current_domains = list(EVENTS = events, SUPPORT = support),
    current_key_vars = list(EVENTS = "id", SUPPORT = "id")
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = "AGG-004")
})
