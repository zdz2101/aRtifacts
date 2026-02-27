# Edge test structure:
# 1) positive anomaly detection
# 2) boundary/non-trigger expectation
# 3) dirty input robustness (no runtime error)

test_that("HYB-004 flags cross-domain risk escalation", {
  rule <- rule_hyb_004_cross_domain_risk_escalation(
    events_domain = "EVENTS",
    support_domain = "SUPPORT",
    metric_domain = "METRIC",
    group_var = "group_id",
    entity_var = "entity_id",
    metric_var = "metric_value",
    metric_high_cutoff = 100,
    risk_delta_threshold = 0.2
  )

  prev_events <- data.frame(id = 1, group_id = "A", entity_id = "E1", stringsAsFactors = FALSE)
  cur_events <- data.frame(id = 1:3, group_id = c("A", "A", "A"), entity_id = c("E1", "E1", "E1"), stringsAsFactors = FALSE)
  prev_support <- data.frame(id = 1, group_id = "A", entity_id = "E1", stringsAsFactors = FALSE)
  cur_support <- data.frame(id = 1, group_id = "A", entity_id = "E1", stringsAsFactors = FALSE)
  prev_metric <- data.frame(id = 1, group_id = "A", metric_value = 50, stringsAsFactors = FALSE)
  cur_metric <- data.frame(id = 1, group_id = "A", metric_value = 50, stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(EVENTS = cur_events, SUPPORT = cur_support, METRIC = cur_metric),
    previous_domains = list(EVENTS = prev_events, SUPPORT = prev_support, METRIC = prev_metric),
    current_key_vars = list(EVENTS = "id", SUPPORT = "id", METRIC = "id"),
    previous_key_vars = list(EVENTS = "id", SUPPORT = "id", METRIC = "id")
  )

  out <- run_rules(ctx, list(rule))
  edge_expect_rule_output(out, expected_id = "HYB-004", require_rows = TRUE)
  expect_true(any(out$risk_delta > 0.2))
})

test_that("HYB-004 returns empty when risk delta is below threshold", {
  rule <- rule_hyb_004_cross_domain_risk_escalation(
    events_domain = "EVENTS",
    support_domain = "SUPPORT",
    metric_domain = "METRIC",
    group_var = "group_id",
    entity_var = "entity_id",
    metric_var = "metric_value",
    metric_high_cutoff = 100,
    risk_delta_threshold = 0.2
  )

  events <- data.frame(id = 1:2, group_id = c("A", "A"), entity_id = c("E1", "E2"), stringsAsFactors = FALSE)
  support <- data.frame(id = 1:2, group_id = c("A", "A"), entity_id = c("E1", "E2"), stringsAsFactors = FALSE)
  metric <- data.frame(id = 1:2, group_id = c("A", "A"), metric_value = c(50, 60), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(EVENTS = events, SUPPORT = support, METRIC = metric),
    previous_domains = list(EVENTS = events, SUPPORT = support, METRIC = metric),
    current_key_vars = list(EVENTS = "id", SUPPORT = "id", METRIC = "id"),
    previous_key_vars = list(EVENTS = "id", SUPPORT = "id", METRIC = "id")
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("HYB-004 handles blank/NA groups without crashing", {
  rule <- rule_hyb_004_cross_domain_risk_escalation(
    events_domain = "EVENTS",
    support_domain = "SUPPORT",
    metric_domain = "METRIC",
    group_var = "group_id",
    entity_var = "entity_id",
    metric_var = "metric_value",
    metric_high_cutoff = 100,
    risk_delta_threshold = 0
  )

  prev_events <- data.frame(id = 1:2, group_id = c("A", NA_character_), entity_id = c("E1", "E2"), stringsAsFactors = FALSE)
  cur_events <- data.frame(id = 1:3, group_id = c("A", "", NA_character_), entity_id = c("E1", "E2", "E3"), stringsAsFactors = FALSE)
  prev_support <- data.frame(id = 1, group_id = "A", entity_id = "E1", stringsAsFactors = FALSE)
  cur_support <- data.frame(id = 1:2, group_id = c("A", " "), entity_id = c("E1", NA_character_), stringsAsFactors = FALSE)
  prev_metric <- data.frame(id = 1:2, group_id = c("A", NA_character_), metric_value = c(50, 200), stringsAsFactors = FALSE)
  cur_metric <- data.frame(id = 1:3, group_id = c("A", "", NA_character_), metric_value = c(50, 300, 400), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(EVENTS = cur_events, SUPPORT = cur_support, METRIC = cur_metric),
    previous_domains = list(EVENTS = prev_events, SUPPORT = prev_support, METRIC = prev_metric),
    current_key_vars = list(EVENTS = "id", SUPPORT = "id", METRIC = "id"),
    previous_key_vars = list(EVENTS = "id", SUPPORT = "id", METRIC = "id")
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = "HYB-004")
})
