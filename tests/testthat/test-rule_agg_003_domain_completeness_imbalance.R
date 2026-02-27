# Edge test structure:
# 1) positive anomaly detection
# 2) boundary/non-trigger expectation
# 3) dirty input robustness (no runtime error)

test_that("AGG-003 flags cross-domain completeness imbalance", {
  rule <- rule_agg_003_domain_completeness_imbalance(
    domains_set = c("PRIMARY", "SUPPORT", "AUX"),
    group_var = "group_id",
    threshold_max_gap = 0.2
  )

  primary <- data.frame(id = 1:4, group_id = c("A", "A", "A", "B"), stringsAsFactors = FALSE)
  support <- data.frame(id = 1:2, group_id = c("A", "B"), stringsAsFactors = FALSE)
  aux <- data.frame(id = 1:2, group_id = c("B", "B"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(PRIMARY = primary, SUPPORT = support, AUX = aux),
    current_key_vars = list(PRIMARY = "id", SUPPORT = "id", AUX = "id")
  )

  out <- run_rules(ctx, list(rule))
  edge_expect_rule_output(out, expected_id = "AGG-003", require_rows = TRUE)
  expect_true(any(out$rate_gap > 0.2))
})

test_that("AGG-003 returns empty when domain balance gap is below threshold", {
  rule <- rule_agg_003_domain_completeness_imbalance(
    domains_set = c("PRIMARY", "SUPPORT", "AUX"),
    group_var = "group_id",
    threshold_max_gap = 0.8
  )

  primary <- data.frame(id = 1:2, group_id = c("A", "B"), stringsAsFactors = FALSE)
  support <- data.frame(id = 1:2, group_id = c("A", "B"), stringsAsFactors = FALSE)
  aux <- data.frame(id = 1:2, group_id = c("A", "B"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(PRIMARY = primary, SUPPORT = support, AUX = aux),
    current_key_vars = list(PRIMARY = "id", SUPPORT = "id", AUX = "id")
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("AGG-003 handles blank/NA groups without crashing", {
  rule <- rule_agg_003_domain_completeness_imbalance(
    domains_set = c("PRIMARY", "SUPPORT", "AUX"),
    group_var = "group_id",
    threshold_max_gap = 0.2
  )

  primary <- data.frame(id = 1:3, group_id = c("A", "", NA_character_), stringsAsFactors = FALSE)
  support <- data.frame(id = 1:3, group_id = c("A", "A", "B"), stringsAsFactors = FALSE)
  aux <- data.frame(id = 1:3, group_id = c("A", "B", "B"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(PRIMARY = primary, SUPPORT = support, AUX = aux),
    current_key_vars = list(PRIMARY = "id", SUPPORT = "id", AUX = "id")
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = "AGG-003")
})
