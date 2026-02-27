# Edge test structure:
# 1) positive anomaly detection
# 2) boundary/non-trigger expectation
# 3) dirty input robustness (no runtime error)

test_that("SCH-003 flags shared-column type drift", {
  rule <- rule_sch_003_column_type_drift(domain = "PRIMARY")

  prev <- data.frame(id = 1:2, a = c(1, 2), stringsAsFactors = FALSE)
  cur <- data.frame(id = 1:2, a = c("1", "2"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(PRIMARY = cur),
    previous_domains = list(PRIMARY = prev),
    current_key_vars = list(PRIMARY = "id"),
    previous_key_vars = list(PRIMARY = "id")
  )

  out <- run_rules(ctx, list(rule))
  edge_expect_rule_output(out, expected_id = "SCH-003", require_rows = TRUE)
  expect_true("a" %in% out$column)
})

test_that("SCH-003 returns empty when shared types are unchanged", {
  rule <- rule_sch_003_column_type_drift(domain = "PRIMARY")

  cur <- data.frame(id = 1:2, a = c(1, 2), stringsAsFactors = FALSE)
  ctx <- edge_make_context(
    current_domains = list(PRIMARY = cur),
    previous_domains = list(PRIMARY = cur),
    current_key_vars = list(PRIMARY = "id"),
    previous_key_vars = list(PRIMARY = "id")
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("SCH-003 handles dirty values without crashing", {
  rule <- rule_sch_003_column_type_drift(domain = "PRIMARY")

  prev <- data.frame(id = 1:3, a = c("", NA, "x"), stringsAsFactors = FALSE)
  cur <- data.frame(id = 1:3, a = c("", " ", NA), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(PRIMARY = cur),
    previous_domains = list(PRIMARY = prev),
    current_key_vars = list(PRIMARY = "id"),
    previous_key_vars = list(PRIMARY = "id")
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = "SCH-003")
})
