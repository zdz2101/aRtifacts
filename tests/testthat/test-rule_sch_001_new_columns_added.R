# Edge test structure:
# 1) positive anomaly detection
# 2) boundary/non-trigger expectation
# 3) dirty input robustness (no runtime error)

test_that("SCH-001 flags newly added columns", {
  rule <- rule_sch_001_new_columns_added(domain = "PRIMARY")

  prev <- data.frame(id = 1:2, a = c(1, 2), stringsAsFactors = FALSE)
  cur <- data.frame(id = 1:2, a = c(1, 2), b = c("x", "y"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(PRIMARY = cur),
    previous_domains = list(PRIMARY = prev),
    current_key_vars = list(PRIMARY = "id"),
    previous_key_vars = list(PRIMARY = "id")
  )

  out <- run_rules(ctx, list(rule))
  edge_expect_rule_output(out, expected_id = "SCH-001", require_rows = TRUE)
  expect_true("b" %in% out$column)
})

test_that("SCH-001 returns empty when schema is unchanged", {
  rule <- rule_sch_001_new_columns_added(domain = "PRIMARY")

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

test_that("SCH-001 handles dirty values without crashing", {
  rule <- rule_sch_001_new_columns_added(domain = "PRIMARY")

  prev <- data.frame(id = 1:3, a = c("", NA, "x"), stringsAsFactors = FALSE)
  cur <- data.frame(id = 1:3, a = c("", NA, "x"), b = c(" ", "Y", NA), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(PRIMARY = cur),
    previous_domains = list(PRIMARY = prev),
    current_key_vars = list(PRIMARY = "id"),
    previous_key_vars = list(PRIMARY = "id")
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = "SCH-001")
})
