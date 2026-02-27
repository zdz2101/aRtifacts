# Edge test structure:
# 1) positive anomaly detection
# 2) boundary/non-trigger expectation
# 3) dirty input robustness (no runtime error)

test_that("HYB-002 flags driver increase with metric worsening", {
  rule <- rule_hyb_002_driver_change_with_metric_worsening(
    driver_domain = "DRIVER",
    metric_domain = "METRIC",
    entity_var = "entity_id",
    driver_var = "driver_value",
    metric_var = "metric_value",
    driver_delta_min = 1,
    metric_delta_min = 1
  )

  drv_prev <- data.frame(id = 1, entity_id = "E1", driver_value = 1, stringsAsFactors = FALSE)
  drv_cur <- data.frame(id = 1, entity_id = "E1", driver_value = 3, stringsAsFactors = FALSE)
  met_prev <- data.frame(id = 1, entity_id = "E1", metric_value = 1, stringsAsFactors = FALSE)
  met_cur <- data.frame(id = 1, entity_id = "E1", metric_value = 3, stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(DRIVER = drv_cur, METRIC = met_cur),
    previous_domains = list(DRIVER = drv_prev, METRIC = met_prev),
    current_key_vars = list(DRIVER = "id", METRIC = "id"),
    previous_key_vars = list(DRIVER = "id", METRIC = "id")
  )

  out <- run_rules(ctx, list(rule))
  edge_expect_rule_output(out, expected_id = "HYB-002", require_rows = TRUE)
  expect_true(any(out$driver_delta >= 1))
  expect_true(any(out$metric_delta >= 1))
})

test_that("HYB-002 returns empty when worsening threshold is not met", {
  rule <- rule_hyb_002_driver_change_with_metric_worsening(
    driver_domain = "DRIVER",
    metric_domain = "METRIC",
    entity_var = "entity_id",
    driver_var = "driver_value",
    metric_var = "metric_value",
    driver_delta_min = 1,
    metric_delta_min = 1
  )

  drv_prev <- data.frame(id = 1, entity_id = "E1", driver_value = 1, stringsAsFactors = FALSE)
  drv_cur <- data.frame(id = 1, entity_id = "E1", driver_value = 2, stringsAsFactors = FALSE)
  met_prev <- data.frame(id = 1, entity_id = "E1", metric_value = 3, stringsAsFactors = FALSE)
  met_cur <- data.frame(id = 1, entity_id = "E1", metric_value = 2, stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(DRIVER = drv_cur, METRIC = met_cur),
    previous_domains = list(DRIVER = drv_prev, METRIC = met_prev),
    current_key_vars = list(DRIVER = "id", METRIC = "id"),
    previous_key_vars = list(DRIVER = "id", METRIC = "id")
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("HYB-002 handles non-numeric values without crashing", {
  rule <- rule_hyb_002_driver_change_with_metric_worsening(
    driver_domain = "DRIVER",
    metric_domain = "METRIC",
    entity_var = "entity_id",
    driver_var = "driver_value",
    metric_var = "metric_value",
    driver_delta_min = 0,
    metric_delta_min = 0
  )

  drv_prev <- data.frame(id = 1:2, entity_id = c("E1", "E2"), driver_value = c("x", "2"), stringsAsFactors = FALSE)
  drv_cur <- data.frame(id = 1:2, entity_id = c("E1", "E2"), driver_value = c("y", "3"), stringsAsFactors = FALSE)
  met_prev <- data.frame(id = 1:2, entity_id = c("E1", "E2"), metric_value = c("1", "z"), stringsAsFactors = FALSE)
  met_cur <- data.frame(id = 1:2, entity_id = c("E1", "E2"), metric_value = c("2", "w"), stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(DRIVER = drv_cur, METRIC = met_cur),
    previous_domains = list(DRIVER = drv_prev, METRIC = met_prev),
    current_key_vars = list(DRIVER = "id", METRIC = "id"),
    previous_key_vars = list(DRIVER = "id", METRIC = "id")
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = "HYB-002")
})
