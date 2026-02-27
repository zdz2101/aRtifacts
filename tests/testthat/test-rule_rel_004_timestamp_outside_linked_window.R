# Edge test structure:
# 1) positive anomaly detection
# 2) boundary/non-trigger expectation
# 3) dirty input robustness (no runtime error)

test_that("REL-004 flags event timestamps outside linked window", {
  rule <- rule_rel_004_timestamp_outside_linked_window(
    event_domain = "EVENTS",
    window_domain = "WINDOW",
    link_keys = "entity_id",
    event_time_var = "event_time",
    window_start_var = "window_start",
    window_end_var = "window_end",
    window_days = 0
  )

  events <- data.frame(id = 1, entity_id = "E1", event_time = "2024-02-10", stringsAsFactors = FALSE)
  window <- data.frame(id = 1, entity_id = "E1", window_start = "2024-01-01", window_end = "2024-01-31", stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(EVENTS = events, WINDOW = window),
    current_key_vars = list(EVENTS = "id", WINDOW = "id")
  )

  out <- run_rules(ctx, list(rule))
  edge_expect_rule_output(out, expected_id = "REL-004", require_rows = TRUE)
  expect_true(any(out$entity_id == "E1"))
})

test_that("REL-004 returns empty when event timestamps are in-window", {
  rule <- rule_rel_004_timestamp_outside_linked_window(
    event_domain = "EVENTS",
    window_domain = "WINDOW",
    link_keys = "entity_id",
    event_time_var = "event_time",
    window_start_var = "window_start",
    window_end_var = "window_end",
    window_days = 0
  )

  events <- data.frame(id = 1, entity_id = "E1", event_time = "2024-01-10", stringsAsFactors = FALSE)
  window <- data.frame(id = 1, entity_id = "E1", window_start = "2024-01-01", window_end = "2024-01-31", stringsAsFactors = FALSE)

  ctx <- edge_make_context(
    current_domains = list(EVENTS = events, WINDOW = window),
    current_key_vars = list(EVENTS = "id", WINDOW = "id")
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("REL-004 handles missing keys and malformed dates without crashing", {
  rule <- rule_rel_004_timestamp_outside_linked_window(
    event_domain = "EVENTS",
    window_domain = "WINDOW",
    link_keys = "entity_id",
    event_time_var = "event_time",
    window_start_var = "window_start",
    window_end_var = "window_end",
    window_days = 0
  )

  events <- data.frame(
    id = 1:3,
    entity_id = c(NA_character_, "", "E1"),
    event_time = c("not-a-date", "", "2024-01-10"),
    stringsAsFactors = FALSE
  )
  window <- data.frame(
    id = 1:3,
    entity_id = c(NA_character_, " ", "E1"),
    window_start = c("", "2024-01-01", "2024-01-01"),
    window_end = c("", "2024-01-31", "2024-01-31"),
    stringsAsFactors = FALSE
  )

  ctx <- edge_make_context(
    current_domains = list(EVENTS = events, WINDOW = window),
    current_key_vars = list(EVENTS = "id", WINDOW = "id")
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = "REL-004")
})
