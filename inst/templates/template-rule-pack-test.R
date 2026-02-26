# ---------------------------------------------------------------------------
# Test template for custom topology rule packs (aRtifacts core)
# ---------------------------------------------------------------------------
# Copy to tests/testthat/test-<your-pack>.R and replace function/IDs.

rule_by_id <- function(rules, id) {
  Filter(function(rule) identical(rule@id, id), rules)[[1]]
}

build_ctx <- function(df, domain = "PRIMARY") {
  dom <- new_domain_snapshot(
    name = domain,
    data = df,
    key_vars = "entity_id"
  )
  art <- new_artifact_snapshot(domains = stats::setNames(list(dom), domain))
  new_rule_context(current = art)
}

test_that("template pack returns CoreRule objects", {
  rules <- make_topology_pack_template(domain = "PRIMARY")

  expect_type(rules, "list")
  expect_true(length(rules) >= 1L)
  expect_true(all(vapply(rules, function(rule) S7::S7_inherits(rule, CoreRule), logical(1))))
})

test_that("TMP001_MEASURE_OVER_THRESHOLD flags rows over threshold", {
  rules <- make_topology_pack_template(domain = "PRIMARY", threshold = 10)

  df <- data.frame(
    entity_id = c("E1", "E2", "E3"),
    measure_value = c(5, 11, 8),
    stringsAsFactors = FALSE
  )

  ctx <- build_ctx(df, domain = "PRIMARY")
  out <- run_rules(ctx, list(rule_by_id(rules, "TMP001_MEASURE_OVER_THRESHOLD")))
  expect_equal(nrow(out), 1L)
  expect_identical(out$entity_id, "E2")
  expect_true(all(out$RULE_ID == "TMP001_MEASURE_OVER_THRESHOLD"))
})

test_that("TMP002_FLAG_WITHOUT_EVENT_DATE flags inconsistent rows", {
  rules <- make_topology_pack_template(domain = "PRIMARY")

  df <- data.frame(
    entity_id = c("E1", "E2"),
    event_flag = c("Y", "N"),
    event_date = c("", "2026-01-01"),
    stringsAsFactors = FALSE
  )

  ctx <- build_ctx(df, domain = "PRIMARY")
  out <- run_rules(ctx, list(rule_by_id(rules, "TMP002_FLAG_WITHOUT_EVENT_DATE")))
  expect_equal(nrow(out), 1L)
  expect_identical(out$entity_id, "E1")
  expect_true(all(out$RULE_ID == "TMP002_FLAG_WITHOUT_EVENT_DATE"))
})
