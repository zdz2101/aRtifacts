test_that("new_domain_snapshot() returns a DomainSnapshot", {
  dom <- .dom_orders()
  expect_true(S7::S7_inherits(dom, DomainSnapshot))
})

test_that("new_domain_snapshot() stores properties correctly", {
  dom <- new_domain_snapshot(
    name         = "orders",
    data         = .df_orders(),
    key_vars     = "order_id",
    build_id     = "b1",
    spec_version = "v1",
    code_sha     = "abc123",
    inputs_hash  = "xyz"
  )
  expect_equal(dom@name, "orders")
  expect_equal(dom@key_vars, "order_id")
  expect_equal(dom@build_id, "b1")
  expect_equal(dom@spec_version, "v1")
  expect_equal(dom@code_sha, "abc123")
  expect_equal(dom@inputs_hash, "xyz")
  expect_identical(dom@data, .df_orders())
})

test_that("new_domain_snapshot() errors when data is not a data.frame", {
  expect_error(
    new_domain_snapshot("x", data = list(a = 1), key_vars = "a"),
    regexp = "data.frame"
  )
})

test_that("new_domain_snapshot() errors when key_vars is empty", {
  expect_error(
    new_domain_snapshot("x", data = .df_orders(), key_vars = character()),
    regexp = "key_vars"
  )
})

test_that("new_domain_snapshot() errors when key_vars not in data", {
  expect_error(
    new_domain_snapshot("x", data = .df_orders(), key_vars = "NONEXISTENT"),
    regexp = "Missing key"
  )
})

test_that("new_domain_snapshot() errors when keys are not unique", {
  df <- rbind(.df_orders(), .df_orders()[1, ])  # duplicate order_id 101
  expect_error(
    new_domain_snapshot("x", data = df, key_vars = "order_id"),
    regexp = "unique"
  )
})

test_that("new_artifact_snapshot() returns an ArtifactSnapshot", {
  art <- .art_orders()
  expect_true(S7::S7_inherits(art, ArtifactSnapshot))
})

test_that("new_artifact_snapshot() stores domains and metadata", {
  art <- new_artifact_snapshot(
    domains  = list(orders = .dom_orders()),
    build_id = "run-99"
  )
  expect_named(art@domains, "orders")
  expect_equal(art@build_id, "run-99")
  expect_true(S7::S7_inherits(art@domains[["orders"]], DomainSnapshot))
})

test_that("new_artifact_snapshot() errors when domains is empty", {
  expect_error(
    new_artifact_snapshot(domains = list()),
    regexp = "DomainSnapshot"
  )
})

test_that("new_artifact_snapshot() errors when a domain is not a DomainSnapshot", {
  expect_error(
    new_artifact_snapshot(domains = list(bad = data.frame())),
    regexp = "DomainSnapshot"
  )
})

test_that("new_flag_rule() returns a FlagRule", {
  rule <- .rule_high_amount()
  expect_true(S7::S7_inherits(rule, FlagRule))
})

test_that("new_flag_rule() stores properties correctly", {
  rule <- .rule_high_amount()
  expect_equal(rule@id, "HIGH_AMOUNT")
  expect_equal(rule@domain, "orders")
  expect_equal(rule@severity, "warning")
  expect_equal(rule@category, "review")
  expect_equal(rule@owner_team, "Finance")
  expect_true(is.function(rule@fn))
})

test_that("new_flag_rule() uses sensible defaults", {
  rule <- new_flag_rule(
    id = "R1", domain = "X",
    fn = function(df) data.frame()
  )
  expect_equal(rule@severity, "warning")
  expect_equal(rule@category, "quality")
  expect_equal(rule@owner_team, "Biostats")
})

test_that("new_flag_rule() errors when fn is not a function", {
  expect_error(
    new_flag_rule(id = "R1", domain = "X", fn = "not_a_function"),
    regexp = "fn"
  )
})
