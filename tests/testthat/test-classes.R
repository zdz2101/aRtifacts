orders_df <- function() {
  data.frame(
    order_id = c(101L, 102L, 103L),
    customer = c("Alice", "Bob", "Carol"),
    amount = c(250, 480, 120),
    stringsAsFactors = FALSE
  )
}

orders_dom <- function() {
  new_domain_snapshot(
    name = "orders",
    data = orders_df(),
    key_vars = "order_id",
    build_id = "build-1"
  )
}

orders_art <- function(build_id = "build-1") {
  new_artifact_snapshot(
    domains = list(orders = orders_dom()),
    build_id = build_id
  )
}

rule_high_amount <- function() {
  new_flag_rule(
    id = "HIGH_AMOUNT",
    domain = "orders",
    severity = "warning",
    category = "review",
    owner_team = "Finance",
    fn = function(df) {
      out <- df[!is.na(df$amount) & df$amount > 400, , drop = FALSE]
      if (!nrow(out)) return(data.frame())
      out[, c("order_id", "amount"), drop = FALSE]
    }
  )
}

test_that("new_domain_snapshot() returns a DomainSnapshot", {
  dom <- orders_dom()
  expect_true(S7::S7_inherits(dom, DomainSnapshot))
})

test_that("new_domain_snapshot() stores properties correctly", {
  dom <- new_domain_snapshot(
    name         = "orders",
    data         = orders_df(),
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
  expect_identical(dom@data, orders_df())
})

test_that("new_domain_snapshot() errors when data is not a data.frame", {
  expect_error(
    new_domain_snapshot("x", data = list(a = 1), key_vars = "a"),
    regexp = "data.frame"
  )
})

test_that("new_domain_snapshot() errors when key_vars is empty", {
  expect_error(
    new_domain_snapshot("x", data = orders_df(), key_vars = character()),
    regexp = "key_vars"
  )
})

test_that("new_domain_snapshot() errors when key_vars not in data", {
  expect_error(
    new_domain_snapshot("x", data = orders_df(), key_vars = "NONEXISTENT"),
    regexp = "Missing key"
  )
})

test_that("new_domain_snapshot() errors when keys are not unique", {
  df <- rbind(orders_df(), orders_df()[1, ])  # duplicate order_id 101
  expect_error(
    new_domain_snapshot("x", data = df, key_vars = "order_id"),
    regexp = "unique"
  )
})

test_that("new_artifact_snapshot() returns an ArtifactSnapshot", {
  art <- orders_art()
  expect_true(S7::S7_inherits(art, ArtifactSnapshot))
})

test_that("new_artifact_snapshot() stores domains and metadata", {
  art <- new_artifact_snapshot(
    domains  = list(orders = orders_dom()),
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
  rule <- rule_high_amount()
  expect_true(S7::S7_inherits(rule, FlagRule))
})

test_that("new_flag_rule() stores properties correctly", {
  rule <- rule_high_amount()
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
