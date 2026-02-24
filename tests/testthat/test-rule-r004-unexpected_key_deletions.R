test_that("rule_r004_unexpected_key_deletions returns expected diff-scope object", {
  rule <- rule_r004_unexpected_key_deletions(domain = "X", key_vars = c("order_id"))
  expect_false(is.null(rule))
  expect_true(startsWith(rule$id, "R004"))
  expect_identical(rule$scope, "diff")
  expect_true(is.function(rule$fn))
})

test_that("rule_r004_unexpected_key_deletions flags unexpected key deletions", {
  rule <- rule_r004_unexpected_key_deletions(domain = "X", key_vars = c("order_id"))
  old_df <- data.frame(
    order_id = c(101L, 102L, 103L),
    customer = c("Alice", "Bob", "Carol"),
    amount = c(250, 480, 120),
    stringsAsFactors = FALSE
  )
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_df <- old_df[-1, , drop = FALSE]
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- rule$fn(old_dom, new_dom)
  expect_named(out, c("order_id"))
  expect_equal(nrow(out), 1L)
  expect_identical(out$order_id, 101L)
})
