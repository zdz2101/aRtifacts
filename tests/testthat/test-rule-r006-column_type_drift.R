test_that("rule_r006_column_type_drift returns expected diff-scope object", {
  rule <- rule_r006_column_type_drift(domain = "X", key_vars = c("order_id"))
  expect_false(is.null(rule))
  expect_true(startsWith(rule$id, "R006"))
  expect_identical(rule$scope, "diff")
  expect_true(is.function(rule$fn))
})

test_that("rule_r006_column_type_drift flags changed column types", {
  rule <- rule_r006_column_type_drift(domain = "X", key_vars = c("order_id"))
  old_df <- data.frame(
    order_id = c(101L, 102L, 103L),
    customer = c("Alice", "Bob", "Carol"),
    amount = c(250, 480, 120),
    stringsAsFactors = FALSE
  )
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_df <- old_df
  new_df$amount <- as.character(new_df$amount)
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- rule$fn(old_dom, new_dom)
  expect_named(out, c("column", "old_type", "new_type"))
  expect_equal(nrow(out), 1L)
  expect_identical(out$column, "amount")
  expect_identical(out$old_type, "numeric")
  expect_identical(out$new_type, "character")
})
