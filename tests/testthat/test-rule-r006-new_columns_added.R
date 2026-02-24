test_that("rule_r006_new_columns_added returns expected diff-scope object", {
  rule <- rule_r006_new_columns_added(domain = "X", key_vars = c("order_id"))
  expect_false(is.null(rule))
  expect_true(startsWith(rule$id, "R006"))
  expect_identical(rule$scope, "diff")
  expect_true(is.function(rule$fn))
})

test_that("rule_r006_new_columns_added flags newly added columns", {
  rule <- rule_r006_new_columns_added(domain = "X", key_vars = c("order_id"))
  old_df <- data.frame(
    order_id = c(101L, 102L, 103L),
    customer = c("Alice", "Bob", "Carol"),
    amount = c(250, 480, 120),
    stringsAsFactors = FALSE
  )
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_df <- old_df
  new_df$site <- "SITE01"
  new_df$region <- "NA"
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- rule$fn(old_dom, new_dom)
  expect_named(out, c("column", "status"))
  expect_equal(nrow(out), 2L)
  expect_identical(out$column, c("region", "site"))
  expect_true(all(out$status == "added"))
})
