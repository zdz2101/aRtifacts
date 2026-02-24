test_that("rule_r007_columns_removed returns expected DiffFlagRule", {
  rule <- rule_r007_columns_removed(domain = "X", key_vars = c("order_id"), dup_rate_threshold = 0.2)
  expect_true(S7::S7_inherits(rule, DiffFlagRule))
  expect_true(startsWith(rule@id, "R007"))
  expect_true(is.function(rule@fn))
})

test_that("rule_r007_columns_removed flags removed columns", {
  rule <- rule_r007_columns_removed(domain = "X", key_vars = c("order_id"), dup_rate_threshold = 0.2)

  old_df <- data.frame(order_id = 1:5, customer = c("A", "A", "B", "B", "B"), amount = c(1, 2, 3, 4, 5), stringsAsFactors = FALSE)
  new_df <- old_df[, c("order_id"), drop = FALSE]
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- rule@fn(old_dom, new_dom)
  expect_named(out, c("column", "status"))
  expect_equal(nrow(out), 2L)
  expect_identical(out$column, c("amount", "customer"))
  expect_true(all(out$status == "removed"))
})
