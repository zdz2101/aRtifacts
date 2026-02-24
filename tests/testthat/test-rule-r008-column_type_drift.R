test_that("rule_r008_column_type_drift returns expected DiffFlagRule", {
  rule <- rule_r008_column_type_drift(domain = "X", key_vars = c("order_id"), dup_rate_threshold = 0.2)
  expect_true(S7::S7_inherits(rule, DiffFlagRule))
  expect_true(startsWith(rule@id, "R008"))
  expect_true(is.function(rule@fn))
})

test_that("rule_r008_column_type_drift flags column type drift", {
  rule <- rule_r008_column_type_drift(domain = "X", key_vars = c("order_id"), dup_rate_threshold = 0.2)

  old_df <- data.frame(order_id = 1:5, customer = c("A", "A", "B", "B", "B"), amount = c(1, 2, 3, 4, 5), stringsAsFactors = FALSE)
  new_df <- old_df
  new_df$amount <- as.character(new_df$amount)
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- rule@fn(old_dom, new_dom)
  expect_named(out, c("column", "old_type", "new_type"))
  expect_equal(nrow(out), 1L)
  expect_identical(out$column, "amount")
  expect_identical(out$old_type, "numeric")
  expect_identical(out$new_type, "character")
})
