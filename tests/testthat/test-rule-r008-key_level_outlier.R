test_that("rule_r008_key_level_outlier returns expected diff-scope object", {
  rule <- rule_r008_key_level_outlier(domain = "X", key_vars = c("customer"), dup_rate_threshold = 0.2)
  expect_false(is.null(rule))
  expect_true(startsWith(rule$id, "R008"))
  expect_identical(rule$scope, "diff")
  expect_true(is.function(rule$fn))
})

test_that("rule_r008_key_level_outlier flags levels with large positive rate jumps", {
  rule <- rule_r008_key_level_outlier(domain = "X", key_vars = c("customer"), dup_rate_threshold = 0.2)

  old_df <- data.frame(order_id = 1:10, customer = c(rep("A", 8), rep("B", 2)), amount = 1:10, stringsAsFactors = FALSE)
  new_df <- data.frame(order_id = 1:10, customer = c(rep("A", 3), rep("B", 7)), amount = 1:10, stringsAsFactors = FALSE)
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- rule$fn(old_dom, new_dom)
  expect_named(out, c("KEY_VAR", "KEY_LEVEL", "OLD_RATE", "NEW_RATE", "RATE_DIFF"))
  expect_equal(nrow(out), 1L)
  expect_identical(out$KEY_LEVEL, "B")
  expect_equal(out$OLD_RATE, 0.2)
  expect_equal(out$NEW_RATE, 0.7)
  expect_equal(out$RATE_DIFF, 0.5)
})
