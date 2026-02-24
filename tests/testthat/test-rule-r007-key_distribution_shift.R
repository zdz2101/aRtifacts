test_that("rule_r007_key_distribution_shift returns expected diff-scope object", {
  rule <- rule_r007_key_distribution_shift(domain = "X", key_vars = c("customer"), dup_rate_threshold = 0.2)
  expect_false(is.null(rule))
  expect_true(startsWith(rule$id, "R007"))
  expect_identical(rule$scope, "diff")
  expect_true(is.function(rule$fn))
})

test_that("rule_r007_key_distribution_shift flags shifted key-level rates", {
  rule <- rule_r007_key_distribution_shift(domain = "X", key_vars = c("customer"), dup_rate_threshold = 0.2)

  old_df <- data.frame(order_id = 1:5, customer = c("A", "A", "B", "B", "B"), amount = c(1, 2, 3, 4, 5), stringsAsFactors = FALSE)
  new_df <- data.frame(order_id = 1:5, customer = c("A", "A", "A", "A", "B"), amount = c(1, 2, 3, 4, 5), stringsAsFactors = FALSE)
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- rule$fn(old_dom, new_dom)
  expect_named(out, c("KEY_VAR", "KEY_LEVEL", "OLD_RATE", "NEW_RATE", "ABS_DIFF"))
  expect_equal(nrow(out), 2L)
  expect_identical(out$KEY_LEVEL, c("A", "B"))
  expect_equal(out$OLD_RATE, c(0.4, 0.6))
  expect_equal(out$NEW_RATE, c(0.8, 0.2))
  expect_equal(out$ABS_DIFF, c(0.4, 0.4))
})
