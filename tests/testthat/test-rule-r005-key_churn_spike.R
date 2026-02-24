test_that("rule_r005_key_churn_spike returns expected DiffFlagRule", {
  rule <- rule_r005_key_churn_spike(domain = "X", key_vars = c("order_id"))
  expect_true(S7::S7_inherits(rule, DiffFlagRule))
  expect_true(startsWith(rule@id, "R005"))
  expect_true(is.function(rule@fn))
})

test_that("rule_r005_key_churn_spike flags key churn spikes", {
  rule <- rule_r005_key_churn_spike(domain = "X", key_vars = c("order_id"))
  old_df <- data.frame(order_id = 1:10, customer = rep("A", 10), amount = rep(1, 10), stringsAsFactors = FALSE)
  new_df <- data.frame(order_id = 6:15, customer = rep("A", 10), amount = rep(1, 10), stringsAsFactors = FALSE)
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- rule@fn(old_dom, new_dom)
  expect_named(out, c("old_n_keys", "new_n_keys", "added_n", "removed_n", "churn_n", "churn_rate", "threshold"))
  expect_equal(nrow(out), 1L)
  expect_equal(out$added_n, 5L)
  expect_equal(out$removed_n, 5L)
  expect_equal(out$churn_rate, 1)
})
