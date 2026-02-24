test_that("rule_r028_late_arrival_spike returns expected DiffFlagRule", {
  rule <- rule_r028_late_arrival_spike(domain = "X", key_vars = c("ID"), missingness_threshold = 0.1)
  expect_true(S7::S7_inherits(rule, DiffFlagRule))
  expect_true(startsWith(rule@id, "R028"))
  expect_true(is.function(rule@fn))
})

test_that("rule_r028_late_arrival_spike flags volume spikes", {
  rule <- rule_r028_late_arrival_spike(domain = "X", key_vars = c("ID"), missingness_threshold = 0.3)

  old_df <- data.frame(ID = 1:10, VAL = 1:10, stringsAsFactors = FALSE)
  new_df <- data.frame(ID = 1:16, VAL = 1:16, stringsAsFactors = FALSE)
  old_dom <- new_domain_snapshot("X", old_df, key_vars = "ID")
  new_dom <- new_domain_snapshot("X", new_df, key_vars = "ID")

  out <- rule@fn(old_dom, new_dom)
  expect_named(out, c("old_n_records", "new_n_records", "absolute_change", "relative_change", "threshold"))
  expect_equal(nrow(out), 1L)
  expect_equal(out$absolute_change, 6L)
  expect_true(out$relative_change > out$threshold)
})
