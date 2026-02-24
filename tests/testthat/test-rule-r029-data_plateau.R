test_that("rule_r029_data_plateau returns expected DiffFlagRule", {
  rule <- rule_r029_data_plateau(domain = "X", key_vars = c("ID"))
  expect_true(S7::S7_inherits(rule, DiffFlagRule))
  expect_true(startsWith(rule@id, "R029"))
  expect_true(is.function(rule@fn))
})

test_that("rule_r029_data_plateau flags unchanged cuts with zero key churn", {
  rule <- rule_r029_data_plateau(domain = "X", key_vars = c("ID"))

  old_df <- data.frame(ID = 1:10, VAL = 1:10, stringsAsFactors = FALSE)
  new_df <- old_df
  old_dom <- new_domain_snapshot("X", old_df, key_vars = "ID")
  new_dom <- new_domain_snapshot("X", new_df, key_vars = "ID")

  out <- rule@fn(old_dom, new_dom)
  expect_named(out, c("old_n_records", "new_n_records", "absolute_change", "key_churn_n"))
  expect_equal(nrow(out), 1L)
  expect_equal(out$absolute_change, 0L)
  expect_equal(out$key_churn_n, 0L)
})
