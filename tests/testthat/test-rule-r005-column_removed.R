test_that("rule_r005_column_removed is template placeholder", {
  rule <- rule_r005_column_removed(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
