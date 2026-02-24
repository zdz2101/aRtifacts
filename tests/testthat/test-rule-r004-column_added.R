test_that("rule_r004_column_added is template placeholder", {
  rule <- rule_r004_column_added(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
