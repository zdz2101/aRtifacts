test_that("rule_r007_key_distribution_shift is template placeholder", {
  rule <- rule_r007_key_distribution_shift(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
