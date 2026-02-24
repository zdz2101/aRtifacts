test_that("rule_r030_completion_rate_outlier is template placeholder", {
  rule <- rule_r030_completion_rate_outlier(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
