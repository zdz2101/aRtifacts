test_that("rule_r008_key_level_outlier is template placeholder", {
  rule <- rule_r008_key_level_outlier(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
