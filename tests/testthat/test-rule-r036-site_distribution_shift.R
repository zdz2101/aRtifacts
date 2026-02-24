test_that("rule_r036_site_distribution_shift is template placeholder", {
  rule <- rule_r036_site_distribution_shift(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
