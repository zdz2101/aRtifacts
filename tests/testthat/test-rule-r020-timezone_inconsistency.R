test_that("rule_r020_timezone_inconsistency is template placeholder", {
  rule <- rule_r020_timezone_inconsistency(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
