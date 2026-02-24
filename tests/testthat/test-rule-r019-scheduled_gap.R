test_that("rule_r019_scheduled_gap is template placeholder", {
  rule <- rule_r019_scheduled_gap(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
