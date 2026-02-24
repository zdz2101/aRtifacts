test_that("rule_r028_late_arrival_spike is template placeholder", {
  rule <- rule_r028_late_arrival_spike(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
