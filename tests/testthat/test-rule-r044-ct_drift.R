test_that("rule_r044_ct_drift is template placeholder", {
  rule <- rule_r044_ct_drift(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
