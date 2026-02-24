test_that("rule_r003_key_type_drift is template placeholder", {
  rule <- rule_r003_key_type_drift(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
