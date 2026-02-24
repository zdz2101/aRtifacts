test_that("rule_r032_unit_mismatch is template placeholder", {
  rule <- rule_r032_unit_mismatch(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
