test_that("rule_r042_derived_inconsistent is template placeholder", {
  rule <- rule_r042_derived_inconsistent(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
