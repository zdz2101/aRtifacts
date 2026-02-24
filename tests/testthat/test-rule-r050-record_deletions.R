test_that("rule_r050_record_deletions is template placeholder", {
  rule <- rule_r050_record_deletions(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
