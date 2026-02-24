test_that("rule_r049_enrollment_assessment_mismatch is template placeholder", {
  rule <- rule_r049_enrollment_assessment_mismatch(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
