test_that("rule_r029_data_plateau is template placeholder", {
  rule <- rule_r029_data_plateau(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
