test_that("rule_r046_child_without_parent is template placeholder", {
  rule <- rule_r046_child_without_parent(domain = "X", key_vars = c("ID"))
  expect_null(rule)
})
