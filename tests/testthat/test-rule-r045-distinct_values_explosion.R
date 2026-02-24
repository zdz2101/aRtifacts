test_that("rule_r045_distinct_values_explosion returns expected object", {
  rule <- rule_r045_distinct_values_explosion(domain = "X", categorical_vars = c("PARAM"), max_distinct_values = 3L)
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R045"))
})

test_that("rule_r045_distinct_values_explosion flags expected records", {
  rule <- rule_r045_distinct_values_explosion(domain = "X", categorical_vars = c("PARAM"), max_distinct_values = 3L)

  df <- data.frame(PARAM=c("A","B","C","D"), stringsAsFactors=FALSE)
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
