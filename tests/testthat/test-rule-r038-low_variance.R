test_that("rule_r038_low_variance returns expected object", {
  rule <- rule_r038_low_variance(domain = "X", numeric_vars = c("VAL"), low_cv_threshold = 0.01)
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R038"))
})

test_that("rule_r038_low_variance flags expected records", {
  rule <- rule_r038_low_variance(domain = "X", numeric_vars = c("VAL"), low_cv_threshold = 0.01)

  df <- data.frame(VAL=c(100,100.2,99.9,100.1), stringsAsFactors=FALSE)
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
