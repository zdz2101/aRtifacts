test_that("rule_r034_suspicious_zeros returns expected object", {
  rule <- rule_r034_suspicious_zeros(domain = "X", numeric_vars = c("VAL"), zero_rate_threshold = 0.5)
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R034"))
})

test_that("rule_r034_suspicious_zeros flags expected records", {
  rule <- rule_r034_suspicious_zeros(domain = "X", numeric_vars = c("VAL"), zero_rate_threshold = 0.5)

  df <- data.frame(VAL=c(0,0,1), stringsAsFactors=FALSE)
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
