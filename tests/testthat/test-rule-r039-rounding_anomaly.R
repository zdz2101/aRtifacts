test_that("rule_r039_rounding_anomaly returns expected object", {
  rule <- rule_r039_rounding_anomaly(domain = "X", numeric_vars = c("VAL"), round_rate_threshold = 0.7)
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R039"))
})

test_that("rule_r039_rounding_anomaly flags expected records", {
  rule <- rule_r039_rounding_anomaly(domain = "X", numeric_vars = c("VAL"), round_rate_threshold = 0.7)

  df <- data.frame(VAL=c(5,10,15,20,21), stringsAsFactors=FALSE)
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
