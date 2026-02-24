test_that("rule_r035_statistical_outliers returns expected object", {
  rule <- rule_r035_statistical_outliers(domain = "X", key_vars = c("ID"), numeric_vars = c("VAL"), iqr_multiplier = 1.5)
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R035"))
})

test_that("rule_r035_statistical_outliers flags expected records", {
  rule <- rule_r035_statistical_outliers(domain = "X", key_vars = c("ID"), numeric_vars = c("VAL"), iqr_multiplier = 1.5)

  df <- data.frame(ID=c("A","B","C","D","E"), VAL=c(10,10,11,11,100), stringsAsFactors=FALSE)
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
