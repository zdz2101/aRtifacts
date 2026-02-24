test_that("rule_r031_impossible_values returns expected object", {
  rule <- rule_r031_impossible_values(domain = "X", key_vars = c("ID"), value_ranges = list(VAL = c(0, 10)))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R031"))
})

test_that("rule_r031_impossible_values flags expected records", {
  rule <- rule_r031_impossible_values(domain = "X", key_vars = c("ID"), value_ranges = list(VAL = c(0, 10)))

  df <- data.frame(ID=c("A"), VAL=c(20), stringsAsFactors=FALSE)
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
