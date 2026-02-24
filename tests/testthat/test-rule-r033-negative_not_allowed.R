test_that("rule_r033_negative_not_allowed returns expected object", {
  rule <- rule_r033_negative_not_allowed(domain = "X", key_vars = c("ID"), non_negative_vars = c("VAL"))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R033"))
})

test_that("rule_r033_negative_not_allowed flags expected records", {
  rule <- rule_r033_negative_not_allowed(domain = "X", key_vars = c("ID"), non_negative_vars = c("VAL"))

  df <- data.frame(ID=c("A"), VAL=c(-1), stringsAsFactors=FALSE)
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
