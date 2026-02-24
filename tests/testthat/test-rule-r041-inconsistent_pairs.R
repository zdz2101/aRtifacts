test_that("rule_r041_inconsistent_pairs returns expected object", {
  rule <- rule_r041_inconsistent_pairs(domain = "X", key_vars = c("ID"), inconsistent_pairs = list(c(A = "Y", B = "N")))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R041"))
})

test_that("rule_r041_inconsistent_pairs flags expected records", {
  rule <- rule_r041_inconsistent_pairs(domain = "X", key_vars = c("ID"), inconsistent_pairs = list(c(A = "Y", B = "N")))

  df <- data.frame(ID=c("A"), A=c("Y"), B=c("N"), stringsAsFactors=FALSE)
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
