test_that("rule_r040_impossible_combos returns expected object", {
  rule <- rule_r040_impossible_combos(domain = "X", key_vars = c("ID"), impossible_combos = list(c(SEX = "M", PREGNANT = "Y")))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R040"))
})

test_that("rule_r040_impossible_combos flags expected records", {
  rule <- rule_r040_impossible_combos(domain = "X", key_vars = c("ID"), impossible_combos = list(c(SEX = "M", PREGNANT = "Y")))

  df <- data.frame(ID=c("A"), SEX=c("M"), PREGNANT=c("Y"), stringsAsFactors=FALSE)
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
