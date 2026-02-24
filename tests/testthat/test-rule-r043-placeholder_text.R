test_that("rule_r043_placeholder_text returns expected object", {
  rule <- rule_r043_placeholder_text(domain = "X", key_vars = c("ID"))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R043"))
})

test_that("rule_r043_placeholder_text flags expected records", {
  rule <- rule_r043_placeholder_text(domain = "X", key_vars = c("ID"))

  df <- data.frame(ID=c("A"), COMMENT=c("N/A"), stringsAsFactors=FALSE)
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
