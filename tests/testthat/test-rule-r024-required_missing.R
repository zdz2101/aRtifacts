test_that("rule_r024_required_missing returns expected object", {
  rule <- rule_r024_required_missing(domain = "X", required_vars = c("ID", "VAL"))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R024"))
})

test_that("rule_r024_required_missing flags expected records", {
  rule <- rule_r024_required_missing(domain = "X", required_vars = c("ID", "VAL"))

  df <- data.frame(
    ID = c("A", "B"),
    VAL = c(1, NA),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
