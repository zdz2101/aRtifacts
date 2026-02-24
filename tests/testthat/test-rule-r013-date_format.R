test_that("rule_r013_date_format returns expected object", {
  rule <- rule_r013_date_format(domain = "X", key_vars = c("ID"), date_vars = c("DT"))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R013"))
})

test_that("rule_r013_date_format flags expected records", {
  rule <- rule_r013_date_format(domain = "X", key_vars = c("ID"), date_vars = c("DT"))

  df <- data.frame(
    ID = c("A"),
    DT = c("2024-1-01"),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
