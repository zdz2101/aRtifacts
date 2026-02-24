test_that("rule_r018_future_dated returns expected object", {
  rule <- rule_r018_future_dated(domain = "X", key_vars = c("ID"), date_vars = c("DT"), cutoff_date = as.Date("2024-01-01"))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R018"))
})

test_that("rule_r018_future_dated flags expected records", {
  rule <- rule_r018_future_dated(domain = "X", key_vars = c("ID"), date_vars = c("DT"), cutoff_date = as.Date("2024-01-01"))

  df <- data.frame(
    ID = c("A"),
    DT = c("2026-01-01"),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
