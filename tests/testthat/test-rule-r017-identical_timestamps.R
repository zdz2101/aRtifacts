test_that("rule_r017_identical_timestamps returns expected object", {
  rule <- rule_r017_identical_timestamps(domain = "X", key_vars = c("ID"), date_vars = c("DT"))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R017"))
})

test_that("rule_r017_identical_timestamps flags expected records", {
  rule <- rule_r017_identical_timestamps(domain = "X", key_vars = c("ID"), date_vars = c("DT"))

  df <- data.frame(
    ID = c("A", "B"),
    DT = c("2024-06-01", "2024-06-01"),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
