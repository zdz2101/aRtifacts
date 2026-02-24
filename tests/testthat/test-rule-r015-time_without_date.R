test_that("rule_r015_time_without_date returns expected object", {
  rule <- rule_r015_time_without_date(domain = "X", key_vars = c("ID"), time_date_pairs = list(c("TM", "DT")))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R015"))
})

test_that("rule_r015_time_without_date flags expected records", {
  rule <- rule_r015_time_without_date(domain = "X", key_vars = c("ID"), time_date_pairs = list(c("TM", "DT")))

  df <- data.frame(
    ID = c("A"),
    TM = c("08:00"),
    DT = c(""),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
