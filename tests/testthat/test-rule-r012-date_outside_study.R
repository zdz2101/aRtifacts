test_that("rule_r012_date_outside_study returns expected object", {
  rule <- rule_r012_date_outside_study(domain = "X", key_vars = c("ID"), date_vars = c("DT"), study_start = "2020-01-01", study_end = "2025-12-31")
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R012"))
})

test_that("rule_r012_date_outside_study flags expected records", {
  rule <- rule_r012_date_outside_study(domain = "X", key_vars = c("ID"), date_vars = c("DT"), study_start = "2020-01-01", study_end = "2025-12-31")

  df <- data.frame(
    ID = c("A"),
    DT = c("2019-12-31"),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
