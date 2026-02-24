test_that("rule_r027_missing_baseline returns expected object", {
  rule <- rule_r027_missing_baseline(domain = "X", baseline_flag_var = "ABLFL")
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R027"))
})

test_that("rule_r027_missing_baseline flags expected records", {
  rule <- rule_r027_missing_baseline(domain = "X", baseline_flag_var = "ABLFL")

  df <- data.frame(
    USUBJID = c("S1", "S1", "S2"),
    ABLFL = c("Y", "N", "N"),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
