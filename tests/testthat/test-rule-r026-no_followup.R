test_that("rule_r026_no_followup returns expected object", {
  rule <- rule_r026_no_followup(domain = "X", baseline_flag_var = "ABLFL")
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R026"))
})

test_that("rule_r026_no_followup flags expected records", {
  rule <- rule_r026_no_followup(domain = "X", baseline_flag_var = "ABLFL")

  df <- data.frame(
    USUBJID = c("S1", "S2", "S2"),
    ABLFL = c("Y", "Y", "N"),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
