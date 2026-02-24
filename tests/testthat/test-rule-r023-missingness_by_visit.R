test_that("rule_r023_missingness_by_visit returns expected object", {
  rule <- rule_r023_missingness_by_visit(domain = "X", visit_var = "VISIT", missingness_threshold = 0.4)
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R023"))
})

test_that("rule_r023_missingness_by_visit flags expected records", {
  rule <- rule_r023_missingness_by_visit(domain = "X", visit_var = "VISIT", missingness_threshold = 0.4)

  df <- data.frame(
    VISIT = c("V1", "V1", "V2"),
    VAL = c(NA, NA, 1),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
