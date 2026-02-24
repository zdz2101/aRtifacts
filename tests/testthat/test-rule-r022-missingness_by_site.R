test_that("rule_r022_missingness_by_site returns expected object", {
  rule <- rule_r022_missingness_by_site(domain = "X", group_var = "SITEID", missingness_threshold = 0.4)
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R022"))
})

test_that("rule_r022_missingness_by_site flags expected records", {
  rule <- rule_r022_missingness_by_site(domain = "X", group_var = "SITEID", missingness_threshold = 0.4)

  df <- data.frame(
    SITEID = c("S1", "S1", "S2"),
    VAL = c(NA, NA, 1),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
