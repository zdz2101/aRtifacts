test_that("rule_r021_missingness_overall returns expected object", {
  rule <- rule_r021_missingness_overall(domain = "X", missingness_threshold = 0.4)
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R021"))
})

test_that("rule_r021_missingness_overall flags expected records", {
  rule <- rule_r021_missingness_overall(domain = "X", missingness_threshold = 0.4)

  df <- data.frame(
    A = c(1, NA, NA),
    B = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
