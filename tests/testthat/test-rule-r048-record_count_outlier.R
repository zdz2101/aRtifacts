test_that("rule_r048_record_count_outlier returns expected object", {
  rule <- rule_r048_record_count_outlier(domain = "X", record_count_iqr_multiplier = 1.5)
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R048"))
})

test_that("rule_r048_record_count_outlier flags expected records", {
  rule <- rule_r048_record_count_outlier(domain = "X", record_count_iqr_multiplier = 1.5)

  df <- data.frame(
    USUBJID = c(rep("S1", 1), rep("S2", 2), rep("S3", 3), rep("S4", 4), rep("S5", 20)),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
