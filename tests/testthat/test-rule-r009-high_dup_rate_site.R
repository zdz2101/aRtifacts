test_that("rule_r009_high_dup_rate_site returns expected object", {
  rule <- rule_r009_high_dup_rate_site(domain = "X", key_vars = c("ID", "SEQ"), dup_rate_threshold = 0.4)
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R009"))
})

test_that("rule_r009_high_dup_rate_site flags expected records", {
  rule <- rule_r009_high_dup_rate_site(domain = "X", key_vars = c("ID", "SEQ"), dup_rate_threshold = 0.4)

  df <- data.frame(
    ID = c("A", "A", "B", "C", "D", "E"),
    SEQ = c(1, 1, 1, 1, 1, 1),
    USUBJID = c("S1", "S1", "S2", "S3", "S4", "S5"),
    SITEID = c("SITE01", "SITE01", "SITE01", "SITE02", "SITE02", "SITE02"),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
