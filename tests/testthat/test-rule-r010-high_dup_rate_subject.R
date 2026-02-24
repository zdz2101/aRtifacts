test_that("rule_r010_high_dup_rate_subject returns expected object", {
  rule <- rule_r010_high_dup_rate_subject(domain = "X", key_vars = c("ID", "SEQ"), dup_rate_threshold = 0.4)
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R010"))
})

test_that("rule_r010_high_dup_rate_subject flags expected records", {
  rule <- rule_r010_high_dup_rate_subject(domain = "X", key_vars = c("ID", "SEQ"), dup_rate_threshold = 0.4)

  df <- data.frame(
    ID = c("A", "A", "B", "C"),
    SEQ = c(1, 1, 1, 1),
    USUBJID = c("SUBJ01", "SUBJ01", "SUBJ02", "SUBJ03"),
    SITEID = c("SITE01", "SITE01", "SITE01", "SITE02"),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
