test_that("rule_r002_duplicate_keys returns expected object", {
  rule <- rule_r002_duplicate_keys(domain = "X", key_vars = c("ID", "SEQ"))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R002"))
})

test_that("rule_r002_duplicate_keys flags expected records", {
  rule <- rule_r002_duplicate_keys(domain = "X", key_vars = c("ID", "SEQ"))

  df <- data.frame(
    ID = c("A", "A", "B"),
    SEQ = c(1, 1, 2),
    USUBJID = c("S1", "S1", "S2"),
    SITEID = c("SITE01", "SITE01", "SITE02"),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
