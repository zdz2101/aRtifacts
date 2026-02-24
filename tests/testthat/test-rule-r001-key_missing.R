test_that("rule_r001_key_missing returns expected object", {
  rule <- rule_r001_key_missing(domain = "X", key_vars = c("ID", "SEQ"))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R001"))
})

test_that("rule_r001_key_missing flags expected records", {
  rule <- rule_r001_key_missing(domain = "X", key_vars = c("ID", "SEQ"))

  df <- data.frame(
    ID = c("A", "A", "B"),
    SEQ = c(1, NA, 2),
    USUBJID = c("S1", "S1", "S2"),
    SITEID = c("SITE01", "SITE01", "SITE02"),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
