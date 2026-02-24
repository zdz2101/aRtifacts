test_that("rule_r047_seq_gaps returns expected object", {
  rule <- rule_r047_seq_gaps(domain = "X", key_vars = c("ID"), seq_vars = c("SEQ"))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R047"))
})

test_that("rule_r047_seq_gaps flags expected records", {
  rule <- rule_r047_seq_gaps(domain = "X", key_vars = c("ID"), seq_vars = c("SEQ"))

  df <- data.frame(
    ID=c("A","B","C"),
    USUBJID=c("S1","S1","S1"),
    SITEID=c("SITE01","SITE01","SITE01"),
    SEQ=c(1,3,3),
    stringsAsFactors=FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
