test_that("rule_r011_start_after_end returns expected object", {
  rule <- rule_r011_start_after_end(domain = "X", key_vars = c("ID"), start_end_pairs = list(c("STARTDT", "ENDDT")))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R011"))
})

test_that("rule_r011_start_after_end flags expected records", {
  rule <- rule_r011_start_after_end(domain = "X", key_vars = c("ID"), start_end_pairs = list(c("STARTDT", "ENDDT")))

  df <- data.frame(
    ID = c("A"),
    STARTDT = c("2024-02-10"),
    ENDDT = c("2024-02-01"),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
