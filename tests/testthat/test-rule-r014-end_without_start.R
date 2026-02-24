test_that("rule_r014_end_without_start returns expected object", {
  rule <- rule_r014_end_without_start(domain = "X", key_vars = c("ID"), start_end_pairs = list(c("STARTDT", "ENDDT")))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R014"))
})

test_that("rule_r014_end_without_start flags expected records", {
  rule <- rule_r014_end_without_start(domain = "X", key_vars = c("ID"), start_end_pairs = list(c("STARTDT", "ENDDT")))

  df <- data.frame(
    ID = c("A"),
    STARTDT = c(NA),
    ENDDT = c("2024-03-01"),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
