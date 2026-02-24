test_that("rule_r016_visit_order returns expected object", {
  rule <- rule_r016_visit_order(domain = "X", key_vars = c("ID"), visit_order_var = "VISITNUM")
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R016"))
})

test_that("rule_r016_visit_order flags expected records", {
  rule <- rule_r016_visit_order(domain = "X", key_vars = c("ID"), visit_order_var = "VISITNUM")

  df <- data.frame(
    ID = c("A", "A", "A"),
    USUBJID = c("S1", "S1", "S1"),
    SITEID = c("SITE01", "SITE01", "SITE01"),
    VISITNUM = c(1, 3, 2),
    stringsAsFactors = FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
