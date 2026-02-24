test_that("rule_r037_implausible_change returns expected object", {
  rule <- rule_r037_implausible_change(domain = "X", key_vars = c("ID"), visit_order_var = "VISITNUM", max_within_change = list(VAL = 5))
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R037"))
})

test_that("rule_r037_implausible_change flags expected records", {
  rule <- rule_r037_implausible_change(domain = "X", key_vars = c("ID"), visit_order_var = "VISITNUM", max_within_change = list(VAL = 5))

  df <- data.frame(
    ID=c("A","A","A"),
    USUBJID=c("S1","S1","S1"),
    SITEID=c("SITE01","SITE01","SITE01"),
    VISITNUM=c(1,2,3),
    VAL=c(10,30,31),
    stringsAsFactors=FALSE
  )
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
