test_that("rule_r025_domain_empty returns expected object", {
  rule <- rule_r025_domain_empty(domain = "X")
  expect_false(is.null(rule))
  expect_true(startsWith(rule@id, "R025"))
})

test_that("rule_r025_domain_empty flags expected records", {
  rule <- rule_r025_domain_empty(domain = "X")

  df <- data.frame(ID = character(), stringsAsFactors = FALSE)
  out <- rule@fn(df)

  expect_gt(nrow(out), 0L)
})
