test_that("DE-002 flags out-of-range numeric values", {
  expected_id <- "DE-002"
  rule <- rule_de_002_out_of_range_numeric_values(domain = "ADSL", value_var = "AGE", lower = 18, upper = 65)

  adsl <- data.frame(
    USUBJID = c("S1", "S2"),
    AGE = c(30, 99),
    stringsAsFactors = FALSE
  )

  snap <- new_artifact_snapshot(domains = list(ADSL = new_domain_snapshot("ADSL", adsl, key_vars = intersect(c("USUBJID"), names(adsl)))))
  ctx <- new_rule_context(current = snap, previous = snap, options = list())

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(out$RULE_ID == expected_id))
})
