test_that("ID-004 flags high duplicate rate by subject", {
  expected_id <- "ID-004"
  rule <- rule_id_004_high_duplicate_rate_by_subject(domain = "AE", key_vars = c("USUBJID", "VISIT", "AETERM"))

  ae <- data.frame(
    USUBJID = c("S1", "S1", "S2"),
    VISIT = c("V1", "V1", "V1"),
    AETERM = c("HEADACHE", "HEADACHE", "NAUSEA"),
    stringsAsFactors = FALSE
  )

  snap <- new_artifact_snapshot(domains = list(AE = new_domain_snapshot("AE", transform(ae, .ROWID = seq_len(nrow(ae))), key_vars = ".ROWID")))
  ctx <- new_rule_context(current = snap, previous = snap, options = list())

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(out$RULE_ID == expected_id))
})
