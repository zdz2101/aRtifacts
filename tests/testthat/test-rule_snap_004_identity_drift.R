test_that("SNAP-004 flags identity drift", {
  expected_id <- "SNAP-004"
  rule <- rule_snap_004_identity_drift(domain = "AE", stable_fields = c("USUBJID", "AETERM", "AESTDTC"), identity_fields = c("AESEQ"))

  prev_ae <- data.frame(USUBJID = "S1", AETERM = "HEADACHE", AESTDTC = "2024-01-01", AESEQ = 1, stringsAsFactors = FALSE)
  cur_ae <- data.frame(USUBJID = "S1", AETERM = "HEADACHE", AESTDTC = "2024-01-01", AESEQ = 99, stringsAsFactors = FALSE)

  previous <- new_artifact_snapshot(domains = list(AE = new_domain_snapshot("AE", prev_ae, key_vars = intersect(c("USUBJID", "AESEQ"), names(prev_ae)))))
  current <- new_artifact_snapshot(domains = list(AE = new_domain_snapshot("AE", cur_ae, key_vars = intersect(c("USUBJID", "AESEQ"), names(cur_ae)))))
  ctx <- new_rule_context(current = current, previous = previous, options = list())

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(out$RULE_ID == expected_id))
})
