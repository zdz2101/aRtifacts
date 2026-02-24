test_that("XDOM-004 flags CM outside AE window", {
  expected_id <- "XDOM-004"
  rule <- rule_xdom_004_cm_outside_ae_window(window_days = 0)

  cm <- data.frame(USUBJID = "S1", CMSEQ = 1, CMSTDTC = "2024-02-01", stringsAsFactors = FALSE)
  ae <- data.frame(USUBJID = "S1", AESTDTC = "2024-01-01", AEENDTC = "2024-01-05", stringsAsFactors = FALSE)

  snap <- new_artifact_snapshot(domains = list(CM = new_domain_snapshot("CM", cm, key_vars = intersect(c("USUBJID", "CMSEQ"), names(cm))), AE = new_domain_snapshot("AE", ae, key_vars = intersect(c("USUBJID", "AESEQ"), names(ae)))))
  ctx <- new_rule_context(current = snap, previous = snap, options = list())

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(out$RULE_ID == expected_id))
})
