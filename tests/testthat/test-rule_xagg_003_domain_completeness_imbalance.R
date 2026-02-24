test_that("XAGG-003 flags completeness imbalance", {
  expected_id <- "XAGG-003"
  rule <- rule_xagg_003_domain_completeness_imbalance(domains_set = c("VS", "LB", "EG"), group_var = "SITEID", threshold_max_gap = 0.2)

  vs <- data.frame(USUBJID = c("S1", "S2", "S3", "S4"), VISIT = c("W1", "W1", "W2", "W1"), VSTESTCD = c("SYSBP", "SYSBP", "SYSBP", "SYSBP"), SITEID = c("A", "A", "A", "B"), stringsAsFactors = FALSE)
  lb <- data.frame(USUBJID = c("S1", "S4", "S5"), LBSEQ = c(1, 1, 1), SITEID = c("A", "B", "B"), stringsAsFactors = FALSE)
  eg <- data.frame(USUBJID = "S1", EGSEQ = 1, SITEID = "A", stringsAsFactors = FALSE)

  snap <- new_artifact_snapshot(domains = list(VS = new_domain_snapshot("VS", vs, key_vars = intersect(c("USUBJID", "VISIT", "VSTESTCD"), names(vs))), LB = new_domain_snapshot("LB", lb, key_vars = intersect(c("USUBJID", "LBSEQ"), names(lb))), EG = new_domain_snapshot("EG", eg, key_vars = intersect(c("USUBJID", "EGSEQ"), names(eg)))))
  ctx <- new_rule_context(current = snap, previous = snap, options = list())

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(out$RULE_ID == expected_id))
})
