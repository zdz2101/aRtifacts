test_that("GRP-005 flags skew anomaly", {
  expected_id <- "GRP-005"
  rule <- rule_grp_005_distribution_skew_anomaly(domain = "LB", value_var = "LBSTRESN", threshold = 0.5)

  lb <- data.frame(
    USUBJID = c("S1", "S2", "S3", "S4"),
    LBSEQ = c(1, 1, 1, 1),
    LBSTRESN = c(1, 1, 1, 100),
    stringsAsFactors = FALSE
  )

  snap <- new_artifact_snapshot(domains = list(LB = new_domain_snapshot("LB", lb, key_vars = intersect(c("USUBJID", "LBSEQ"), names(lb)))))
  ctx <- new_rule_context(current = snap, previous = snap, options = list())

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(out$RULE_ID == expected_id))
})
