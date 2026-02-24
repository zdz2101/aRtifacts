test_that("SNAP-009 flags distribution drift", {
  expected_id <- "SNAP-009"
  rule <- rule_snap_009_distribution_drift(domain = "ADSL", value_var = "AGE", ks_threshold = 0.2)

  prev_adsl <- data.frame(USUBJID = c("S1", "S2", "S3", "S4"), AGE = c(1, 1, 2, 2), stringsAsFactors = FALSE)
  cur_adsl <- data.frame(USUBJID = c("S1", "S2", "S3", "S4"), AGE = c(10, 10, 11, 11), stringsAsFactors = FALSE)

  previous <- new_artifact_snapshot(domains = list(ADSL = new_domain_snapshot("ADSL", prev_adsl, key_vars = intersect(c("USUBJID"), names(prev_adsl)))))
  current <- new_artifact_snapshot(domains = list(ADSL = new_domain_snapshot("ADSL", cur_adsl, key_vars = intersect(c("USUBJID"), names(cur_adsl)))))
  ctx <- new_rule_context(current = current, previous = previous, options = list())

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(out$RULE_ID == expected_id))
})
