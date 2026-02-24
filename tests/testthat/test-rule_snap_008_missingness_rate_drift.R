test_that("SNAP-008 flags missingness rate drift", {
  expected_id <- "SNAP-008"
  rule <- rule_snap_008_missingness_rate_drift(domain = "ADSL", group_var = "SITEID", value_var = "AGE", threshold_increase = 0.1)

  prev_adsl <- data.frame(USUBJID = c("S1", "S2"), SITEID = c("A", "A"), AGE = c(20, 21), stringsAsFactors = FALSE)
  cur_adsl <- data.frame(USUBJID = c("S1", "S2"), SITEID = c("A", "A"), AGE = c(NA, 21), stringsAsFactors = FALSE)

  previous <- new_artifact_snapshot(domains = list(ADSL = new_domain_snapshot("ADSL", prev_adsl, key_vars = intersect(c("USUBJID"), names(prev_adsl)))))
  current <- new_artifact_snapshot(domains = list(ADSL = new_domain_snapshot("ADSL", cur_adsl, key_vars = intersect(c("USUBJID"), names(cur_adsl)))))
  ctx <- new_rule_context(current = current, previous = previous, options = list())

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(out$RULE_ID == expected_id))
})
