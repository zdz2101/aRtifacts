test_that("SNAP-002 flags unexpected record changes", {
  expected_id <- "SNAP-002"
  rule <- rule_snap_002_record_changed_unexpectedly(domain = "ADSL", keys = c("USUBJID"), monitored_fields = c("AGE"))

  prev_adsl <- data.frame(USUBJID = "S1", AGE = 30, stringsAsFactors = FALSE)
  cur_adsl <- data.frame(USUBJID = "S1", AGE = 40, stringsAsFactors = FALSE)

  previous <- new_artifact_snapshot(domains = list(ADSL = new_domain_snapshot("ADSL", prev_adsl, key_vars = intersect(c("USUBJID"), names(prev_adsl)))))
  current <- new_artifact_snapshot(domains = list(ADSL = new_domain_snapshot("ADSL", cur_adsl, key_vars = intersect(c("USUBJID"), names(cur_adsl)))))
  ctx <- new_rule_context(current = current, previous = previous, options = list())

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(out$RULE_ID == expected_id))
})
