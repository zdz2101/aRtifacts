test_that("XDOM-008 flags demographic inconsistency", {
  expected_id <- "XDOM-008"
  rule <- rule_xdom_008_demographic_inconsistency(left_domain = "DM", right_domain = "VS", left_field = "SEX", right_field = "SEX")

  dm <- data.frame(USUBJID = "S1", SEX = "M", stringsAsFactors = FALSE)
  vs <- data.frame(USUBJID = "S1", SEX = "F", stringsAsFactors = FALSE)

  snap <- new_artifact_snapshot(domains = list(DM = new_domain_snapshot("DM", dm, key_vars = intersect(c("USUBJID"), names(dm))), VS = new_domain_snapshot("VS", vs, key_vars = intersect(c("USUBJID", "VISIT", "VSTESTCD"), names(vs)))))
  ctx <- new_rule_context(current = snap, previous = snap, options = list())

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(out$RULE_ID == expected_id))
})
