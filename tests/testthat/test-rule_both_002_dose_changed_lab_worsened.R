test_that("BOTH-002 flags dose increase with lab worsening", {
  expected_id <- "BOTH-002"
  rule <- rule_both_002_dose_changed_lab_worsened()

  prev_ex <- data.frame(USUBJID = "S1", EXDOSE = 10, stringsAsFactors = FALSE)
  cur_ex <- data.frame(USUBJID = "S1", EXDOSE = 20, stringsAsFactors = FALSE)
  prev_lb <- data.frame(USUBJID = "S1", LBSTRESN = 50, stringsAsFactors = FALSE)
  cur_lb <- data.frame(USUBJID = "S1", LBSTRESN = 100, stringsAsFactors = FALSE)

  previous <- new_artifact_snapshot(domains = list(EX = new_domain_snapshot("EX", prev_ex, key_vars = intersect(c("USUBJID", "EXSEQ"), names(prev_ex))), LB = new_domain_snapshot("LB", prev_lb, key_vars = intersect(c("USUBJID", "LBSEQ"), names(prev_lb)))))
  current <- new_artifact_snapshot(domains = list(EX = new_domain_snapshot("EX", cur_ex, key_vars = intersect(c("USUBJID", "EXSEQ"), names(cur_ex))), LB = new_domain_snapshot("LB", cur_lb, key_vars = intersect(c("USUBJID", "LBSEQ"), names(cur_lb)))))
  ctx <- new_rule_context(current = current, previous = previous, options = list())

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(out$RULE_ID == expected_id))
})
