test_that("BOTH-004 flags risk score escalation trend", {
  expected_id <- "BOTH-004"
  rule <- rule_both_004_risk_score_escalation_trend(domains_set = c("AE", "EX", "LB"), group_var = "SITEID", subject_var = "USUBJID", lb_var = "LBSTRESN", lb_high_cutoff = 50, risk_delta_threshold = 0.5)

  prev_ae <- data.frame(SITEID = c("A", "A"), USUBJID = c("S1", "S2"), AESEQ = c(1, 1), stringsAsFactors = FALSE)
  cur_ae <- data.frame(SITEID = c("A", "A", "A", "A"), USUBJID = c("S1", "S1", "S2", "S2"), AESEQ = c(1, 2, 1, 2), stringsAsFactors = FALSE)
  prev_ex <- data.frame(SITEID = c("A", "A"), USUBJID = c("S1", "S2"), EXSEQ = c(1, 1), stringsAsFactors = FALSE)
  cur_ex <- data.frame(SITEID = c("A", "A", "A", "A"), USUBJID = c("S1", "S1", "S2", "S2"), EXSEQ = c(1, 2, 1, 2), stringsAsFactors = FALSE)
  prev_lb <- data.frame(SITEID = c("A", "A"), USUBJID = c("S1", "S2"), LBSEQ = c(1, 1), LBSTRESN = c(40, 45), stringsAsFactors = FALSE)
  cur_lb <- data.frame(SITEID = c("A", "A"), USUBJID = c("S1", "S2"), LBSEQ = c(1, 1), LBSTRESN = c(90, 95), stringsAsFactors = FALSE)

  previous <- new_artifact_snapshot(domains = list(AE = new_domain_snapshot("AE", prev_ae, key_vars = intersect(c("USUBJID", "AESEQ"), names(prev_ae))), EX = new_domain_snapshot("EX", prev_ex, key_vars = intersect(c("USUBJID", "EXSEQ"), names(prev_ex))), LB = new_domain_snapshot("LB", prev_lb, key_vars = intersect(c("USUBJID", "LBSEQ"), names(prev_lb)))))
  current <- new_artifact_snapshot(domains = list(AE = new_domain_snapshot("AE", cur_ae, key_vars = intersect(c("USUBJID", "AESEQ"), names(cur_ae))), EX = new_domain_snapshot("EX", cur_ex, key_vars = intersect(c("USUBJID", "EXSEQ"), names(cur_ex))), LB = new_domain_snapshot("LB", cur_lb, key_vars = intersect(c("USUBJID", "LBSEQ"), names(cur_lb)))))
  ctx <- new_rule_context(current = current, previous = previous, options = list())

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(out$RULE_ID == expected_id))
})
