test_that("DE-003 flags invalid categorical code", {
  expected_id <- "DE-003"
  rule <- rule_de_003_invalid_categorical_code(domain = "DM", value_var = "SEX", allowed_levels = c("M", "F"))

  dm <- data.frame(
    USUBJID = c("S1", "S2"),
    SEX = c("M", "X"),
    stringsAsFactors = FALSE
  )

  snap <- new_artifact_snapshot(domains = list(DM = new_domain_snapshot("DM", dm, key_vars = intersect(c("USUBJID"), names(dm)))))
  ctx <- new_rule_context(current = snap, previous = snap, options = list())

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  expect_true(all(out$RULE_ID == expected_id))
})
