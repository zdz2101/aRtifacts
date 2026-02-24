test_that(
  "GRP-003 flags repeated identical lab values",
  {
    expected_id <- "GRP-003"
    rule <- rule_grp_003_repeated_identical_lab_values(domain = "LB", min_repeats = 3)

    lb <- data.frame(
      USUBJID = c("S1", "S1", "S1", "S2"),
      LBSEQ = c(1, 2, 3, 1),
      LBTESTCD = c("ALT", "ALT", "ALT", "ALT"),
      LBSTRESN = c(10, 10, 10, 20),
      stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        LB = new_domain_snapshot(
          "LB", lb, key_vars = intersect(
          c("USUBJID", "LBSEQ"),
          names(lb)
        )
        )
      )
    )
    ctx <- new_rule_context(
      current = snap, previous = snap,
      options = list()
    )

    out <- run_rules(ctx, list(rule))
    expect_s3_class(out, "data.frame")
    expect_gt(
      nrow(out),
      0
    )
    expect_true(all(out$RULE_ID == expected_id))
  }
)
