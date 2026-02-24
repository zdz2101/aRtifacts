test_that(
  "XAGG-002 flags visit count mismatch",
  {
    expected_id <- "XAGG-002"
    rule <- rule_xagg_002_visit_count_mismatch(
      left_domain = "VS", right_domain = "LB",
      group_var = "SITEID", visit_var = "VISIT",
      tolerance = 0
    )

    vs <- data.frame(
      USUBJID = c("S1", "S2"),
      VISIT = c("W1", "W2"),
      VSTESTCD = c("SYSBP", "SYSBP"),
      SITEID = c("A", "A"),
      stringsAsFactors = FALSE
    )
    lb <- data.frame(
      USUBJID = "S1", LBSEQ = 1, VISIT = "W1",
      SITEID = "A", stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        VS = new_domain_snapshot(
          "VS", vs, key_vars = intersect(
          c(
            "USUBJID", "VISIT",
            "VSTESTCD"
          ),
          names(vs)
        )
        ),
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
