test_that(
  "XAGG-001 flags subject count mismatch",
  {
    expected_id <- "XAGG-001"
    rule <- rule_xagg_001_subject_count_mismatch(
      left_domain = "DM", right_domain = "AE",
      tolerance = 0
    )

    dm <- data.frame(
      USUBJID = c("S1", "S2"),
      stringsAsFactors = FALSE
    )
    ae <- data.frame(
      USUBJID = "S1", stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        DM = new_domain_snapshot(
          "DM", dm, key_vars = intersect(
          c("USUBJID"),
          names(dm)
        )
        ),
        AE = new_domain_snapshot(
          "AE", ae, key_vars = intersect(
          c("USUBJID", "AESEQ"),
          names(ae)
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
