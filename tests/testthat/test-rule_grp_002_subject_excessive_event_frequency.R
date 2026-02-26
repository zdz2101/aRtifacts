test_that(
  "GRP-002 flags excessive subject event frequency",
  {
    expected_id <- "GRP-002"
    rule <- rule_grp_002_subject_excessive_event_frequency(
      domain = "AE",
      subject_var = "USUBJID",
      max_events = 2
    )

    ae <- data.frame(
      USUBJID = c("S1", "S1", "S1", "S2"),
      AESEQ = c(1, 2, 3, 1),
      stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
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
