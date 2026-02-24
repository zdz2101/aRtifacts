test_that(
  "XAGG-004 flags exposure-event rate discrepancy",
  {
    expected_id <- "XAGG-004"
    rule <- rule_xagg_004_exposure_event_rate_discrepancy(
      ae_domain = "AE", ex_domain = "EX",
      group_var = "SITEID", ae_high_threshold = 1.5,
      ex_low_threshold = 2
    )

    ae <- data.frame(
      SITEID = c("A", "A", "A"),
      USUBJID = c("S1", "S1", "S1"),
      AESEQ = c(1, 2, 3),
      stringsAsFactors = FALSE
    )
    ex <- data.frame(
      SITEID = "A", USUBJID = "S1",
      EXSEQ = 1, stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        AE = new_domain_snapshot(
          "AE", ae, key_vars = intersect(
          c("USUBJID", "AESEQ"),
          names(ae)
        )
        ),
        EX = new_domain_snapshot(
          "EX", ex, key_vars = intersect(
          c("USUBJID", "EXSEQ"),
          names(ex)
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
