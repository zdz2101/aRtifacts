test_that(
  "SNAP-007 flags key churn spike", {
    expected_id <- "SNAP-007"
    rule <- rule_snap_007_key_churn_spike(
      domain = "ADSL", keys = c("USUBJID"),
      churn_threshold = 0.1
    )

    prev_adsl <- data.frame(
      USUBJID = c("S1", "S2"),
      stringsAsFactors = FALSE
    )
    cur_adsl <- data.frame(
      USUBJID = c("S1", "S3"),
      stringsAsFactors = FALSE
    )

    previous <- new_artifact_snapshot(
      domains = list(
        ADSL = new_domain_snapshot(
          "ADSL", prev_adsl, key_vars = intersect(
          c("USUBJID"),
          names(prev_adsl)
        )
        )
      )
    )
    current <- new_artifact_snapshot(
      domains = list(
        ADSL = new_domain_snapshot(
          "ADSL", cur_adsl, key_vars = intersect(
          c("USUBJID"),
          names(cur_adsl)
        )
        )
      )
    )
    ctx <- new_rule_context(
      current = current, previous = previous,
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
