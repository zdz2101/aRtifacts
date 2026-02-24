test_that(
  "SNAP-006 flags row count spike/drop",
  {
    expected_id <- "SNAP-006"
    rule <- rule_snap_006_row_count_spike_drop(
      domain = "ADSL", threshold_abs = 1,
      threshold_rel = 0.5
    )

    prev_adsl <- data.frame(
      USUBJID = "S1", stringsAsFactors = FALSE
    )
    cur_adsl <- data.frame(
      USUBJID = c("S1", "S2", "S3"),
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
