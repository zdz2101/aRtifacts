test_that(
  "TMP-002 flags start after end", {
    expected_id <- "TMP-002"
    rule <- rule_tmp_002_start_after_end(
      domain = "ADSL", start_var = "TRTSDT",
      end_var = "TRTEDT"
    )

    adsl <- data.frame(
      USUBJID = c("S1", "S2"),
      TRTSDT = c("2024-02-01", "2024-01-01"),
      TRTEDT = c("2024-01-01", "2024-02-01"),
      stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        ADSL = new_domain_snapshot(
          "ADSL", adsl, key_vars = intersect(
          c("USUBJID"),
          names(adsl)
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
