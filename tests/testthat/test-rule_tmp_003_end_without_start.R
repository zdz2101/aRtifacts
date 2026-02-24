test_that(
  "TMP-003 flags end without start", {
    expected_id <- "TMP-003"
    rule <- rule_tmp_003_end_without_start(
      domain = "ADSL", start_var = "TRTSDT",
      end_var = "TRTEDT"
    )

    adsl <- data.frame(
      USUBJID = c("S1", "S2"),
      TRTSDT = c(NA, "2024-01-01"),
      TRTEDT = c("2024-01-05", "2024-01-02"),
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
