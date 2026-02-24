test_that(
  "TMP-004 flags time without date", {
    expected_id <- "TMP-004"
    rule <- rule_tmp_004_time_without_date(
      domain = "ADSL", time_var = "RANDTM",
      date_var = "RANDDT"
    )

    adsl <- data.frame(
      USUBJID = c("S1", "S2"),
      RANDTM = c("08:00", "09:00"),
      RANDDT = c("", "2024-01-02"),
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
