test_that(
  "TMP-001 flags invalid date format",
  {
    expected_id <- "TMP-001"
    rule <- rule_tmp_001_invalid_date_format(domain = "ADSL", date_var = "RANDDT")

    adsl <- data.frame(
      USUBJID = c("S1", "S2"),
      RANDDT = c("2024/01/01", "2024-01-02"),
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
