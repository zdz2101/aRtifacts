test_that(
  "ID-001 flags missing key fields", {
    expected_id <- "ID-001"
    rule <- rule_id_001_missing_key_fields(
      domain = "ADSL", key_vars = c("USUBJID", "SITEID")
    )

    adsl <- data.frame(
      USUBJID = c("S1", ""),
      SITEID = c("A", "A"),
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
