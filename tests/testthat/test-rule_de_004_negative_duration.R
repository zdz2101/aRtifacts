test_that(
  "DE-004 flags negative duration", {
    expected_id <- "DE-004"
    rule <- rule_de_004_negative_duration(
      domain = "ADSL", duration_var = "ADURN"
    )

    adsl <- data.frame(
      USUBJID = c("S1", "S2"),
      ADURN = c(10, -1),
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
