test_that(
  "DE-005 flags impossible logical combination",
  {
    expected_id <- "DE-005"
    rule <- rule_de_005_impossible_logical_combination(domain = "ADSL")

    adsl <- data.frame(
      USUBJID = c("S1", "S2"),
      SEX = c("F", "M"),
      PREGNANT = c("N", "Y"),
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
