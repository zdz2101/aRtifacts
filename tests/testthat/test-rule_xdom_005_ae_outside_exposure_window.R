test_that(
  "XDOM-005 flags AE outside exposure window",
  {
    expected_id <- "XDOM-005"
    rule <- rule_xdom_005_ae_outside_exposure_window(window_days = 0)

    ae <- data.frame(
      USUBJID = "S1", AESEQ = 1, AESTDTC = "2024-02-01",
      stringsAsFactors = FALSE
    )
    ex <- data.frame(
      USUBJID = "S1", EXSTDTC = "2024-01-01",
      EXENDTC = "2024-01-05", stringsAsFactors = FALSE
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
