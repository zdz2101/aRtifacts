test_that(
  "XDOM-001 flags AE without DM", {
    expected_id <- "XDOM-001"
    rule <- rule_xdom_001_ae_without_dm()

    ae <- data.frame(
      USUBJID = c("S1", "S2"),
      AESEQ = c(1, 2),
      AETERM = c("H", "N"),
      stringsAsFactors = FALSE
    )
    dm <- data.frame(
      USUBJID = "S1", stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        AE = new_domain_snapshot(
          "AE", ae, key_vars = intersect(
          c("USUBJID", "AESEQ"),
          names(ae)
        )
        ),
        DM = new_domain_snapshot(
          "DM", dm, key_vars = intersect(
          c("USUBJID"),
          names(dm)
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
