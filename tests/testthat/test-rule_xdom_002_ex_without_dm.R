test_that(
  "XDOM-002 flags EX without DM", {
    expected_id <- "XDOM-002"
    rule <- rule_xdom_002_ex_without_dm()

    ex <- data.frame(
      USUBJID = c("S1", "S2"),
      EXSEQ = c(1, 2),
      EXTRT = c("A", "B"),
      stringsAsFactors = FALSE
    )
    dm <- data.frame(
      USUBJID = "S1", stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        EX = new_domain_snapshot(
          "EX", ex, key_vars = intersect(
          c("USUBJID", "EXSEQ"),
          names(ex)
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
