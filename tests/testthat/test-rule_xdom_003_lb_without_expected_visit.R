test_that(
  "XDOM-003 flags LB visit missing in VS",
  {
    expected_id <- "XDOM-003"
    rule <- rule_xdom_003_lb_without_expected_visit()

    lb <- data.frame(
      USUBJID = "S1", VISIT = "WEEK2",
      stringsAsFactors = FALSE
    )
    vs <- data.frame(
      USUBJID = "S1", VISIT = "WEEK1",
      stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        LB = new_domain_snapshot(
          "LB", lb, key_vars = intersect(
          c("USUBJID", "LBSEQ"),
          names(lb)
        )
        ),
        VS = new_domain_snapshot(
          "VS", vs, key_vars = intersect(
          c(
            "USUBJID", "VISIT",
            "VSTESTCD"
          ),
          names(vs)
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
