test_that(
  "XDOM-006 flags visit mismatch across domains",
  {
    expected_id <- "XDOM-006"
    rule <- rule_xdom_006_visit_mismatch_across_domains(
      left_domain = "VS", right_domain = "LB"
    )

    vs <- data.frame(
      USUBJID = c("S1", "S2"),
      VISIT = c("W1", "W1"),
      stringsAsFactors = FALSE
    )
    lb <- data.frame(
      USUBJID = "S1", VISIT = "W2",
      stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        VS = new_domain_snapshot(
          "VS", vs, key_vars = intersect(
          c(
            "USUBJID", "VISIT",
            "VSTESTCD"
          ),
          names(vs)
        )
        ),
        LB = new_domain_snapshot(
          "LB", lb, key_vars = intersect(
          c("USUBJID", "LBSEQ"),
          names(lb)
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
