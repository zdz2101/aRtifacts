test_that(
  "GRP-004 flags site-level outlier rate",
  {
    expected_id <- "GRP-004"
    rule <- rule_grp_004_site_level_outlier_rate(
      domain = "AE", site_var = "SITEID",
      event_flag_var = "FLAG", k = 1
    )

    ae <- data.frame(
      USUBJID = paste0("S", 1:8),
      AESEQ = c(1, 1, 1, 2, 1, 2, 1, 2),
      SITEID = c(
        "A", "A", "B", "B", "C",
        "C", "D", "D"
      ),
      FLAG = c(0, 0, 1, 0, 1, 0, 1, 1),
      stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        AE = new_domain_snapshot(
          "AE", ae, key_vars = intersect(
          c("USUBJID", "AESEQ"),
          names(ae)
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
