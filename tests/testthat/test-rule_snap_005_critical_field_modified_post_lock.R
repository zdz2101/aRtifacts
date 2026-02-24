test_that(
  "SNAP-005 flags critical field changes post-lock",
  {
    expected_id <- "SNAP-005"
    rule <- rule_snap_005_critical_field_modified_post_lock(
      domain = "ADSL", keys = c("USUBJID"),
      critical_fields = c("TRT01A")
    )

    prev_adsl <- data.frame(
      USUBJID = "S1", TRT01A = "A",
      stringsAsFactors = FALSE
    )
    cur_adsl <- data.frame(
      USUBJID = "S1", TRT01A = "B",
      stringsAsFactors = FALSE
    )

    previous <- new_artifact_snapshot(
      domains = list(
        ADSL = new_domain_snapshot(
          "ADSL", prev_adsl, key_vars = intersect(
          c("USUBJID"),
          names(prev_adsl)
        )
        )
      )
    )
    current <- new_artifact_snapshot(
      domains = list(
        ADSL = new_domain_snapshot(
          "ADSL", cur_adsl, key_vars = intersect(
          c("USUBJID"),
          names(cur_adsl)
        )
        )
      )
    )
    ctx <- new_rule_context(
      current = current, previous = previous,
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
