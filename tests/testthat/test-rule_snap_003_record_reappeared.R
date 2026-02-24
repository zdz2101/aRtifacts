test_that(
  "SNAP-003 flags record reappearance",
  {
    expected_id <- "SNAP-003"
    rule <- rule_snap_003_record_reappeared(domain = "ADSL", keys = c("USUBJID"))

    cur_adsl <- data.frame(
      USUBJID = c("S9", "S1"),
      stringsAsFactors = FALSE
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
    previous <- new_artifact_snapshot(
      domains = list(
        ADSL = new_domain_snapshot(
          "ADSL", data.frame(
          USUBJID = "S1", stringsAsFactors = FALSE
        ),
          key_vars = "USUBJID"
        )
      )
    )

    ctx <- new_rule_context(
      current = current, previous = previous,
      options = list(
        removed_keys_history = data.frame(
          USUBJID = "S9", stringsAsFactors = FALSE
        )
      )
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
