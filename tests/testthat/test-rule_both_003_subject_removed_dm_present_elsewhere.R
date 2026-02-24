test_that(
  "BOTH-003 flags subject removed from DM but present elsewhere",
  {
    expected_id <- "BOTH-003"
    rule <- rule_both_003_subject_removed_dm_present_elsewhere(dm_domain = "DM", other_domain = "AE")

    prev_dm <- data.frame(
      USUBJID = c("S1", "S2"),
      stringsAsFactors = FALSE
    )
    cur_dm <- data.frame(
      USUBJID = "S1", stringsAsFactors = FALSE
    )
    cur_ae <- data.frame(
      USUBJID = "S2", AESEQ = 1, stringsAsFactors = FALSE
    )

    previous <- new_artifact_snapshot(
      domains = list(
        DM = new_domain_snapshot(
          "DM", prev_dm,
          key_vars = intersect(
            c("USUBJID"),
            names(prev_dm)
          )
        ),
        AE = new_domain_snapshot(
          "AE", data.frame(
            USUBJID = "S1", AESEQ = 1,
            stringsAsFactors = FALSE
          ),
          key_vars = c("USUBJID", "AESEQ")
        )
      )
    )
    current <- new_artifact_snapshot(
      domains = list(
        DM = new_domain_snapshot(
          "DM", cur_dm,
          key_vars = intersect(
            c("USUBJID"),
            names(cur_dm)
          )
        ),
        AE = new_domain_snapshot(
          "AE", cur_ae,
          key_vars = intersect(
            c("USUBJID", "AESEQ"),
            names(cur_ae)
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
