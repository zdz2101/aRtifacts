test_that(
  "BOTH-001 flags new AE without new EX",
  {
    expected_id <- "BOTH-001"
    rule <- rule_both_001_new_ae_without_new_ex()

    prev_ae <- data.frame(
      USUBJID = "S1", AESEQ = 1, stringsAsFactors = FALSE
    )
    cur_ae <- data.frame(
      USUBJID = c("S1", "S2"),
      AESEQ = c(1, 1),
      stringsAsFactors = FALSE
    )
    prev_ex <- data.frame(
      USUBJID = "S1", EXSEQ = 1, stringsAsFactors = FALSE
    )
    cur_ex <- data.frame(
      USUBJID = "S1", EXSEQ = 1, stringsAsFactors = FALSE
    )

    previous <- new_artifact_snapshot(
      domains = list(
        AE = new_domain_snapshot(
          "AE", prev_ae,
          key_vars = intersect(
            c("USUBJID", "AESEQ"),
            names(prev_ae)
          )
        ),
        EX = new_domain_snapshot(
          "EX", prev_ex,
          key_vars = intersect(
            c("USUBJID", "EXSEQ"),
            names(prev_ex)
          )
        )
      )
    )
    current <- new_artifact_snapshot(
      domains = list(
        AE = new_domain_snapshot(
          "AE", cur_ae,
          key_vars = intersect(
            c("USUBJID", "AESEQ"),
            names(cur_ae)
          )
        ),
        EX = new_domain_snapshot(
          "EX", cur_ex,
          key_vars = intersect(
            c("USUBJID", "EXSEQ"),
            names(cur_ex)
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
