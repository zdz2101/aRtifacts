test_that(
  "GRP-002 flags excessive subject event frequency",
  {
    expected_id <- "GRP-002"
    rule <- rule_grp_002_subject_excessive_event_frequency(
      domain = "AE",
      subject_var = "USUBJID",
      max_events = 2
    )

    ae <- data.frame(
      USUBJID = c("S1", "S1", "S1", "S2"),
      AESEQ = c(1, 2, 3, 1),
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

# Edge harness note: scenario-specific behavior is tested above.
# The two tests below enforce a shared contract:
# - clean boundary payload stays silent
# - dirty payload does not raise runtime errors
test_that("rule_grp_002_subject_excessive_event_frequency boundary clean payload returns empty findings", {
  rule <- edge_build_rule_for_boundary("rule_grp_002_subject_excessive_event_frequency")
  ctx <- edge_make_full_context(
    clean = TRUE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rule_grp_002_subject_excessive_event_frequency handles dirty payload without crashing", {
  rule <- edge_build_rule(get("rule_grp_002_subject_excessive_event_frequency", mode = "function"))
  ctx <- edge_make_full_context(
    clean = FALSE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = rule@id)
})
