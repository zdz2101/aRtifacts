test_that(
  "SNAP-011 flags event rate change anomaly",
  {
    expected_id <- "SNAP-011"
    rule <- rule_snap_011_event_rate_change_anomaly(
      domain = "AE", group_var = "SITEID",
      subject_var = "USUBJID", threshold_abs_change = 0.5
    )

    prev_ae <- data.frame(
      SITEID = c("A", "A"),
      USUBJID = c("S1", "S2"),
      AESEQ = c(1, 1),
      stringsAsFactors = FALSE
    )
    cur_ae <- data.frame(
      SITEID = c("A", "A", "A", "A"),
      USUBJID = c("S1", "S1", "S2", "S2"),
      AESEQ = c(1, 2, 1, 2),
      stringsAsFactors = FALSE
    )

    previous <- new_artifact_snapshot(
      domains = list(
        AE = new_domain_snapshot(
          "AE", prev_ae, key_vars = intersect(
          c("USUBJID", "AESEQ"),
          names(prev_ae)
        )
        )
      )
    )
    current <- new_artifact_snapshot(
      domains = list(
        AE = new_domain_snapshot(
          "AE", cur_ae, key_vars = intersect(
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

# Edge harness note: scenario-specific behavior is tested above.
# The two tests below enforce a shared contract:
# - clean boundary payload stays silent
# - dirty payload does not raise runtime errors
test_that("rule_snap_011_event_rate_change_anomaly boundary clean payload returns empty findings", {
  rule <- edge_build_rule_for_boundary("rule_snap_011_event_rate_change_anomaly")
  ctx <- edge_make_full_context(
    clean = TRUE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rule_snap_011_event_rate_change_anomaly handles dirty payload without crashing", {
  rule <- edge_build_rule(get("rule_snap_011_event_rate_change_anomaly", mode = "function"))
  ctx <- edge_make_full_context(
    clean = FALSE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = rule@id)
})
