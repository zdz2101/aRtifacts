test_that(
  "SNAP-006 flags row count spike/drop",
  {
    expected_id <- "SNAP-006"
    rule <- rule_snap_006_row_count_spike_drop(
      domain = "ADSL", threshold_abs = 1,
      threshold_rel = 0.5
    )

    prev_adsl <- data.frame(
      USUBJID = "S1", stringsAsFactors = FALSE
    )
    cur_adsl <- data.frame(
      USUBJID = c("S1", "S2", "S3"),
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

# Edge harness note: scenario-specific behavior is tested above.
# The two tests below enforce a shared contract:
# - clean boundary payload stays silent
# - dirty payload does not raise runtime errors
test_that("rule_snap_006_row_count_spike_drop boundary clean payload returns empty findings", {
  rule <- edge_build_rule_for_boundary("rule_snap_006_row_count_spike_drop")
  ctx <- edge_make_full_context(
    clean = TRUE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rule_snap_006_row_count_spike_drop handles dirty payload without crashing", {
  rule <- edge_build_rule(get("rule_snap_006_row_count_spike_drop", mode = "function"))
  ctx <- edge_make_full_context(
    clean = FALSE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = rule@id)
})
