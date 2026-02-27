test_that(
  "SNAP-002 flags unexpected record changes",
  {
    expected_id <- "SNAP-002"
    rule <- rule_snap_002_record_changed_unexpectedly(
      domain = "ADSL", keys = c("USUBJID"),
      monitored_fields = c("AGE")
    )

    prev_adsl <- data.frame(
      USUBJID = "S1", AGE = 30, stringsAsFactors = FALSE
    )
    cur_adsl <- data.frame(
      USUBJID = "S1", AGE = 40, stringsAsFactors = FALSE
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
test_that("rule_snap_002_record_changed_unexpectedly boundary clean payload returns empty findings", {
  rule <- edge_build_rule_for_boundary("rule_snap_002_record_changed_unexpectedly")
  ctx <- edge_make_full_context(
    clean = TRUE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rule_snap_002_record_changed_unexpectedly handles dirty payload without crashing", {
  rule <- edge_build_rule(get("rule_snap_002_record_changed_unexpectedly", mode = "function"))
  ctx <- edge_make_full_context(
    clean = FALSE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = rule@id)
})
