test_that(
  "SNAP-004 flags identity drift", {
    expected_id <- "SNAP-004"
    rule <- rule_snap_004_identity_drift(
      domain = "AE", stable_fields = c("USUBJID", "AETERM", "AESTDTC"),
      identity_fields = c("AESEQ")
    )

    prev_ae <- data.frame(
      USUBJID = "S1", AETERM = "HEADACHE",
      AESTDTC = "2024-01-01", AESEQ = 1,
      stringsAsFactors = FALSE
    )
    cur_ae <- data.frame(
      USUBJID = "S1", AETERM = "HEADACHE",
      AESTDTC = "2024-01-01", AESEQ = 99,
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
test_that("rule_snap_004_identity_drift boundary clean payload returns empty findings", {
  rule <- edge_build_rule_for_boundary("rule_snap_004_identity_drift")
  ctx <- edge_make_full_context(
    clean = TRUE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rule_snap_004_identity_drift handles dirty payload without crashing", {
  rule <- edge_build_rule(get("rule_snap_004_identity_drift", mode = "function"))
  ctx <- edge_make_full_context(
    clean = FALSE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = rule@id)
})
