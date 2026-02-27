test_that(
  "ID-002 flags duplicate keys", {
    expected_id <- "ID-002"
    rule <- rule_id_002_duplicate_keys(
      domain = "AE", key_vars = c("USUBJID", "VISIT")
    )

    ae <- data.frame(
      USUBJID = c("S1", "S1", "S2"),
      VISIT = c("V1", "V1", "V1"),
      stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        AE = new_domain_snapshot(
          "AE", transform(ae, .ROWID = seq_len(nrow(ae))),
          key_vars = ".ROWID"
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
test_that("rule_id_002_duplicate_keys boundary clean payload returns empty findings", {
  rule <- edge_build_rule_for_boundary("rule_id_002_duplicate_keys")
  ctx <- edge_make_full_context(
    clean = TRUE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rule_id_002_duplicate_keys handles dirty payload without crashing", {
  rule <- edge_build_rule(get("rule_id_002_duplicate_keys", mode = "function"))
  ctx <- edge_make_full_context(
    clean = FALSE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = rule@id)
})
