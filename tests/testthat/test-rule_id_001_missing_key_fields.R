test_that(
  "ID-001 flags missing key fields", {
    expected_id <- "ID-001"
    rule <- rule_id_001_missing_key_fields(
      domain = "ADSL", key_vars = c("USUBJID", "SITEID")
    )

    adsl <- data.frame(
      USUBJID = c("S1", ""),
      SITEID = c("A", "A"),
      stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        ADSL = new_domain_snapshot(
          "ADSL", adsl, key_vars = intersect(
          c("USUBJID"),
          names(adsl)
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
test_that("rule_id_001_missing_key_fields boundary clean payload returns empty findings", {
  rule <- edge_build_rule_for_boundary("rule_id_001_missing_key_fields")
  ctx <- edge_make_full_context(
    clean = TRUE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rule_id_001_missing_key_fields handles dirty payload without crashing", {
  rule <- edge_build_rule(get("rule_id_001_missing_key_fields", mode = "function"))
  ctx <- edge_make_full_context(
    clean = FALSE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = rule@id)
})
