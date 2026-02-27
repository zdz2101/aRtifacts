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

# Edge harness note: scenario-specific behavior is tested above.
# The two tests below enforce a shared contract:
# - clean boundary payload stays silent
# - dirty payload does not raise runtime errors
test_that("rule_snap_003_record_reappeared boundary clean payload returns empty findings", {
  rule <- edge_build_rule_for_boundary("rule_snap_003_record_reappeared")
  ctx <- edge_make_full_context(
    clean = TRUE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rule_snap_003_record_reappeared handles dirty payload without crashing", {
  rule <- edge_build_rule(get("rule_snap_003_record_reappeared", mode = "function"))
  ctx <- edge_make_full_context(
    clean = FALSE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = rule@id)
})
