test_that(
  "GRP-001 flags excessive missingness",
  {
    expected_id <- "GRP-001"
    rule <- rule_grp_001_excessive_missingness_rate(
      domain = "ADSL", target_vars = c("AGE"),
      group_var = "SITEID", threshold = 0.4
    )

    adsl <- data.frame(
      USUBJID = c("S1", "S2", "S3"),
      SITEID = c("A", "A", "B"),
      AGE = c(NA, 20, 30),
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
test_that("rule_grp_001_excessive_missingness_rate boundary clean payload returns empty findings", {
  rule <- edge_build_rule_for_boundary("rule_grp_001_excessive_missingness_rate")
  ctx <- edge_make_full_context(
    clean = TRUE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rule_grp_001_excessive_missingness_rate handles dirty payload without crashing", {
  rule <- edge_build_rule(get("rule_grp_001_excessive_missingness_rate", mode = "function"))
  ctx <- edge_make_full_context(
    clean = FALSE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = rule@id)
})
