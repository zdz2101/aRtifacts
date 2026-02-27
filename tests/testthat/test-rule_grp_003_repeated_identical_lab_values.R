test_that(
  "GRP-003 flags repeated identical lab values",
  {
    expected_id <- "GRP-003"
    rule <- rule_grp_003_repeated_identical_lab_values(
      domain = "LB",
      subject_var = "USUBJID",
      min_repeats = 3
    )

    lb <- data.frame(
      USUBJID = c("S1", "S1", "S1", "S2"),
      LBSEQ = c(1, 2, 3, 1),
      LBTESTCD = c("ALT", "ALT", "ALT", "ALT"),
      LBSTRESN = c(10, 10, 10, 20),
      stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        LB = new_domain_snapshot(
          "LB", lb, key_vars = intersect(
          c("USUBJID", "LBSEQ"),
          names(lb)
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
test_that("rule_grp_003_repeated_identical_lab_values boundary clean payload returns empty findings", {
  rule <- edge_build_rule_for_boundary("rule_grp_003_repeated_identical_lab_values")
  ctx <- edge_make_full_context(
    clean = TRUE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rule_grp_003_repeated_identical_lab_values handles dirty payload without crashing", {
  rule <- edge_build_rule(get("rule_grp_003_repeated_identical_lab_values", mode = "function"))
  ctx <- edge_make_full_context(
    clean = FALSE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = rule@id)
})
