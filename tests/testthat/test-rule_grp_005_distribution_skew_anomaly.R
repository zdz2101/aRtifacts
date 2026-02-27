test_that(
  "GRP-005 flags skew anomaly", {
    expected_id <- "GRP-005"
    rule <- rule_grp_005_distribution_skew_anomaly(
      domain = "LB", value_var = "LBSTRESN",
      threshold = 0.5
    )

    lb <- data.frame(
      USUBJID = c("S1", "S2", "S3", "S4"),
      LBSEQ = c(1, 1, 1, 1),
      LBSTRESN = c(1, 1, 1, 100),
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
test_that("rule_grp_005_distribution_skew_anomaly boundary clean payload returns empty findings", {
  rule <- edge_build_rule_for_boundary("rule_grp_005_distribution_skew_anomaly")
  ctx <- edge_make_full_context(
    clean = TRUE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rule_grp_005_distribution_skew_anomaly handles dirty payload without crashing", {
  rule <- edge_build_rule(get("rule_grp_005_distribution_skew_anomaly", mode = "function"))
  ctx <- edge_make_full_context(
    clean = FALSE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = rule@id)
})
