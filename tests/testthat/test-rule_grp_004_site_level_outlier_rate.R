test_that(
  "GRP-004 flags site-level outlier rate",
  {
    expected_id <- "GRP-004"
    rule <- rule_grp_004_site_level_outlier_rate(
      domain = "AE", site_var = "SITEID",
      event_flag_var = "FLAG", k = 1
    )

    ae <- data.frame(
      USUBJID = paste0("S", 1:8),
      AESEQ = c(1, 1, 1, 2, 1, 2, 1, 2),
      SITEID = c(
        "A", "A", "B", "B", "C",
        "C", "D", "D"
      ),
      FLAG = c(0, 0, 1, 0, 1, 0, 1, 1),
      stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        AE = new_domain_snapshot(
          "AE", ae, key_vars = intersect(
          c("USUBJID", "AESEQ"),
          names(ae)
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
test_that("rule_grp_004_site_level_outlier_rate boundary clean payload returns empty findings", {
  rule <- edge_build_rule_for_boundary("rule_grp_004_site_level_outlier_rate")
  ctx <- edge_make_full_context(
    clean = TRUE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rule_grp_004_site_level_outlier_rate handles dirty payload without crashing", {
  rule <- edge_build_rule(get("rule_grp_004_site_level_outlier_rate", mode = "function"))
  ctx <- edge_make_full_context(
    clean = FALSE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = rule@id)
})
