test_that(
  "DE-003 flags invalid categorical code",
  {
    expected_id <- "DE-003"
    rule <- rule_de_003_invalid_categorical_code(
      domain = "DM", value_var = "SEX",
      allowed_levels = c("M", "F")
    )

    dm <- data.frame(
      USUBJID = c("S1", "S2"),
      SEX = c("M", "X"),
      stringsAsFactors = FALSE
    )

    snap <- new_artifact_snapshot(
      domains = list(
        DM = new_domain_snapshot(
          "DM", dm, key_vars = intersect(
          c("USUBJID"),
          names(dm)
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
test_that("rule_de_003_invalid_categorical_code boundary clean payload returns empty findings", {
  rule <- edge_build_rule_for_boundary("rule_de_003_invalid_categorical_code")
  ctx <- edge_make_full_context(
    clean = TRUE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  out <- run_rules(ctx, list(rule))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 0L)
})

test_that("rule_de_003_invalid_categorical_code handles dirty payload without crashing", {
  rule <- edge_build_rule(get("rule_de_003_invalid_categorical_code", mode = "function"))
  ctx <- edge_make_full_context(
    clean = FALSE,
    with_previous = TRUE,
    options = list(removed_keys_history = character())
  )

  expect_no_error(out <- run_rules(ctx, list(rule)))
  edge_expect_rule_output(out, expected_id = rule@id)
})
