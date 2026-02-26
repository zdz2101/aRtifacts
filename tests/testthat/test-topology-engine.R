make_artifact <- function(primary_df, reference_df, events_df = NULL, support_df = NULL, aux_df = NULL) {
  domains <- list(
    PRIMARY = new_domain_snapshot("PRIMARY", primary_df, key_vars = c("entity_id", "record_id")),
    REFERENCE = new_domain_snapshot("REFERENCE", reference_df, key_vars = c("entity_id", "record_id"))
  )

  if (!is.null(events_df)) domains$EVENTS <- new_domain_snapshot("EVENTS", events_df, key_vars = c("entity_id", "event_id"))
  if (!is.null(support_df)) domains$SUPPORT <- new_domain_snapshot("SUPPORT", support_df, key_vars = c("entity_id", "support_id"))
  if (!is.null(aux_df)) domains$AUX <- new_domain_snapshot("AUX", aux_df, key_vars = c("entity_id", "aux_id"))

  new_artifact_snapshot(domains = domains)
}

test_that("topology packs return CoreRule objects", {
  packs <- list(
    pack_topology_a1(
      domain = "PRIMARY",
      key_vars = c("entity_id", "record_id"),
      date_var = "event_date",
      start_var = "start_date",
      end_var = "end_date",
      time_var = "event_time",
      condition_var = "status",
      required_var = "required_field",
      numeric_var = "value",
      numeric_min = 0,
      numeric_max = 120,
      categorical_var = "state",
      allowed_levels = c("A", "B"),
      duration_var = "duration"
    ),
    pack_topology_a2(domain = "PRIMARY", key_vars = c("entity_id", "record_id"), skew_value_var = "value"),
    pack_topology_a3(),
    pack_topology_a4(),
    pack_topology_a5(domain_primary = "PRIMARY", keys = c("entity_id", "record_id")),
    pack_topology_a6(domain_primary = "PRIMARY", keys = c("entity_id", "record_id")),
    pack_topology_a7()
  )

  for (pack in packs) {
    expect_true(all(vapply(pack, function(x) S7::S7_inherits(x, CoreRule), logical(1))))
  }
})

test_that("run_rules() executes A1 rule against current snapshot", {
  primary <- data.frame(
    entity_id = c("E-001", "", "E-003"),
    record_id = c(1, 2, 3),
    value = c(34, 41, 55),
    stringsAsFactors = FALSE
  )
  reference <- data.frame(
    entity_id = c("E-001", "E-002", "E-003"),
    record_id = c(1, 2, 3),
    group_id = c("G1", "G1", "G2"),
    stringsAsFactors = FALSE
  )

  art <- make_artifact(primary_df = primary, reference_df = reference)
  ctx <- new_rule_context(current = art)
  rule <- rule_id_001_missing_key_fields(domain = "PRIMARY", key_vars = c("entity_id", "record_id"))

  out <- run_rules(ctx, list(rule))
  expect_true(nrow(out) >= 1L)
  expect_true(all(c("RULE_ID", "TOPOLOGY", "SCOPE", "INTENT", "SEVERITY", "OWNER_TEAM") %in% names(out)))
  expect_true(all(out$RULE_ID == "ID-001"))
  expect_true(all(out$TOPOLOGY == "A1"))
})

test_that("run_rules() executes A5 cross-snapshot rule", {
  primary_prev <- data.frame(
    entity_id = c("E-001", "E-002", "E-003"),
    record_id = c(1, 2, 3),
    value = c(34, 41, 55),
    stringsAsFactors = FALSE
  )
  primary_cur <- data.frame(
    entity_id = c("E-001", "E-003"),
    record_id = c(1, 3),
    value = c(34, 55),
    stringsAsFactors = FALSE
  )

  reference <- data.frame(
    entity_id = c("E-001", "E-002", "E-003"),
    record_id = c(1, 2, 3),
    group_id = c("G1", "G1", "G2"),
    stringsAsFactors = FALSE
  )

  prev_art <- make_artifact(primary_df = primary_prev, reference_df = reference)
  cur_art <- make_artifact(primary_df = primary_cur, reference_df = reference[reference$entity_id != "E-002", , drop = FALSE])
  ctx <- new_rule_context(current = cur_art, previous = prev_art)

  rule <- rule_snap_001_unexpected_key_deletions(domain = "PRIMARY", keys = c("entity_id", "record_id"))
  out <- run_rules(ctx, list(rule))

  expect_true(nrow(out) >= 1L)
  expect_true(any(out$entity_id == "E-002"))
  expect_true(all(out$RULE_ID == "SNAP-001"))
  expect_true(all(out$TOPOLOGY == "A5"))
})
