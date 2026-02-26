# ---------------------------------------------------------------------------
# Topology coverage across starter packs
# ---------------------------------------------------------------------------

test_that("all starter pack IDs use axis prefixes", {
  rules <- c(
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
      numeric_max = 1000,
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

  ids <- vapply(rules, function(r) r@id, character(1))
  expect_true(all(grepl("^[A-Z]+-[0-9]{3}$", ids)))
})

test_that("starter packs expose expected topology coverage", {
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
      numeric_max = 1000,
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

  topologies <- unlist(lapply(packs, function(pack) unique(vapply(pack, function(r) r@topology, character(1)))))
  expect_setequal(unique(topologies), paste0("A", 1:7))
})

test_that("starter pack sizes match expected allocations", {
  sizes <- c(
    length(pack_topology_a1(
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
      numeric_max = 1000,
      categorical_var = "state",
      allowed_levels = c("A", "B"),
      duration_var = "duration"
    )),
    length(pack_topology_a2(domain = "PRIMARY", key_vars = c("entity_id", "record_id"), skew_value_var = "value")),
    length(pack_topology_a3()),
    length(pack_topology_a4()),
    length(pack_topology_a5(domain_primary = "PRIMARY", keys = c("entity_id", "record_id"))),
    length(pack_topology_a6(domain_primary = "PRIMARY", keys = c("entity_id", "record_id"))),
    length(pack_topology_a7())
  )

  expect_identical(sizes, c(10L, 8L, 4L, 4L, 5L, 6L, 4L))
})
