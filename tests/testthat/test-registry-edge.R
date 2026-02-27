test_that("registry APIs support register/get/list/clear lifecycle", {
  clear_rule_registry()
  on.exit(clear_rule_registry(), add = TRUE)

  rule <- rule_id_001_missing_key_fields(domain = "PRIMARY", key_vars = c("entity_id", "record_id"))
  reg_id <- register_rule(rule)

  expect_identical(reg_id, "ID-001")
  expect_true("ID-001" %in% list_rules())
  expect_identical(get_rule("ID-001")@id, "ID-001")

  expect_error(register_rule(rule), "already registered")

  overwritten <- rule_id_001_missing_key_fields(domain = "PRIMARY", key_vars = c("entity_id", "record_id"), owner_team = "X")
  expect_no_error(register_rule(overwritten, overwrite = TRUE))
  expect_identical(get_rule("ID-001")@owner_team, "X")

  expect_true(isTRUE(clear_rule_registry()))
  expect_equal(length(list_rules()), 0L)
})

test_that("pack_topology_all returns selected IDs and errors on unknown IDs", {
  clear_rule_registry()
  on.exit(clear_rule_registry(), add = TRUE)

  register_rule(rule_id_001_missing_key_fields(domain = "PRIMARY", key_vars = c("entity_id", "record_id")))
  register_rule(rule_id_002_duplicate_keys(domain = "PRIMARY", key_vars = c("entity_id", "record_id")))

  all_pack <- pack_topology_all()
  expect_equal(length(all_pack), 2L)

  subset_pack <- pack_topology_all(ids = "ID-001")
  expect_equal(length(subset_pack), 1L)
  expect_identical(subset_pack[[1]]@id, "ID-001")

  expect_error(pack_topology_all(ids = "NOT-REAL"), "Unknown rule IDs")
})

test_that("get_rule errors for unknown IDs", {
  clear_rule_registry()
  on.exit(clear_rule_registry(), add = TRUE)

  expect_error(get_rule("UNKNOWN-ID"), "is not registered")
})
