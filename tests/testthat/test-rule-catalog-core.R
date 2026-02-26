# ---------------------------------------------------------------------------
# rule_catalog_core() topology starter catalog
# ---------------------------------------------------------------------------

test_that("rule_catalog_core() returns expected columns", {
  cat <- rule_catalog_core()
  expect_s3_class(cat, "data.frame")
  expect_named(cat, c("rule_id", "rule_name", "topology", "scope", "intent", "category"))
})

test_that("rule_catalog_core() has unique rule IDs and no missing values", {
  cat <- rule_catalog_core()
  expect_false(anyNA(cat))
  expect_equal(length(unique(cat$rule_id)), nrow(cat))
})

test_that("rule_catalog_core() covers all topologies A0-A7", {
  cat <- rule_catalog_core()
  expect_setequal(unique(cat$topology), paste0("A", 0:7))
})

test_that("rule_catalog_core() reflects bundled starter rules", {
  rules <- c(
    pack_topology_a0(domain = "PRIMARY"),
    pack_topology_a1(domain = "PRIMARY", key_vars = c("entity_id", "record_id")),
    pack_topology_a2(domain = "PRIMARY", key_vars = c("entity_id", "record_id")),
    pack_topology_a3(),
    pack_topology_a4(),
    pack_topology_a5(domain_primary = "PRIMARY", keys = c("entity_id", "record_id")),
    pack_topology_a6(domain_primary = "PRIMARY", keys = c("entity_id", "record_id")),
    pack_topology_a7()
  )

  expected_ids <- sort(vapply(rules, function(x) x@id, character(1)))
  observed_ids <- sort(rule_catalog_core()$rule_id)
  expect_identical(observed_ids, expected_ids)
})
