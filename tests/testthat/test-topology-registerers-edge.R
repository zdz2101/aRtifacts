register_all_topologies_edge <- function(overwrite = FALSE) {
  ids <- c(
    register_topology_a0(domain = "PRIMARY", overwrite = overwrite),
    register_topology_a1(domain = "PRIMARY", key_vars = c("entity_id", "record_id"), overwrite = overwrite),
    register_topology_a2(domain = "PRIMARY", key_vars = c("entity_id", "record_id"), overwrite = overwrite),
    register_topology_a3(overwrite = overwrite),
    register_topology_a4(overwrite = overwrite),
    register_topology_a5(domain_primary = "PRIMARY", keys = c("entity_id", "record_id"), overwrite = overwrite),
    register_topology_a6(domain_primary = "PRIMARY", keys = c("entity_id", "record_id"), overwrite = overwrite),
    register_topology_a7(overwrite = overwrite)
  )
  unname(ids)
}

test_that("register_topology_a0..a7 smoke-registers rules", {
  clear_rule_registry()
  on.exit(clear_rule_registry(), add = TRUE)

  ids <- register_all_topologies_edge(overwrite = FALSE)
  listed <- list_rules()

  expect_true(length(ids) > 0L)
  expect_setequal(ids, listed)

  packed <- pack_topology_all()
  expect_equal(length(packed), length(listed))
  expect_true(all(vapply(packed, function(x) S7::S7_inherits(x, CoreRule), logical(1))))
})

test_that("register_topology_* respects overwrite semantics", {
  clear_rule_registry()
  on.exit(clear_rule_registry(), add = TRUE)

  register_all_topologies_edge(overwrite = FALSE)
  expect_error(register_all_topologies_edge(overwrite = FALSE), "already registered")
  expect_no_error(register_all_topologies_edge(overwrite = TRUE))
})
