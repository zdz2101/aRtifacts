test_that("choose_topology maps decision shape to A0-A7", {
  expect_identical(choose_topology(FALSE, 1, "schema"), "A0")
  expect_identical(choose_topology(FALSE, 1, "record"), "A1")
  expect_identical(choose_topology(FALSE, 1, "aggregate"), "A2")
  expect_identical(choose_topology(FALSE, 2, "relational"), "A3")
  expect_identical(choose_topology(FALSE, 2, "aggregate"), "A4")
  expect_identical(choose_topology(TRUE, 1, "record"), "A5")
  expect_identical(choose_topology(TRUE, 1, "aggregate"), "A6")
  expect_identical(choose_topology(TRUE, 2, "record"), "A7")
})

test_that("choose_topology accepts level aliases", {
  expect_identical(choose_topology(FALSE, 1, "row"), "A1")
  expect_identical(choose_topology(FALSE, 1, "group"), "A2")
  expect_identical(choose_topology(FALSE, 2, "relationship"), "A3")
})

test_that("choose_topology rejects invalid combinations", {
  expect_error(
    choose_topology(FALSE, 2, "record"),
    "single-cut cross-domain rules"
  )
  expect_error(
    choose_topology(TRUE, 1, "schema"),
    "longitudinal single-domain rules"
  )
  expect_error(
    choose_topology(TRUE, 2, "schema"),
    "Longitudinal cross-domain rules map to A7"
  )
})

test_that("new_rule gives topology-specific missing domain hints", {
  expect_error(
    new_rule(
      id = "TMP-1",
      title = "missing domain",
      topology = "A1",
      scope = "record",
      intent = "quality_screening",
      fn = function(data, context, rule) data.frame()
    ),
    "`domain` is required for topology A1"
  )

  expect_error(
    new_rule(
      id = "TMP-2",
      title = "missing domains",
      topology = "A3",
      scope = "record",
      intent = "core_integrity",
      domains = "LEFT",
      fn = function(domains, context, rule) data.frame()
    ),
    "`domains` must contain at least two domain names for topology A3"
  )
})

test_that("choose_topology validates malformed scalar inputs", {
  expect_error(choose_topology(is_longitudinal = NA, n_domains = 1, level = "record"), "is_longitudinal")
  expect_error(choose_topology(is_longitudinal = "TRUE", n_domains = 1, level = "record"), "is_longitudinal")

  expect_error(choose_topology(is_longitudinal = FALSE, n_domains = 0, level = "record"), "n_domains")
  expect_error(choose_topology(is_longitudinal = FALSE, n_domains = 1.5, level = "record"), "n_domains")
  expect_error(choose_topology(is_longitudinal = FALSE, n_domains = NA, level = "record"), "n_domains")

  expect_error(choose_topology(is_longitudinal = FALSE, n_domains = 1, level = NA), "level")
  expect_error(choose_topology(is_longitudinal = FALSE, n_domains = 1, level = ""), "level")
  expect_error(choose_topology(is_longitudinal = FALSE, n_domains = 1, level = 123), "level")
})
