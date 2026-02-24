# ---------------------------------------------------------------------------
# rule_catalog_core() dataset-backed catalog
# ---------------------------------------------------------------------------

test_that("rule_catalog_core() returns expected structure and row count", {
  cat <- rule_catalog_core()
  expect_s3_class(cat, "data.frame")
  expect_named(cat, c("rule_id", "rule_name", "scope", "category"))
  expect_equal(nrow(cat), 50L)
})

test_that("rule_catalog_core() has ordered IDs R001..R050", {
  ids <- rule_catalog_core()$rule_id
  expect_identical(ids, sprintf("R%03d", seq_len(50)))
})

test_that("rule_catalog_core() has no missing values and unique IDs", {
  cat <- rule_catalog_core()
  expect_false(anyNA(cat))
  expect_equal(length(unique(cat$rule_id)), 50L)
})

test_that("rule_catalog_core() uses supported scope values", {
  scopes <- unique(rule_catalog_core()$scope)
  expect_setequal(scopes, c("Record", "Group", "Snapshot"))
})

test_that("rule_catalog_core() matches data-raw CSV source exactly", {
  skip_if_not(!is.null(system.file("data-raw/rule_catalog_core.csv", package = "aRtifacts")))
  csv <- utils::read.csv(system.file("data-raw/rule_catalog_core.csv", package = "aRtifacts"), stringsAsFactors = FALSE, check.names = FALSE)
  cat <- rule_catalog_core()
  expect_identical(cat, csv)
})

test_that("rule_catalog_core() exposes the packaged data object", {
  expect_true(exists("rule_catalog_core_data", inherits = TRUE))
  expect_identical(rule_catalog_core(), rule_catalog_core_data)
})
