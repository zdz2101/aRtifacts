# ---------------------------------------------------------------------------
# Test template for custom rule packs
# ---------------------------------------------------------------------------
# Copy to tests/testthat/test-<your-pack>.R and replace function/IDs.

rule_by_id <- function(rules, id) {
  Filter(function(rule) identical(rule@id, id), rules)[[1]]
}

test_that("template pack returns FlagRule objects", {
  rules <- make_rule_pack_template(domain = "X")

  expect_type(rules, "list")
  expect_true(length(rules) >= 1L)
  expect_true(all(vapply(rules, function(rule) S7::S7_inherits(rule, FlagRule), logical(1))))
})

test_that("TMP001_VALUE_OVER_THRESHOLD flags rows over threshold", {
  rules <- make_rule_pack_template(domain = "X", threshold = 10)

  df <- data.frame(
    USUBJID = c("S1", "S2", "S3"),
    VALUE = c(5, 11, 8),
    stringsAsFactors = FALSE
  )

  out <- rule_by_id(rules, "TMP001_VALUE_OVER_THRESHOLD")@fn(df)
  expect_equal(nrow(out), 1L)
  expect_identical(out$USUBJID, "S2")
})

test_that("TMP002_FLAG_WITHOUT_SUPPORTING_DATE flags inconsistent rows", {
  rules <- make_rule_pack_template(domain = "X")

  df <- data.frame(
    USUBJID = c("S1", "S2"),
    FLAG = c("Y", "N"),
    DATE = c("", "2024-01-01"),
    stringsAsFactors = FALSE
  )

  out <- rule_by_id(rules, "TMP002_FLAG_WITHOUT_SUPPORTING_DATE")@fn(df)
  expect_equal(nrow(out), 1L)
  expect_identical(out$USUBJID, "S1")
})
