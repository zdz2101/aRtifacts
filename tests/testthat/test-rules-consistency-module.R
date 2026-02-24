rule_by_id <- function(rules, id) {
  Filter(function(rule) identical(rule@id, id), rules)[[1]]
}

test_that("make_rule_pack_consistency() builds expected consistency module rules", {
  rules <- make_rule_pack_consistency(
    domain = "X",
    key_vars = c("USUBJID", "SEQ"),
    impossible_combos = list(c(SEX = "M", PREGNANT = "Y")),
    inconsistent_pairs = list(c(AESER = "Y", AESDTH = "N")),
    categorical_vars = c("CAT"),
    seq_vars = c("SEQ")
  )

  expect_identical(
    names(rules),
    c("R040", "R041", "R043", "R045", "R047", "R048")
  )
})

test_that("R040 and R041 flag impossible/inconsistent categorical patterns", {
  rules <- make_rule_pack_consistency(
    domain = "X",
    key_vars = c("USUBJID", "SEQ"),
    impossible_combos = list(c(SEX = "M", PREGNANT = "Y")),
    inconsistent_pairs = list(c(AESER = "Y", AESDTH = "N"))
  )

  df <- data.frame(
    USUBJID = c("S1", "S2"),
    SEQ = c(1, 2),
    SEX = c("M", "F"),
    PREGNANT = c("Y", "Y"),
    AESER = c("Y", "N"),
    AESDTH = c("N", "N"),
    stringsAsFactors = FALSE
  )

  expect_gt(nrow(rule_by_id(rules, "R040_IMPOSSIBLE_COMBOS")@fn(df)), 0L)
  expect_gt(nrow(rule_by_id(rules, "R041_INCONSISTENT_PAIRS")@fn(df)), 0L)
})

test_that("R043 flags placeholder text", {
  rules <- make_rule_pack_consistency(domain = "X", key_vars = c("USUBJID", "SEQ"))
  df <- data.frame(
    USUBJID = c("S1", "S2"),
    SEQ = c(1, 2),
    COMMENT = c("N/A", "valid"),
    stringsAsFactors = FALSE
  )

  out <- rule_by_id(rules, "R043_PLACEHOLDER_TEXT")@fn(df)
  expect_gt(nrow(out), 0L)
  expect_true(any(out$BAD_VALUE == "N/A"))
})

test_that("R045 flags categorical distinct-count explosion", {
  rules <- make_rule_pack_consistency(
    domain = "X",
    key_vars = c("USUBJID", "SEQ"),
    categorical_vars = c("CAT"),
    max_distinct_values = 3L
  )

  df <- data.frame(
    USUBJID = paste0("S", 1:5),
    SEQ = 1:5,
    CAT = LETTERS[1:5],
    stringsAsFactors = FALSE
  )

  expect_gt(nrow(rule_by_id(rules, "R045_DISTINCT_VALUES_EXPLOSION")@fn(df)), 0L)
})

test_that("R047 flags sequence issues within subject", {
  rules <- make_rule_pack_consistency(
    domain = "X",
    key_vars = c("USUBJID", "SEQ"),
    seq_vars = c("SEQ")
  )

  df <- data.frame(
    USUBJID = c("S1", "S1", "S1", "S2", "S2"),
    SEQ = c(1, 3, 3, 1, 2),
    stringsAsFactors = FALSE
  )

  out <- rule_by_id(rules, "R047_SEQ_GAPS")@fn(df)
  expect_gt(nrow(out), 0L)
  expect_true(any(out$USUBJID == "S1"))
})

test_that("R048 flags record-count outlier subjects", {
  rules <- make_rule_pack_consistency(
    domain = "X",
    key_vars = c("USUBJID", "SEQ"),
    record_count_iqr_multiplier = 1
  )

  df <- data.frame(
    USUBJID = c(rep("S1", 1), rep("S2", 2), rep("S3", 2), rep("S4", 3), rep("S5", 10)),
    SEQ = c(
      1,
      1, 2,
      1, 2,
      1, 2, 3,
      seq_len(10)
    ),
    stringsAsFactors = FALSE
  )

  out <- rule_by_id(rules, "R048_RECORD_COUNT_OUTLIER")@fn(df)
  expect_gt(nrow(out), 0L)
  expect_true(any(out$USUBJID == "S5"))
})
