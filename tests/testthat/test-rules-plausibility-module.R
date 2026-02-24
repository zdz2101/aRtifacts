rule_by_id <- function(rules, id) {
  Filter(function(rule) identical(rule@id, id), rules)[[1]]
}

test_that("make_rule_pack_plausibility() builds expected plausibility module rules", {
  rules <- make_rule_pack_plausibility(
    domain = "X",
    key_vars = c("ID"),
    numeric_vars = c("VAL"),
    non_negative_vars = c("VAL"),
    value_ranges = list(VAL = c(0, 100)),
    visit_order_var = "VISITNUM",
    max_within_change = list(VAL = 10)
  )

  expect_identical(
    names(rules),
    c("R031", "R033", "R034", "R035", "R037", "R038", "R039")
  )
})

test_that("R031 and R033 flag out-of-range and negative values", {
  rules <- make_rule_pack_plausibility(
    domain = "X",
    key_vars = c("ID"),
    non_negative_vars = c("VAL"),
    value_ranges = list(VAL = c(0, 100))
  )

  df <- data.frame(ID = 1:3, VAL = c(50, -5, 120), stringsAsFactors = FALSE)

  expect_gt(nrow(rule_by_id(rules, "R031_IMPOSSIBLE_VALUES")@fn(df)), 0L)
  expect_gt(nrow(rule_by_id(rules, "R033_NEGATIVE_NOT_ALLOWED")@fn(df)), 0L)
})

test_that("R034, R035, R038 and R039 flag distribution anomalies", {
  rules <- make_rule_pack_plausibility(
    domain = "X",
    key_vars = c("ID"),
    numeric_vars = c("VAL"),
    zero_rate_threshold = 0.5,
    low_cv_threshold = 0.02,
    round_rate_threshold = 0.8
  )

  df_zeros <- data.frame(ID = 1:10, VAL = c(0, 0, 0, 0, 0, 0, 1, 2, 3, 4), stringsAsFactors = FALSE)
  df_outlier <- data.frame(ID = 1:10, VAL = c(10, 11, 10, 9, 11, 10, 10, 11, 10, 999), stringsAsFactors = FALSE)
  df_lowcv <- data.frame(ID = 1:10, VAL = rep(100, 10), stringsAsFactors = FALSE)
  df_round <- data.frame(ID = 1:10, VAL = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 46), stringsAsFactors = FALSE)

  expect_gt(nrow(rule_by_id(rules, "R034_SUSPICIOUS_ZEROS")@fn(df_zeros)), 0L)
  expect_gt(nrow(rule_by_id(rules, "R035_STATISTICAL_OUTLIERS")@fn(df_outlier)), 0L)
  expect_gt(nrow(rule_by_id(rules, "R038_LOW_VARIANCE")@fn(df_lowcv)), 0L)
  expect_gt(nrow(rule_by_id(rules, "R039_ROUNDING_ANOMALY")@fn(df_round)), 0L)
})

test_that("R037 flags within-subject implausible changes", {
  rules <- make_rule_pack_plausibility(
    domain = "X",
    key_vars = c("USUBJID", "VISITNUM"),
    subject_var = "USUBJID",
    numeric_vars = c("VAL"),
    visit_order_var = "VISITNUM",
    max_within_change = list(VAL = 20)
  )

  df <- data.frame(
    USUBJID = c("S1", "S1", "S1", "S2", "S2"),
    VISITNUM = c(1, 2, 3, 1, 2),
    VAL = c(100, 105, 170, 90, 95),
    stringsAsFactors = FALSE
  )

  out <- rule_by_id(rules, "R037_IMPLAUSIBLE_CHANGE")@fn(df)
  expect_gt(nrow(out), 0L)
  expect_true(any(out$USUBJID == "S1"))
})
