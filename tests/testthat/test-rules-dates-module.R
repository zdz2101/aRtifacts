rule_by_id <- function(rules, id) {
  Filter(function(rule) identical(rule@id, id), rules)[[1]]
}

test_that("make_rule_pack_dates() builds expected date module rules when fully configured", {
  rules <- make_rule_pack_dates(
    domain = "X",
    key_vars = c("ID"),
    start_end_pairs = list(c("STARTDT", "ENDDT")),
    date_vars = c("DT", "STARTDT", "ENDDT"),
    time_date_pairs = list(c("TM", "DT")),
    visit_order_var = "VISITNUM",
    study_start = "2020-01-01",
    study_end = "2025-12-31",
    cutoff_date = as.Date("2024-01-01")
  )

  expect_identical(
    names(rules),
    c("R011", "R012", "R013", "R014", "R015", "R016", "R017", "R018")
  )
})

test_that("R011 and R014 flag bad start/end date relationships", {
  rules <- make_rule_pack_dates(
    domain = "X",
    key_vars = c("ID"),
    start_end_pairs = list(c("STARTDT", "ENDDT")),
    date_vars = c("STARTDT", "ENDDT")
  )

  df <- data.frame(
    ID = c("A", "B"),
    STARTDT = c("2024-02-10", NA),
    ENDDT = c("2024-02-01", "2024-03-01"),
    stringsAsFactors = FALSE
  )

  expect_gt(nrow(rule_by_id(rules, "R011_START_AFTER_END")@fn(df)), 0L)
  expect_gt(nrow(rule_by_id(rules, "R014_END_WITHOUT_START")@fn(df)), 0L)
})

test_that("R013, R015, R017 and R018 flag format/time/timestamp/future issues", {
  rules <- make_rule_pack_dates(
    domain = "X",
    key_vars = c("ID"),
    date_vars = c("DT"),
    time_date_pairs = list(c("TM", "DT")),
    cutoff_date = as.Date("2024-01-01")
  )

  df <- data.frame(
    ID = c("A", "B", "C", "D", "E"),
    DT = c("2024-1-01", "", "2026-01-01", "2024-06-01", "2024-06-01"),
    TM = c("", "08:00", "", "", ""),
    stringsAsFactors = FALSE
  )

  expect_gt(nrow(rule_by_id(rules, "R013_DATE_FORMAT")@fn(df)), 0L)
  expect_gt(nrow(rule_by_id(rules, "R015_TIME_WITHOUT_DATE")@fn(df)), 0L)
  expect_gt(nrow(rule_by_id(rules, "R017_IDENTICAL_TIMESTAMPS")@fn(df)), 0L)
  expect_gt(nrow(rule_by_id(rules, "R018_FUTURE_DATED")@fn(df)), 0L)
})
