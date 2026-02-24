# ---------------------------------------------------------------------------
# check_tabulation_domain()
# ---------------------------------------------------------------------------

test_that("check_tabulation_domain() returns empty issue table for clean data", {
  result <- check_tabulation_domain(
    data         = example_sdtm_lb(),
    domain       = "LB",
    key_vars     = c("STUDYID", "USUBJID", "LBSEQ"),
    required_vars = c("STUDYID", "USUBJID", "LBSEQ", "LBTESTCD")
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("check_tabulation_domain() has correct column schema", {
  result <- check_tabulation_domain(
    data     = example_sdtm_lb(),
    key_vars = c("STUDYID", "USUBJID", "LBSEQ")
  )
  expect_named(result, c("check", "severity", "message", "n"))
})

test_that("check_tabulation_domain() flags missing required variables", {
  result <- check_tabulation_domain(
    data          = example_sdtm_lb(),
    key_vars      = c("STUDYID", "USUBJID", "LBSEQ"),
    required_vars = c("NONEXISTENT_VAR")
  )
  expect_true(any(result$check == "R024_REQUIRED_MISSING"))
  expect_true(any(result$severity == "error"))
})

test_that("check_tabulation_domain() flags missing key variables", {
  result <- check_tabulation_domain(
    data     = example_sdtm_lb(),
    key_vars = c("NONEXISTENT_KEY")
  )
  expect_true(any(result$check == "R001_KEY_MISSING"))
})

test_that("check_tabulation_domain() flags duplicate keys", {
  df <- rbind(example_sdtm_lb(), example_sdtm_lb()[1, ])  # duplicate first row
  result <- check_tabulation_domain(
    data     = df,
    key_vars = c("STUDYID", "USUBJID", "LBSEQ")
  )
  expect_true(any(result$check == "R002_DUPLICATE_KEYS"))
  expect_true(any(result$severity == "error"))
})

test_that("check_tabulation_domain() flags missing key field values", {
  df           <- example_sdtm_lb()
  df$LBSEQ[1] <- NA
  result       <- check_tabulation_domain(data = df, key_vars = c("STUDYID", "USUBJID", "LBSEQ"))
  expect_true(any(result$check == "R001_KEY_MISSING"))
})

test_that("check_tabulation_domain() flags negative values", {
  result <- check_tabulation_domain(
    data               = example_sdtm_lb(),
    key_vars           = c("STUDYID", "USUBJID", "LBSEQ"),
    non_negative_vars  = "LBSTRESN"
  )
  # example_sdtm_lb has a -1.0 LBSTRESN value
  expect_true(any(result$check == "R033_NEGATIVE_NOT_ALLOWED"))
  expect_true(any(result$severity == "warning"))
})

test_that("check_tabulation_domain() flags bad ISO date format", {
  df             <- example_sdtm_lb()
  df$LBDTC[1]   <- "01/01/2024"  # wrong format
  result <- check_tabulation_domain(
    data      = df,
    key_vars  = c("STUDYID", "USUBJID", "LBSEQ"),
    date_vars = "LBDTC"
  )
  expect_true(any(result$check == "R013_DATE_FORMAT"))
})

test_that("check_tabulation_domain() does NOT flag well-formatted dates", {
  result <- check_tabulation_domain(
    data      = example_sdtm_lb(),
    key_vars  = c("STUDYID", "USUBJID", "LBSEQ"),
    date_vars = "LBDTC"
  )
  expect_false(any(result$check == "R013_DATE_FORMAT"))
})

test_that("check_tabulation_domain() flags start-after-end date pairs", {
  df <- data.frame(
    id     = 1L,
    start  = "2024-02-01",
    end    = "2024-01-01",   # end before start
    stringsAsFactors = FALSE
  )
  result <- check_tabulation_domain(
    data            = df,
    key_vars        = "id",
    start_end_pairs = list(c("start", "end"))
  )
  expect_true(any(result$check == "R011_START_AFTER_END"))
})

test_that("check_tabulation_domain() errors when data is not a data.frame", {
  expect_error(
    check_tabulation_domain(data = list(), key_vars = "id"),
    regexp = "data.frame"
  )
})

# ---------------------------------------------------------------------------
# make_rule_pack_core()
# ---------------------------------------------------------------------------

test_that("make_rule_pack_core() returns a list of FlagRule objects", {
  pack <- make_rule_pack_core(
    domain       = "LB",
    key_vars     = c("STUDYID", "USUBJID", "LBSEQ"),
    required_vars = c("STUDYID", "USUBJID", "LBSEQ", "LBTESTCD"),
    start_end_pairs = list(c("AESTDTC", "AEENDTC")),
    date_vars = c("LBDTC"),
    time_date_pairs = list(c("LBTM", "LBDTC")),
    visit_order_var = "VISITNUM",
    numeric_vars = c("LBSTRESN"),
    non_negative_vars = c("LBSTRESN"),
    value_ranges = list(LBSTRESN = c(0, 30)),
    max_within_change = list(LBSTRESN = 5),
    impossible_combos = list(c(SEX = "M", PREGNANT = "Y")),
    inconsistent_pairs = list(c(AESER = "Y", AESDTH = "N")),
    categorical_vars = c("LBTESTCD"),
    seq_vars = c("LBSEQ")
  )

  expect_type(pack, "list")
  expect_true(length(pack) >= 20L)
  for (rule in pack) {
    expect_true(S7::S7_inherits(rule, FlagRule))
  }
})

test_that("make_rule_pack_core() includes rules from each module family", {
  pack <- make_rule_pack_core(
    domain = "LB",
    key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
    required_vars = c("STUDYID", "USUBJID", "LBSEQ", "LBTESTCD"),
    start_end_pairs = list(c("AESTDTC", "AEENDTC")),
    date_vars = c("LBDTC"),
    time_date_pairs = list(c("LBTM", "LBDTC")),
    visit_order_var = "VISITNUM",
    numeric_vars = c("LBSTRESN"),
    non_negative_vars = c("LBSTRESN"),
    value_ranges = list(LBSTRESN = c(0, 30)),
    max_within_change = list(LBSTRESN = 5),
    impossible_combos = list(c(SEX = "M", PREGNANT = "Y")),
    inconsistent_pairs = list(c(AESER = "Y", AESDTH = "N")),
    categorical_vars = c("LBTESTCD"),
    seq_vars = c("LBSEQ")
  )

  ids <- vapply(pack, function(rule) rule@id, character(1))
  expect_true(any(grepl("^R001_", ids)))
  expect_true(any(grepl("^R011_", ids)))
  expect_true(any(grepl("^R021_", ids)))
  expect_true(any(grepl("^R031_", ids)))
  expect_true(any(grepl("^R040_", ids)))
})

test_that("make_rule_pack_core() composes module builders exactly", {
  args <- list(
    domain = "LB",
    key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
    required_vars = c("STUDYID", "USUBJID", "LBSEQ", "LBTESTCD"),
    start_end_pairs = list(c("AESTDTC", "AEENDTC")),
    date_vars = c("LBDTC"),
    time_date_pairs = list(c("LBTM", "LBDTC")),
    visit_order_var = "VISITNUM",
    numeric_vars = c("LBSTRESN"),
    non_negative_vars = c("LBSTRESN"),
    value_ranges = list(LBSTRESN = c(0, 30)),
    max_within_change = list(LBSTRESN = 5),
    impossible_combos = list(c(SEX = "M", PREGNANT = "Y")),
    inconsistent_pairs = list(c(AESER = "Y", AESDTH = "N")),
    categorical_vars = c("LBTESTCD"),
    seq_vars = c("LBSEQ")
  )

  core <- do.call(make_rule_pack_core, args)
  modules <- c(
    do.call(make_rule_pack_keys, args),
    do.call(make_rule_pack_dates, args),
    do.call(make_rule_pack_missingness, args),
    do.call(make_rule_pack_plausibility, args),
    do.call(make_rule_pack_consistency, args)
  )

  expect_identical(
    vapply(core, function(rule) rule@id, character(1)),
    vapply(modules, function(rule) rule@id, character(1))
  )
})
