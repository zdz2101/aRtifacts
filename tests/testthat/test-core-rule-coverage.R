# ---------------------------------------------------------------------------
# Core rule coverage across module builders
# ---------------------------------------------------------------------------

.extract_core_id_prefix <- function(rule_id) {
  sub("^(R[0-9]{3}).*$", "\\1", rule_id)
}

.expected_implemented_core_ids <- c(
  sprintf("R%03d", c(1, 2, 9, 10)),
  sprintf("R%03d", 11:18),
  sprintf("R%03d", 21:27),
  sprintf("R%03d", c(31, 33, 34, 35, 37, 38, 39)),
  sprintf("R%03d", c(40, 41, 43, 45, 47, 48))
)

test_that("all module rule IDs map to entries in rule_catalog_core()", {
  rules <- c(
    make_rule_pack_keys(
      domain = "LB",
      key_vars = c("STUDYID", "USUBJID", "LBSEQ")
    ),
    make_rule_pack_dates(
      domain = "LB",
      key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
      start_end_pairs = list(c("AESTDTC", "AEENDTC")),
      date_vars = c("LBDTC"),
      time_date_pairs = list(c("LBTM", "LBDTC")),
      visit_order_var = "VISITNUM",
      study_start = "2020-01-01",
      study_end = "2030-12-31"
    ),
    make_rule_pack_missingness(
      domain = "LB",
      key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
      required_vars = c("STUDYID", "USUBJID", "LBSEQ", "LBTESTCD"),
      visit_var = "VISITNUM",
      baseline_flag_var = "ABLFL"
    ),
    make_rule_pack_plausibility(
      domain = "LB",
      key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
      numeric_vars = c("LBSTRESN"),
      non_negative_vars = c("LBSTRESN"),
      value_ranges = list(LBSTRESN = c(0, 30)),
      visit_order_var = "VISITNUM",
      max_within_change = list(LBSTRESN = 5)
    ),
    make_rule_pack_consistency(
      domain = "LB",
      key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
      impossible_combos = list(c(SEX = "M", PREGNANT = "Y")),
      inconsistent_pairs = list(c(AESER = "Y", AESDTH = "N")),
      categorical_vars = c("LBTESTCD"),
      seq_vars = c("LBSEQ")
    )
  )

  prefixes <- unique(vapply(rules, function(r) .extract_core_id_prefix(r@id), character(1)))
  catalog_ids <- rule_catalog_core()$rule_id
  expect_true(all(prefixes %in% catalog_ids))
})

test_that("implemented core rules match expected implemented subset", {
  rules <- c(
    make_rule_pack_keys(
      domain = "LB",
      key_vars = c("STUDYID", "USUBJID", "LBSEQ")
    ),
    make_rule_pack_dates(
      domain = "LB",
      key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
      start_end_pairs = list(c("AESTDTC", "AEENDTC")),
      date_vars = c("LBDTC"),
      time_date_pairs = list(c("LBTM", "LBDTC")),
      visit_order_var = "VISITNUM",
      study_start = "2020-01-01",
      study_end = "2030-12-31"
    ),
    make_rule_pack_missingness(
      domain = "LB",
      key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
      required_vars = c("STUDYID", "USUBJID", "LBSEQ", "LBTESTCD"),
      visit_var = "VISITNUM",
      baseline_flag_var = "ABLFL"
    ),
    make_rule_pack_plausibility(
      domain = "LB",
      key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
      numeric_vars = c("LBSTRESN"),
      non_negative_vars = c("LBSTRESN"),
      value_ranges = list(LBSTRESN = c(0, 30)),
      visit_order_var = "VISITNUM",
      max_within_change = list(LBSTRESN = 5)
    ),
    make_rule_pack_consistency(
      domain = "LB",
      key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
      impossible_combos = list(c(SEX = "M", PREGNANT = "Y")),
      inconsistent_pairs = list(c(AESER = "Y", AESDTH = "N")),
      categorical_vars = c("LBTESTCD"),
      seq_vars = c("LBSEQ")
    )
  )

  prefixes <- sort(unique(vapply(rules, function(r) .extract_core_id_prefix(r@id), character(1))))
  expect_identical(prefixes, sort(.expected_implemented_core_ids))
})

test_that("non-implemented core rules are explicitly identifiable", {
  all_ids <- sprintf("R%03d", seq_len(50))
  unimplemented <- setdiff(all_ids, .expected_implemented_core_ids)

  expect_identical(
    unimplemented,
    sprintf("R%03d", c(3:8, 19, 20, 28, 29, 30, 32, 36, 42, 44, 46, 49, 50))
  )
})
