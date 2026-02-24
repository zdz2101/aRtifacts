# ---------------------------------------------------------------------------
# Core50 topology coverage across axis packs
# ---------------------------------------------------------------------------

test_that("all Core50 pack IDs use axis-based prefixes", {
  rules <- c(
    pack_core_50_a1(
      domain = "LB",
      key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
      date_var = "LBDTC",
      start_var = "LBDTC",
      end_var = "LBDTC",
      time_var = "LBTM",
      condition_var = "LBOCCUR",
      required_var = "LBTESTCD",
      numeric_var = "LBSTRESN",
      numeric_min = 0,
      numeric_max = 1000,
      categorical_var = "LBSTAT",
      allowed_levels = c("", "NOT DONE"),
      duration_var = "LBDUR"
    ),
    pack_core_50_a2(domain = "LB", key_vars = c("STUDYID", "USUBJID", "LBSEQ"), skew_value_var = "LBSTRESN"),
    pack_core_50_a3(),
    pack_core_50_a4(),
    pack_core_50_a5(domain_primary = "LB", keys = c("STUDYID", "USUBJID", "LBSEQ")),
    pack_core_50_a6(domain_primary = "LB", keys = c("STUDYID", "USUBJID", "LBSEQ")),
    pack_core_50_a7()
  )

  ids <- vapply(rules, function(r) r@id, character(1))
  expect_true(all(grepl("^[A-Z]+-[0-9]{3}$", ids)))
})

test_that("Core50 packs expose expected topology coverage", {
  packs <- list(
    pack_core_50_a1(
      domain = "LB",
      key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
      date_var = "LBDTC",
      start_var = "LBDTC",
      end_var = "LBDTC",
      time_var = "LBTM",
      condition_var = "LBOCCUR",
      required_var = "LBTESTCD",
      numeric_var = "LBSTRESN",
      numeric_min = 0,
      numeric_max = 1000,
      categorical_var = "LBSTAT",
      allowed_levels = c("", "NOT DONE"),
      duration_var = "LBDUR"
    ),
    pack_core_50_a2(domain = "LB", key_vars = c("STUDYID", "USUBJID", "LBSEQ"), skew_value_var = "LBSTRESN"),
    pack_core_50_a3(),
    pack_core_50_a4(),
    pack_core_50_a5(domain_primary = "LB", keys = c("STUDYID", "USUBJID", "LBSEQ")),
    pack_core_50_a6(domain_primary = "LB", keys = c("STUDYID", "USUBJID", "LBSEQ")),
    pack_core_50_a7()
  )

  topologies <- unlist(lapply(packs, function(pack) unique(vapply(pack, function(r) r@topology, character(1)))))
  expect_setequal(unique(topologies), paste0("A", 1:7))
})

test_that("Core50 pack sizes match master-plan allocations", {
  sizes <- c(
    length(pack_core_50_a1(
      domain = "LB",
      key_vars = c("STUDYID", "USUBJID", "LBSEQ"),
      date_var = "LBDTC",
      start_var = "LBDTC",
      end_var = "LBDTC",
      time_var = "LBTM",
      condition_var = "LBOCCUR",
      required_var = "LBTESTCD",
      numeric_var = "LBSTRESN",
      numeric_min = 0,
      numeric_max = 1000,
      categorical_var = "LBSTAT",
      allowed_levels = c("", "NOT DONE"),
      duration_var = "LBDUR"
    )),
    length(pack_core_50_a2(domain = "LB", key_vars = c("STUDYID", "USUBJID", "LBSEQ"), skew_value_var = "LBSTRESN")),
    length(pack_core_50_a3()),
    length(pack_core_50_a4()),
    length(pack_core_50_a5(domain_primary = "LB", keys = c("STUDYID", "USUBJID", "LBSEQ"))),
    length(pack_core_50_a6(domain_primary = "LB", keys = c("STUDYID", "USUBJID", "LBSEQ"))),
    length(pack_core_50_a7())
  )

  expect_identical(sizes, c(10L, 8L, 8L, 4L, 5L, 6L, 4L))
})
