core50_artifact <- function(adsl_df, dm_df, ae_df = NULL, ex_df = NULL, vs_df = NULL, lb_df = NULL, eg_df = NULL) {
  domains <- list(
    ADSL = new_domain_snapshot("ADSL", adsl_df, key_vars = "USUBJID"),
    DM = new_domain_snapshot("DM", dm_df, key_vars = "USUBJID")
  )

  if (!is.null(ae_df)) domains$AE <- new_domain_snapshot("AE", ae_df, key_vars = c("USUBJID", "AESEQ"))
  if (!is.null(ex_df)) domains$EX <- new_domain_snapshot("EX", ex_df, key_vars = c("USUBJID", "EXSEQ"))
  if (!is.null(vs_df)) domains$VS <- new_domain_snapshot("VS", vs_df, key_vars = c("USUBJID", "VISIT"))
  if (!is.null(lb_df)) domains$LB <- new_domain_snapshot("LB", lb_df, key_vars = c("USUBJID", "LBSEQ"))
  if (!is.null(eg_df)) domains$EG <- new_domain_snapshot("EG", eg_df, key_vars = c("USUBJID", "EGSEQ"))

  new_artifact_snapshot(domains = domains)
}

test_that("Core50 topology packs return CoreRule objects", {
  packs <- list(
    pack_core_50_a1(
      domain = "ADSL",
      key_vars = "USUBJID",
      date_var = "RANDDT",
      start_var = "TRTSDT",
      end_var = "TRTEDT",
      time_var = "RANDTM",
      condition_var = "SAFFL",
      required_var = "AGE",
      numeric_var = "AGE",
      numeric_min = 0,
      numeric_max = 120,
      categorical_var = "SEX",
      allowed_levels = c("M", "F"),
      duration_var = "ADURN"
    ),
    pack_core_50_a2(domain = "ADSL", key_vars = "USUBJID", skew_value_var = "AGE"),
    pack_core_50_a3(),
    pack_core_50_a4(),
    pack_core_50_a5(domain_primary = "ADSL", keys = "USUBJID"),
    pack_core_50_a6(domain_primary = "ADSL", keys = "USUBJID"),
    pack_core_50_a7()
  )

  expected_lengths <- c(10L, 8L, 8L, 4L, 5L, 6L, 4L)
  lengths <- vapply(packs, length, integer(1))
  expect_equal(lengths, expected_lengths)

  for (pack in packs) {
    expect_true(all(vapply(pack, function(x) S7::S7_inherits(x, CoreRule), logical(1))))
  }
})

test_that("run_rules() executes A1 rule against current snapshot", {
  adsl <- data.frame(
    USUBJID = c("SUBJ-001", "", "SUBJ-003"),
    AGE = c(34, 41, 55),
    stringsAsFactors = FALSE
  )
  dm <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    SITEID = c("S1", "S1", "S2"),
    stringsAsFactors = FALSE
  )

  art <- core50_artifact(adsl_df = adsl, dm_df = dm)
  ctx <- new_rule_context(current = art)
  rule <- rule_id_001_missing_key_fields(domain = "ADSL", key_vars = "USUBJID")

  out <- run_rules(ctx, list(rule))
  expect_true(nrow(out) >= 1L)
  expect_true(all(c("RULE_ID", "TOPOLOGY", "SCOPE", "INTENT", "SEVERITY", "OWNER_TEAM") %in% names(out)))
  expect_true(all(out$RULE_ID == "ID-001"))
  expect_true(all(out$TOPOLOGY == "A1"))
})

test_that("run_rules() executes A5 cross-snapshot rule", {
  adsl_prev <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    AGE = c(34, 41, 55),
    stringsAsFactors = FALSE
  )
  adsl_cur <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-003"),
    AGE = c(34, 55),
    stringsAsFactors = FALSE
  )

  dm_prev <- data.frame(
    USUBJID = c("SUBJ-001", "SUBJ-002", "SUBJ-003"),
    SITEID = c("S1", "S1", "S2"),
    stringsAsFactors = FALSE
  )
  dm_cur <- dm_prev[dm_prev$USUBJID != "SUBJ-002", , drop = FALSE]

  prev_art <- core50_artifact(adsl_df = adsl_prev, dm_df = dm_prev)
  cur_art <- core50_artifact(adsl_df = adsl_cur, dm_df = dm_cur)
  ctx <- new_rule_context(current = cur_art, previous = prev_art)

  rule <- rule_snap_001_unexpected_key_deletions(domain = "ADSL", keys = "USUBJID")
  out <- run_rules(ctx, list(rule))

  expect_true(nrow(out) >= 1L)
  expect_true(any(out$USUBJID == "SUBJ-002"))
  expect_true(all(out$RULE_ID == "SNAP-001"))
  expect_true(all(out$TOPOLOGY == "A5"))
})
