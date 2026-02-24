rule_by_id <- function(rules, id) {
  Filter(function(rule) identical(rule@id, id), rules)[[1]]
}

test_that("make_rule_pack_keys() builds expected key module rules", {
  rules <- make_rule_pack_keys(
    domain = "X",
    key_vars = c("ID", "SEQ")
  )

  expect_identical(
    vapply(rules, function(rule) rule@id, character(1)),
    c(
      "R001_KEY_MISSING",
      "R002_DUPLICATE_KEYS",
      "R009_HIGH_DUP_RATE_SITE",
      "R010_HIGH_DUP_RATE_SUBJECT"
    )
  )
})

test_that("R001 and R002 flag missing and duplicate keys", {
  rules <- make_rule_pack_keys(domain = "X", key_vars = c("ID", "SEQ"))
  df <- data.frame(
    ID = c("A", "A", "A"),
    SEQ = c(1, 1, NA),
    USUBJID = c("S1", "S1", "S1"),
    SITEID = c("P1", "P1", "P1"),
    stringsAsFactors = FALSE
  )

  expect_gt(nrow(rule_by_id(rules, "R001_KEY_MISSING")@fn(df)), 0L)
  expect_gt(nrow(rule_by_id(rules, "R002_DUPLICATE_KEYS")@fn(df)), 0L)
})

test_that("R009 flags sites above duplicate-rate threshold", {
  rules <- make_rule_pack_keys(
    domain = "X",
    key_vars = c("ID", "SEQ"),
    dup_rate_threshold = 0.4
  )

  df <- data.frame(
    ID = c("A", "A", "B", "C", "D", "E"),
    SEQ = c(1, 1, 1, 1, 1, 1),
    USUBJID = c("S1", "S1", "S2", "S3", "S4", "S5"),
    SITEID = c("SITE01", "SITE01", "SITE01", "SITE02", "SITE02", "SITE02"),
    stringsAsFactors = FALSE
  )

  out <- rule_by_id(rules, "R009_HIGH_DUP_RATE_SITE")@fn(df)
  expect_gt(nrow(out), 0L)
  expect_true(all(out$SITEID == "SITE01"))
  expect_true("DUP_RATE" %in% names(out))
})

test_that("R010 flags subjects above duplicate-rate threshold", {
  rules <- make_rule_pack_keys(
    domain = "X",
    key_vars = c("ID", "SEQ"),
    dup_rate_threshold = 0.4
  )

  df <- data.frame(
    ID = c("A", "A", "B", "C", "D", "E"),
    SEQ = c(1, 1, 1, 1, 1, 1, 1),
    USUBJID = c("SUBJ01", "SUBJ01", "SUBJ02", "SUBJ03", "SUBJ04", "SUBJ05"),
    SITEID = c("SITE01", "SITE01", "SITE01", "SITE02", "SITE02", "SITE02"),
    stringsAsFactors = FALSE
  )

  out <- rule_by_id(rules, "R010_HIGH_DUP_RATE_SUBJECT")@fn(df)
  expect_gt(nrow(out), 0L)
  expect_true(all(out$USUBJID == "SUBJ01"))
  expect_true("DUP_RATE" %in% names(out))
})
