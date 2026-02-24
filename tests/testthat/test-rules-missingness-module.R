rule_by_id <- function(rules, id) {
  Filter(function(rule) identical(rule@id, id), rules)[[1]]
}

test_that("make_rule_pack_missingness() builds expected missingness module rules", {
  rules <- make_rule_pack_missingness(
    domain = "X",
    key_vars = c("ID"),
    required_vars = c("ID", "REQ"),
    visit_var = "VISIT",
    baseline_flag_var = "ABLFL"
  )

  expect_identical(
    vapply(rules, function(rule) rule@id, character(1)),
    c(
      "R021_MISSINGNESS_OVERALL",
      "R022_MISSINGNESS_BY_SITE",
      "R023_MISSINGNESS_BY_VISIT",
      "R024_REQUIRED_MISSING",
      "R025_DOMAIN_EMPTY",
      "R026_NO_FOLLOWUP",
      "R027_MISSING_BASELINE"
    )
  )
})

test_that("R021, R022 and R023 flag high missingness overall/by group", {
  rules <- make_rule_pack_missingness(
    domain = "X",
    key_vars = c("ID"),
    subject_var = "USUBJID",
    site_var = "SITEID",
    visit_var = "VISIT",
    missingness_threshold = 0.3
  )

  df <- data.frame(
    ID = 1:6,
    USUBJID = c("S1", "S2", "S3", "S4", "S5", "S6"),
    SITEID = c("A", "A", "A", "B", "B", "B"),
    VISIT = c("V1", "V1", "V2", "V1", "V2", "V2"),
    VALUE = c(NA, NA, NA, 1, 2, 3),
    stringsAsFactors = FALSE
  )

  expect_gt(nrow(rule_by_id(rules, "R021_MISSINGNESS_OVERALL")@fn(df)), 0L)
  expect_gt(nrow(rule_by_id(rules, "R022_MISSINGNESS_BY_SITE")@fn(df)), 0L)
  expect_gt(nrow(rule_by_id(rules, "R023_MISSINGNESS_BY_VISIT")@fn(df)), 0L)
})

test_that("R024 and R025 flag required-variable issues and empty domain", {
  rules <- make_rule_pack_missingness(
    domain = "X",
    key_vars = c("ID"),
    required_vars = c("ID", "REQ")
  )

  df_missing <- data.frame(ID = c(1, 2), REQ = c("ok", NA), stringsAsFactors = FALSE)
  df_empty <- df_missing[0, , drop = FALSE]

  expect_gt(nrow(rule_by_id(rules, "R024_REQUIRED_MISSING")@fn(df_missing)), 0L)
  expect_gt(nrow(rule_by_id(rules, "R025_DOMAIN_EMPTY")@fn(df_empty)), 0L)
})

test_that("R026 and R027 flag follow-up and baseline-gaps by subject", {
  rules <- make_rule_pack_missingness(
    domain = "X",
    key_vars = c("ID"),
    subject_var = "USUBJID",
    baseline_flag_var = "ABLFL"
  )

  df <- data.frame(
    ID = 1:4,
    USUBJID = c("S1", "S2", "S2", "S3"),
    ABLFL = c("Y", "Y", NA, NA),
    stringsAsFactors = FALSE
  )

  no_follow <- rule_by_id(rules, "R026_NO_FOLLOWUP")@fn(df)
  miss_base <- rule_by_id(rules, "R027_MISSING_BASELINE")@fn(df)

  expect_true("S1" %in% no_follow$USUBJID)
  expect_true("S3" %in% miss_base$USUBJID)
})
