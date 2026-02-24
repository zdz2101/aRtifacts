test_that("rule_r036_site_distribution_shift returns expected DiffFlagRule", {
  rule <- rule_r036_site_distribution_shift(domain = "X", key_vars = c("ID"), numeric_vars = "VAL")
  expect_true(S7::S7_inherits(rule, DiffFlagRule))
  expect_true(startsWith(rule@id, "R036"))
  expect_true(is.function(rule@fn))
})

test_that("rule_r036_site_distribution_shift flags group-level distribution drift", {
  rule <- rule_r036_site_distribution_shift(
    domain = "X",
    key_vars = c("ID"),
    numeric_vars = "VAL",
    group_var = "GROUPID"
  )

  old_df <- data.frame(
    ID = 1:20,
    GROUPID = rep(c("G1", "G2"), each = 10),
    VAL = c(1:10, 1:10),
    stringsAsFactors = FALSE
  )
  new_df <- old_df
  new_df$VAL[new_df$GROUPID == "G1"] <- new_df$VAL[new_df$GROUPID == "G1"] + 8

  old_dom <- new_domain_snapshot("X", old_df, key_vars = "ID")
  new_dom <- new_domain_snapshot("X", new_df, key_vars = "ID")

  out <- rule@fn(old_dom, new_dom)
  expect_true(nrow(out) >= 1L)
  expect_true("group" %in% names(out))
  expect_true("ks_stat" %in% names(out))
  expect_true(any(out$group == "G1"))
  expect_true(any(out$mean_delta != 0))
})

test_that("rule_r036_site_distribution_shift returns empty when there is no shift", {
  rule <- rule_r036_site_distribution_shift(
    domain = "X",
    key_vars = c("ID"),
    numeric_vars = "VAL",
    group_var = "GROUPID"
  )

  old_df <- data.frame(
    ID = 1:20,
    GROUPID = rep(c("G1", "G2"), each = 10),
    VAL = c(1:10, 1:10),
    stringsAsFactors = FALSE
  )
  old_dom <- new_domain_snapshot("X", old_df, key_vars = "ID")
  new_dom <- new_domain_snapshot("X", old_df, key_vars = "ID")

  out <- rule@fn(old_dom, new_dom)
  expect_equal(nrow(out), 0L)
})
