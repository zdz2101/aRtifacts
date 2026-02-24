# ---------------------------------------------------------------------------
# run_flags()
# ---------------------------------------------------------------------------

orders_df <- function() {
  data.frame(
    order_id = c(101L, 102L, 103L),
    customer = c("Alice", "Bob", "Carol"),
    amount = c(250, 480, 120),
    stringsAsFactors = FALSE
  )
}

orders_art <- function(build_id = "build-1") {
  dom <- new_domain_snapshot(
    name = "orders",
    data = orders_df(),
    key_vars = "order_id",
    build_id = build_id
  )
  new_artifact_snapshot(
    domains = list(orders = dom),
    build_id = build_id
  )
}

rule_high_amount <- function() {
  new_flag_rule(
    id = "HIGH_AMOUNT",
    domain = "orders",
    severity = "warning",
    category = "review",
    owner_team = "Finance",
    fn = function(df) {
      out <- df[!is.na(df$amount) & df$amount > 400, , drop = FALSE]
      if (!nrow(out)) return(data.frame())
      out[, c("order_id", "amount"), drop = FALSE]
    }
  )
}

test_that("run_flags() returns empty data.frame when rules list is empty", {
  result <- run_flags(orders_art(), list())
  expect_equal(nrow(result), 0L)
})

test_that("run_flags() returns empty data.frame when no rows are flagged", {
  rule <- new_flag_rule(
    id = "NEVER_FIRES", domain = "orders",
    fn = function(df) data.frame()
  )
  result <- run_flags(orders_art(), list(rule))
  expect_equal(nrow(result), 0L)
})

test_that("run_flags() returns flagged rows with metadata columns", {
  result <- run_flags(orders_art(), list(rule_high_amount()))

  expect_true(nrow(result) > 0L)
  expect_true(all(c("RULE_ID", "DOMAIN", "SEVERITY", "CATEGORY", "OWNER_TEAM") %in% names(result)))
})

test_that("run_flags() metadata columns carry correct values", {
  result <- run_flags(orders_art(), list(rule_high_amount()))

  expect_equal(unique(result$RULE_ID), "HIGH_AMOUNT")
  expect_equal(unique(result$DOMAIN), "orders")
  expect_equal(unique(result$SEVERITY), "warning")
  expect_equal(unique(result$CATEGORY), "review")
  expect_equal(unique(result$OWNER_TEAM), "Finance")
})

test_that("run_flags() only flags rows meeting the rule threshold", {
  result <- run_flags(orders_art(), list(rule_high_amount()))
  # only order 102 has amount > 400
  expect_equal(result$order_id, 102L)
  expect_equal(result$amount, 480)
})

test_that("run_flags() skips a rule targeting a domain not in the artifact", {
  rule <- new_flag_rule(
    id = "GHOST", domain = "NO_SUCH_DOMAIN",
    fn = function(df) df
  )
  result <- run_flags(orders_art(), list(rule))
  expect_equal(nrow(result), 0L)
})

test_that("run_flags() combines results from multiple rules", {
  rule1 <- new_flag_rule(
    id = "GT400", domain = "orders",
    fn = function(df) df[df$amount > 400, c("order_id", "amount"), drop = FALSE]
  )
  rule2 <- new_flag_rule(
    id = "LT200", domain = "orders",
    fn = function(df) df[df$amount < 200, c("order_id", "amount"), drop = FALSE]
  )
  result <- run_flags(orders_art(), list(rule1, rule2))
  expect_setequal(result$RULE_ID, c("GT400", "LT200"))
})

test_that("run_flags() errors when artifact is not an ArtifactSnapshot", {
  expect_error(run_flags(data.frame(), list()), regexp = "ArtifactSnapshot")
})

test_that("run_flags() errors when a rules entry is not a FlagRule", {
  expect_error(
    run_flags(orders_art(), list("not_a_rule")),
    regexp = "FlagRule"
  )
})

# ---------------------------------------------------------------------------
# diff_flags()
# ---------------------------------------------------------------------------

test_that("diff_flags() classifies all new flags as introduced when old is empty", {
  old_f <- data.frame()
  new_f <- data.frame(USUBJID = "A", DOMAIN = "X", RULE_ID = "R1", stringsAsFactors = FALSE)

  result <- diff_flags(old_f, new_f)
  expect_equal(nrow(result$introduced), 1L)
  expect_equal(nrow(result$resolved), 0L)
  expect_equal(nrow(result$persisting), 0L)
})

test_that("diff_flags() classifies all old flags as resolved when new is empty", {
  old_f <- data.frame(USUBJID = "A", DOMAIN = "X", RULE_ID = "R1", stringsAsFactors = FALSE)
  new_f <- data.frame()

  result <- diff_flags(old_f, new_f)
  expect_equal(nrow(result$resolved), 1L)
  expect_equal(nrow(result$introduced), 0L)
  expect_equal(nrow(result$persisting), 0L)
})

test_that("diff_flags() correctly classifies persisting flags", {
  flag_row <- data.frame(USUBJID = "A", DOMAIN = "X", RULE_ID = "R1", stringsAsFactors = FALSE)
  result   <- diff_flags(flag_row, flag_row)

  expect_equal(nrow(result$persisting), 1L)
  expect_equal(nrow(result$introduced), 0L)
  expect_equal(nrow(result$resolved), 0L)
})

test_that("diff_flags() handles a mixed scenario correctly", {
  old_f <- data.frame(
    USUBJID = c("A", "B"), DOMAIN = "X", RULE_ID = "R1",
    stringsAsFactors = FALSE
  )
  new_f <- data.frame(
    USUBJID = c("B", "C"), DOMAIN = "X", RULE_ID = "R1",
    stringsAsFactors = FALSE
  )
  result <- diff_flags(old_f, new_f)

  expect_equal(result$introduced$USUBJID, "C")
  expect_equal(result$resolved$USUBJID, "A")
  expect_equal(result$persisting$USUBJID, "B")
})

test_that("diff_flags() returns list with three elements", {
  result <- diff_flags(data.frame(), data.frame())
  expect_named(result, c("introduced", "resolved", "persisting"))
})
