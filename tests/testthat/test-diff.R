# ---------------------------------------------------------------------------
# diff_keys()
# ---------------------------------------------------------------------------

orders_df <- function() {
  data.frame(
    order_id = c(101L, 102L, 103L),
    customer = c("Alice", "Bob", "Carol"),
    amount = c(250, 480, 120),
    stringsAsFactors = FALSE
  )
}

orders_dom <- function(build_id = "build-1") {
  new_domain_snapshot(
    name = "orders",
    data = orders_df(),
    key_vars = "order_id",
    build_id = build_id
  )
}

orders_art <- function(build_id = "build-1") {
  new_artifact_snapshot(
    domains = list(orders = orders_dom(build_id = build_id)),
    build_id = build_id
  )
}

test_that("diff_keys() returns empty added/removed when snapshots are identical", {
  dom <- orders_dom()
  result <- diff_keys(dom, dom)
  expect_equal(nrow(result$added), 0L)
  expect_equal(nrow(result$removed), 0L)
})

test_that("diff_keys() detects an added key", {
  old_dom <- orders_dom()
  new_df  <- rbind(orders_df(), data.frame(order_id = 999L, customer = "Dave", amount = 10, stringsAsFactors = FALSE))
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_keys(old_dom, new_dom)
  expect_equal(nrow(result$added), 1L)
  expect_equal(result$added$order_id, 999L)
  expect_equal(nrow(result$removed), 0L)
})

test_that("diff_keys() detects a removed key", {
  old_dom <- orders_dom()
  new_df  <- orders_df()[-1, ]   # remove order 101
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_keys(old_dom, new_dom)
  expect_equal(nrow(result$removed), 1L)
  expect_equal(result$removed$order_id, 101L)
  expect_equal(nrow(result$added), 0L)
})

test_that("diff_keys() handles simultaneous additions and removals", {
  old_dom <- orders_dom()
  new_df  <- rbind(
    orders_df()[-1, ],   # remove 101
    data.frame(order_id = 999L, customer = "Dave", amount = 10, stringsAsFactors = FALSE)
  )
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_keys(old_dom, new_dom)
  expect_equal(result$removed$order_id, 101L)
  expect_equal(result$added$order_id, 999L)
})

test_that("diff_keys() errors when old_dom is not a DomainSnapshot", {
  expect_error(diff_keys(data.frame(), orders_dom()), regexp = "old_dom")
})

test_that("diff_keys() errors when new_dom is not a DomainSnapshot", {
  expect_error(diff_keys(orders_dom(), data.frame()), regexp = "new_dom")
})

test_that("diff_keys() errors when key_vars differ between snapshots", {
  dom1 <- new_domain_snapshot("x", orders_df(), key_vars = "order_id")
  dom2 <- new_domain_snapshot("x", orders_df(), key_vars = "customer")
  expect_error(diff_keys(dom1, dom2), regexp = "key_vars")
})

# ---------------------------------------------------------------------------
# diff_values()
# ---------------------------------------------------------------------------

test_that("diff_values() returns empty data.frame when data is identical", {
  dom    <- orders_dom()
  result <- diff_values(dom, dom, vars = "amount")
  expect_equal(nrow(result), 0L)
})

test_that("diff_values() detects a changed numeric value", {
  old_dom <- orders_dom()
  new_df  <- orders_df()
  new_df$amount[new_df$order_id == 102L] <- 999
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_values(old_dom, new_dom, vars = "amount")
  expect_equal(nrow(result), 1L)
  expect_equal(result$var, "amount")
  expect_equal(as.numeric(result$old), 480)
  expect_equal(as.numeric(result$new), 999)
})

test_that("diff_values() detects a changed character value", {
  old_dom <- orders_dom()
  new_df  <- orders_df()
  new_df$customer[new_df$order_id == 101L] <- "Alicia"
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_values(old_dom, new_dom, vars = "customer")
  expect_equal(nrow(result), 1L)
  expect_equal(result$old, "Alice")
  expect_equal(result$new, "Alicia")
})

test_that("diff_values() handles multiple changed variables", {
  old_dom <- orders_dom()
  new_df  <- orders_df()
  new_df$amount[1]   <- 1
  new_df$customer[2] <- "Bobby"
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_values(old_dom, new_dom, vars = c("amount", "customer"))
  expect_equal(nrow(result), 2L)
  expect_setequal(result$var, c("amount", "customer"))
})

test_that("diff_values() returns empty when var is not present in data", {
  dom    <- orders_dom()
  result <- diff_values(dom, dom, vars = "NONEXISTENT")
  expect_equal(nrow(result), 0L)
})

test_that("diff_values() detects NA-to-value change", {
  old_df  <- orders_df()
  old_df$amount[1] <- NA
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_dom <- orders_dom()

  result <- diff_values(old_dom, new_dom, vars = "amount")
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$old))
  expect_equal(as.numeric(result$new), 250)
})

test_that("diff_values() errors when old_dom is not a DomainSnapshot", {
  expect_error(diff_values(list(), orders_dom()), regexp = "old_dom")
})

test_that("diff_values() errors when key_vars differ", {
  dom1 <- new_domain_snapshot("x", orders_df(), key_vars = "order_id")
  dom2 <- new_domain_snapshot("x", orders_df(), key_vars = "customer")
  expect_error(diff_values(dom1, dom2), regexp = "key_vars")
})

# ---------------------------------------------------------------------------
# diff_key_types()
# ---------------------------------------------------------------------------

test_that("diff_key_types() reports unchanged key type when snapshots match", {
  dom <- orders_dom()
  result <- diff_key_types(dom, dom)

  expect_named(result, c("key_var", "old_type", "new_type", "changed"))
  expect_equal(nrow(result), 1L)
  expect_identical(result$key_var, "order_id")
  expect_false(result$changed)
})

test_that("diff_key_types() detects key type drift", {
  old_dom <- orders_dom()
  new_df <- orders_df()
  new_df$order_id <- as.character(new_df$order_id)
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_key_types(old_dom, new_dom)
  expect_equal(nrow(result), 1L)
  expect_identical(result$key_var, "order_id")
  expect_true(result$changed)
  expect_equal(result$old_type, "integer")
  expect_equal(result$new_type, "character")
})

test_that("diff_key_types() errors for non-domain input", {
  expect_error(diff_key_types(list(), orders_dom()), regexp = "old_dom")
  expect_error(diff_key_types(orders_dom(), list()), regexp = "new_dom")
})

# ---------------------------------------------------------------------------
# diff_columns()
# ---------------------------------------------------------------------------

test_that("diff_columns() reports unchanged schema when columns match", {
  dom <- orders_dom()
  result <- diff_columns(dom, dom)

  expect_named(result, c("column", "old_present", "new_present", "status", "changed"))
  expect_identical(result$column, sort(names(orders_df())))
  expect_true(all(result$status == "unchanged"))
  expect_true(all(!result$changed))
})

test_that("diff_columns() detects added and removed columns deterministically", {
  old_dom <- orders_dom()
  new_df <- orders_df()[, c("order_id", "amount"), drop = FALSE]
  new_df$region <- "NA"
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_columns(old_dom, new_dom)
  expect_identical(result$column, sort(unique(c(names(old_dom@data), names(new_dom@data)))))

  removed_row <- result[result$column == "customer", ]
  added_row <- result[result$column == "region", ]

  expect_equal(removed_row$status, "removed")
  expect_true(removed_row$changed)
  expect_equal(added_row$status, "added")
  expect_true(added_row$changed)
})

test_that("diff_columns() errors for non-domain input", {
  expect_error(diff_columns(data.frame(), orders_dom()), regexp = "old_dom")
  expect_error(diff_columns(orders_dom(), data.frame()), regexp = "new_dom")
})

# ---------------------------------------------------------------------------
# diff_column_types()
# ---------------------------------------------------------------------------

test_that("diff_column_types() reports unchanged shared types when snapshots match", {
  dom <- orders_dom()
  result <- diff_column_types(dom, dom)

  expect_named(result, c("column", "old_type", "new_type", "changed"))
  expect_identical(result$column, sort(names(orders_df())))
  expect_true(all(!result$changed))
})

test_that("diff_column_types() detects type drift on shared columns", {
  old_dom <- orders_dom()
  new_df <- orders_df()
  new_df$amount <- as.character(new_df$amount)
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_column_types(old_dom, new_dom)
  amount_row <- result[result$column == "amount", ]

  expect_equal(amount_row$old_type, "numeric")
  expect_equal(amount_row$new_type, "character")
  expect_true(amount_row$changed)
})

test_that("diff_column_types() returns empty data.frame when there are no shared columns", {
  old_dom <- new_domain_snapshot(
    "old",
    data.frame(id = 1:2, value = c("A", "B"), stringsAsFactors = FALSE),
    key_vars = "id"
  )
  new_dom <- new_domain_snapshot(
    "new",
    data.frame(key = 1:2, amount = c(1, 2), stringsAsFactors = FALSE),
    key_vars = "key"
  )

  result <- diff_column_types(old_dom, new_dom)
  expect_equal(nrow(result), 0L)
  expect_named(result, c("column", "old_type", "new_type", "changed"))
})

test_that("diff_column_types() errors for non-domain input", {
  expect_error(diff_column_types(list(), orders_dom()), regexp = "old_dom")
  expect_error(diff_column_types(orders_dom(), list()), regexp = "new_dom")
})

# ---------------------------------------------------------------------------
# diff_meta_artifact()
# ---------------------------------------------------------------------------

test_that("diff_meta_artifact() returns a 5-row data.frame with correct columns", {
  art1   <- orders_art("build-1")
  art2   <- orders_art("build-2")
  result <- diff_meta_artifact(art1, art2)

  expect_equal(nrow(result), 5L)
  expect_named(result, c("field", "old", "new", "changed"))
})

test_that("diff_meta_artifact() flags build_id change as changed = TRUE", {
  art1   <- orders_art("build-1")
  art2   <- orders_art("build-2")
  result <- diff_meta_artifact(art1, art2)

  bid_row <- result[result$field == "build_id", ]
  expect_equal(bid_row$old, "build-1")
  expect_equal(bid_row$new, "build-2")
  expect_true(bid_row$changed)
})

test_that("diff_meta_artifact() marks unchanged fields as changed = FALSE", {
  art    <- orders_art()
  result <- diff_meta_artifact(art, art)
  expect_true(all(!result$changed))
})

test_that("diff_meta_artifact() errors when old_art is not an ArtifactSnapshot", {
  expect_error(diff_meta_artifact(list(), orders_art()), regexp = "old_art")
})

test_that("diff_meta_artifact() errors when new_art is not an ArtifactSnapshot", {
  expect_error(diff_meta_artifact(orders_art(), list()), regexp = "new_art")
})
