# ---------------------------------------------------------------------------
# diff_keys()
# ---------------------------------------------------------------------------

test_that("diff_keys() returns empty added/removed when snapshots are identical", {
  dom <- .dom_orders()
  result <- diff_keys(dom, dom)
  expect_equal(nrow(result$added), 0L)
  expect_equal(nrow(result$removed), 0L)
})

test_that("diff_keys() detects an added key", {
  old_dom <- .dom_orders()
  new_df  <- rbind(.df_orders(), data.frame(order_id = 999L, customer = "Dave", amount = 10, stringsAsFactors = FALSE))
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_keys(old_dom, new_dom)
  expect_equal(nrow(result$added), 1L)
  expect_equal(result$added$order_id, 999L)
  expect_equal(nrow(result$removed), 0L)
})

test_that("diff_keys() detects a removed key", {
  old_dom <- .dom_orders()
  new_df  <- .df_orders()[-1, ]   # remove order 101
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_keys(old_dom, new_dom)
  expect_equal(nrow(result$removed), 1L)
  expect_equal(result$removed$order_id, 101L)
  expect_equal(nrow(result$added), 0L)
})

test_that("diff_keys() handles simultaneous additions and removals", {
  old_dom <- .dom_orders()
  new_df  <- rbind(
    .df_orders()[-1, ],   # remove 101
    data.frame(order_id = 999L, customer = "Dave", amount = 10, stringsAsFactors = FALSE)
  )
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_keys(old_dom, new_dom)
  expect_equal(result$removed$order_id, 101L)
  expect_equal(result$added$order_id, 999L)
})

test_that("diff_keys() errors when old_dom is not a DomainSnapshot", {
  expect_error(diff_keys(data.frame(), .dom_orders()), regexp = "old_dom")
})

test_that("diff_keys() errors when new_dom is not a DomainSnapshot", {
  expect_error(diff_keys(.dom_orders(), data.frame()), regexp = "new_dom")
})

test_that("diff_keys() errors when key_vars differ between snapshots", {
  dom1 <- new_domain_snapshot("x", .df_orders(), key_vars = "order_id")
  dom2 <- new_domain_snapshot("x", .df_orders(), key_vars = "customer")
  expect_error(diff_keys(dom1, dom2), regexp = "key_vars")
})

# ---------------------------------------------------------------------------
# diff_values()
# ---------------------------------------------------------------------------

test_that("diff_values() returns empty data.frame when data is identical", {
  dom    <- .dom_orders()
  result <- diff_values(dom, dom, vars = "amount")
  expect_equal(nrow(result), 0L)
})

test_that("diff_values() detects a changed numeric value", {
  old_dom <- .dom_orders()
  new_df  <- .df_orders()
  new_df$amount[new_df$order_id == 102L] <- 999
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_values(old_dom, new_dom, vars = "amount")
  expect_equal(nrow(result), 1L)
  expect_equal(result$var, "amount")
  expect_equal(as.numeric(result$old), 480)
  expect_equal(as.numeric(result$new), 999)
})

test_that("diff_values() detects a changed character value", {
  old_dom <- .dom_orders()
  new_df  <- .df_orders()
  new_df$customer[new_df$order_id == 101L] <- "Alicia"
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_values(old_dom, new_dom, vars = "customer")
  expect_equal(nrow(result), 1L)
  expect_equal(result$old, "Alice")
  expect_equal(result$new, "Alicia")
})

test_that("diff_values() handles multiple changed variables", {
  old_dom <- .dom_orders()
  new_df  <- .df_orders()
  new_df$amount[1]   <- 1
  new_df$customer[2] <- "Bobby"
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_values(old_dom, new_dom, vars = c("amount", "customer"))
  expect_equal(nrow(result), 2L)
  expect_setequal(result$var, c("amount", "customer"))
})

test_that("diff_values() returns empty when var is not present in data", {
  dom    <- .dom_orders()
  result <- diff_values(dom, dom, vars = "NONEXISTENT")
  expect_equal(nrow(result), 0L)
})

test_that("diff_values() detects NA-to-value change", {
  old_df  <- .df_orders()
  old_df$amount[1] <- NA
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_dom <- .dom_orders()

  result <- diff_values(old_dom, new_dom, vars = "amount")
  expect_equal(nrow(result), 1L)
  expect_true(is.na(result$old))
  expect_equal(as.numeric(result$new), 250)
})

test_that("diff_values() errors when old_dom is not a DomainSnapshot", {
  expect_error(diff_values(list(), .dom_orders()), regexp = "old_dom")
})

test_that("diff_values() errors when key_vars differ", {
  dom1 <- new_domain_snapshot("x", .df_orders(), key_vars = "order_id")
  dom2 <- new_domain_snapshot("x", .df_orders(), key_vars = "customer")
  expect_error(diff_values(dom1, dom2), regexp = "key_vars")
})

# ---------------------------------------------------------------------------
# diff_meta_artifact()
# ---------------------------------------------------------------------------

test_that("diff_meta_artifact() returns a 5-row data.frame with correct columns", {
  art1   <- .art_orders("build-1")
  art2   <- .art_orders("build-2")
  result <- diff_meta_artifact(art1, art2)

  expect_equal(nrow(result), 5L)
  expect_named(result, c("field", "old", "new", "changed"))
})

test_that("diff_meta_artifact() flags build_id change as changed = TRUE", {
  art1   <- .art_orders("build-1")
  art2   <- .art_orders("build-2")
  result <- diff_meta_artifact(art1, art2)

  bid_row <- result[result$field == "build_id", ]
  expect_equal(bid_row$old, "build-1")
  expect_equal(bid_row$new, "build-2")
  expect_true(bid_row$changed)
})

test_that("diff_meta_artifact() marks unchanged fields as changed = FALSE", {
  art    <- .art_orders()
  result <- diff_meta_artifact(art, art)
  expect_true(all(!result$changed))
})

test_that("diff_meta_artifact() errors when old_art is not an ArtifactSnapshot", {
  expect_error(diff_meta_artifact(list(), .art_orders()), regexp = "old_art")
})

test_that("diff_meta_artifact() errors when new_art is not an ArtifactSnapshot", {
  expect_error(diff_meta_artifact(.art_orders(), list()), regexp = "new_art")
})
