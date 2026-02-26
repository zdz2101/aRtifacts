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
  new_df <- rbind(orders_df(), data.frame(order_id = 999L, customer = "Dave", amount = 10, stringsAsFactors = FALSE))
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_keys(old_dom, new_dom)
  expect_equal(nrow(result$added), 1L)
  expect_equal(result$added$order_id, 999L)
  expect_equal(nrow(result$removed), 0L)
})

test_that("diff_keys() detects a removed key", {
  old_dom <- orders_dom()
  new_df <- orders_df()[-1, ] # remove order 101
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_keys(old_dom, new_dom)
  expect_equal(nrow(result$removed), 1L)
  expect_equal(result$removed$order_id, 101L)
  expect_equal(nrow(result$added), 0L)
})

test_that("diff_keys() handles simultaneous additions and removals", {
  old_dom <- orders_dom()
  new_df <- rbind(
    orders_df()[-1, ], # remove 101
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
# compare_flag_states()
# ---------------------------------------------------------------------------

test_that("compare_flag_states() classifies introduced/resolved/persisting", {
  old_flags <- data.frame(
    entity_id = c("01", "02"),
    DOMAIN = c("PRIMARY", "PRIMARY"),
    RULE_ID = c("TMP-900", "TMP-900"),
    stringsAsFactors = FALSE
  )
  new_flags <- data.frame(
    entity_id = c("02", "03"),
    DOMAIN = c("PRIMARY", "PRIMARY"),
    RULE_ID = c("TMP-900", "TMP-900"),
    stringsAsFactors = FALSE
  )

  out <- compare_flag_states(old_flags, new_flags)
  expect_named(out, c("entity_id", "DOMAIN", "RULE_ID", "status"))
  expect_equal(nrow(out), 3L)
  expect_setequal(out$status, c("introduced", "resolved", "persisting"))
})

test_that("compare_flag_states() handles missing key columns", {
  old_flags <- data.frame(entity_id = "01", RULE_ID = "TMP-900", stringsAsFactors = FALSE)
  new_flags <- data.frame(entity_id = "01", DOMAIN = "PRIMARY", stringsAsFactors = FALSE)

  out <- compare_flag_states(old_flags, new_flags, by = c("entity_id", "DOMAIN", "RULE_ID"))
  expect_named(out, c("entity_id", "DOMAIN", "RULE_ID", "status"))
  expect_true(nrow(out) >= 1L)
})

test_that("compare_flag_states() returns empty output for empty inputs", {
  old_flags <- data.frame()
  new_flags <- data.frame()

  out <- compare_flag_states(old_flags, new_flags, by = c("entity_id", "DOMAIN", "RULE_ID"))
  expect_named(out, c("entity_id", "DOMAIN", "RULE_ID", "status"))
  expect_equal(nrow(out), 0L)
})

test_that("compare_flag_states() validates inputs", {
  expect_error(compare_flag_states(list(), data.frame()), regexp = "old_flags")
  expect_error(compare_flag_states(data.frame(), list()), regexp = "new_flags")
  expect_error(compare_flag_states(data.frame(), data.frame(), by = character(0)), regexp = "by")
})

# ---------------------------------------------------------------------------
# diff_values()
# ---------------------------------------------------------------------------

test_that("diff_values() returns empty data.frame when data is identical", {
  dom <- orders_dom()
  result <- diff_values(dom, dom, vars = "amount")
  expect_equal(nrow(result), 0L)
})

test_that("diff_values() detects a changed numeric value", {
  old_dom <- orders_dom()
  new_df <- orders_df()
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
  new_df <- orders_df()
  new_df$customer[new_df$order_id == 101L] <- "Alicia"
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_values(old_dom, new_dom, vars = "customer")
  expect_equal(nrow(result), 1L)
  expect_equal(result$old, "Alice")
  expect_equal(result$new, "Alicia")
})

test_that("diff_values() handles multiple changed variables", {
  old_dom <- orders_dom()
  new_df <- orders_df()
  new_df$amount[1] <- 1
  new_df$customer[2] <- "Bobby"
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  result <- diff_values(old_dom, new_dom, vars = c("amount", "customer"))
  expect_equal(nrow(result), 2L)
  expect_setequal(result$var, c("amount", "customer"))
})

test_that("diff_values() returns empty when var is not present in data", {
  dom <- orders_dom()
  result <- diff_values(dom, dom, vars = "NONEXISTENT")
  expect_equal(nrow(result), 0L)
})

test_that("diff_values() detects NA-to-value change", {
  old_df <- orders_df()
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
  art1 <- orders_art("build-1")
  art2 <- orders_art("build-2")
  result <- diff_meta_artifact(art1, art2)

  expect_equal(nrow(result), 5L)
  expect_named(result, c("field", "old", "new", "changed"))
})

test_that("diff_meta_artifact() flags build_id change as changed = TRUE", {
  art1 <- orders_art("build-1")
  art2 <- orders_art("build-2")
  result <- diff_meta_artifact(art1, art2)

  bid_row <- result[result$field == "build_id", ]
  expect_equal(bid_row$old, "build-1")
  expect_equal(bid_row$new, "build-2")
  expect_true(bid_row$changed)
})

test_that("diff_meta_artifact() marks unchanged fields as changed = FALSE", {
  art <- orders_art()
  result <- diff_meta_artifact(art, art)
  expect_true(all(!result$changed))
})

test_that("diff_meta_artifact() errors when old_art is not an ArtifactSnapshot", {
  expect_error(diff_meta_artifact(list(), orders_art()), regexp = "old_art")
})

test_that("diff_meta_artifact() errors when new_art is not an ArtifactSnapshot", {
  expect_error(diff_meta_artifact(orders_art(), list()), regexp = "new_art")
})

# ---------------------------------------------------------------------------
# compare_key_churn()
# ---------------------------------------------------------------------------

test_that("compare_key_churn() is stable for unchanged snapshots", {
  old_dom <- orders_dom()
  new_dom <- orders_dom()

  out <- compare_key_churn(old_dom, new_dom)
  expect_named(
    out,
    c(
      "domain", "old_n_keys", "new_n_keys", "added_n", "removed_n",
      "churn_n", "added_rate", "removed_rate", "churn_rate",
      "net_change_n", "net_change_rate"
    )
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$old_n_keys, 3L)
  expect_equal(out$new_n_keys, 3L)
  expect_equal(out$added_n, 0L)
  expect_equal(out$removed_n, 0L)
  expect_equal(out$churn_n, 0L)
  expect_equal(out$churn_rate, 0)
  expect_equal(out$net_change_n, 0L)
  expect_equal(out$net_change_rate, 0)
})

test_that("compare_key_churn() captures add/remove churn and rates", {
  old_dom <- orders_dom()
  new_df <- rbind(
    orders_df()[-1, , drop = FALSE],
    data.frame(order_id = 999L, customer = "Dave", amount = 10, stringsAsFactors = FALSE)
  )
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- compare_key_churn(old_dom, new_dom)
  expect_equal(out$old_n_keys, 3L)
  expect_equal(out$new_n_keys, 3L)
  expect_equal(out$added_n, 1L)
  expect_equal(out$removed_n, 1L)
  expect_equal(out$churn_n, 2L)
  expect_equal(out$added_rate, 1 / 3)
  expect_equal(out$removed_rate, 1 / 3)
  expect_equal(out$churn_rate, 2 / 3)
  expect_equal(out$net_change_n, 0L)
})

test_that("compare_key_churn() handles empty-old to non-empty-new cuts", {
  old_df <- data.frame(order_id = integer(), customer = character(), amount = numeric(), stringsAsFactors = FALSE)
  new_df <- data.frame(order_id = c(1L, 2L), customer = c("A", "B"), amount = c(10, 20), stringsAsFactors = FALSE)
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- compare_key_churn(old_dom, new_dom)
  expect_equal(out$old_n_keys, 0L)
  expect_equal(out$new_n_keys, 2L)
  expect_equal(out$added_n, 2L)
  expect_equal(out$removed_n, 0L)
  expect_equal(out$churn_n, 2L)
  expect_true(is.na(out$added_rate))
  expect_true(is.na(out$churn_rate))
})

test_that("compare_key_churn() supports threshold pattern for spikes", {
  old_df <- data.frame(order_id = 1:10, customer = rep("X", 10), amount = rep(1, 10), stringsAsFactors = FALSE)
  new_df <- data.frame(order_id = 6:15, customer = rep("X", 10), amount = rep(1, 10), stringsAsFactors = FALSE)
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- compare_key_churn(old_dom, new_dom)
  expect_true(out$churn_rate > 0.5)
})

test_that("compare_key_churn() errors when key_vars differ", {
  dom1 <- new_domain_snapshot("x", orders_df(), key_vars = "order_id")
  dom2 <- new_domain_snapshot("x", orders_df(), key_vars = "customer")
  expect_error(compare_key_churn(dom1, dom2), regexp = "key_vars")
})

# ---------------------------------------------------------------------------
# compare_record_volume()
# ---------------------------------------------------------------------------

test_that("compare_record_volume() is stable for unchanged cuts", {
  old_dom <- orders_dom()
  new_dom <- orders_dom()

  out <- compare_record_volume(old_dom, new_dom)
  expect_named(
    out,
    c("domain", "old_n_records", "new_n_records", "absolute_change", "relative_change", "direction")
  )
  expect_equal(nrow(out), 1L)
  expect_equal(out$old_n_records, 3L)
  expect_equal(out$new_n_records, 3L)
  expect_equal(out$absolute_change, 0L)
  expect_equal(out$relative_change, 0)
  expect_identical(out$direction, "unchanged")
})

test_that("compare_record_volume() captures increases", {
  old_dom <- orders_dom()
  new_df <- rbind(
    orders_df(),
    data.frame(order_id = 999L, customer = "Dave", amount = 10, stringsAsFactors = FALSE)
  )
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- compare_record_volume(old_dom, new_dom)
  expect_equal(out$old_n_records, 3L)
  expect_equal(out$new_n_records, 4L)
  expect_equal(out$absolute_change, 1L)
  expect_equal(out$relative_change, 1 / 3)
  expect_identical(out$direction, "increase")
})

test_that("compare_record_volume() captures drops", {
  old_dom <- orders_dom()
  new_df <- orders_df()[1:2, , drop = FALSE]
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- compare_record_volume(old_dom, new_dom)
  expect_equal(out$old_n_records, 3L)
  expect_equal(out$new_n_records, 2L)
  expect_equal(out$absolute_change, -1L)
  expect_equal(out$relative_change, -1 / 3)
  expect_identical(out$direction, "decrease")
})

test_that("compare_record_volume() handles empty-old to non-empty-new", {
  old_df <- data.frame(order_id = integer(), customer = character(), amount = numeric(), stringsAsFactors = FALSE)
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_dom <- orders_dom()

  out <- compare_record_volume(old_dom, new_dom)
  expect_equal(out$old_n_records, 0L)
  expect_equal(out$new_n_records, 3L)
  expect_equal(out$absolute_change, 3L)
  expect_true(is.na(out$relative_change))
  expect_identical(out$direction, "increase")
})

test_that("compare_record_volume() supports threshold pattern for plateaus/spikes", {
  old_dom <- orders_dom()
  new_df <- rbind(
    orders_df(),
    data.frame(order_id = c(201L, 202L, 203L), customer = c("D", "E", "F"), amount = c(1, 1, 1), stringsAsFactors = FALSE)
  )
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- compare_record_volume(old_dom, new_dom)
  expect_true(out$relative_change > 0.5)
})

# ---------------------------------------------------------------------------
# compare_distribution_shift()
# ---------------------------------------------------------------------------

dist_df <- function(shift_a = 0, shift_b = 0) {
  data.frame(
    id = 1:40,
    SITEID = rep(c("A", "B"), each = 20),
    value = c((1:20) + shift_a, (1:20) + shift_b),
    stringsAsFactors = FALSE
  )
}

dist_dom <- function(df) {
  new_domain_snapshot("dist", df, key_vars = "id")
}

test_that("compare_distribution_shift() reports no shift for identical cuts", {
  old_dom <- dist_dom(dist_df())
  new_dom <- dist_dom(dist_df())

  out <- compare_distribution_shift(old_dom, new_dom, var = "value", by = "SITEID")
  expect_named(
    out,
    c(
      "domain", "var", "by", "group", "n_old", "n_new",
      "mean_old", "mean_new", "mean_delta", "median_old", "median_new", "median_delta",
      "q10_old", "q10_new", "q10_delta", "q25_old", "q25_new", "q25_delta",
      "q75_old", "q75_new", "q75_delta", "q90_old", "q90_new", "q90_delta",
      "ks_stat", "ks_p_value"
    )
  )
  expect_equal(nrow(out), 2L)
  expect_true(all(out$mean_delta == 0))
  expect_true(all(out$median_delta == 0))
  expect_true(all(out$ks_stat == 0))
})

test_that("compare_distribution_shift() captures moderate shift in one group", {
  old_dom <- dist_dom(dist_df())
  new_dom <- dist_dom(dist_df(shift_a = 2, shift_b = 0))

  out <- compare_distribution_shift(old_dom, new_dom, var = "value", by = "SITEID")
  row_a <- out[out$group == "A", , drop = FALSE]
  row_b <- out[out$group == "B", , drop = FALSE]

  expect_equal(row_a$mean_delta, 2)
  expect_equal(row_a$median_delta, 2)
  expect_true(row_a$ks_stat > 0)
  expect_equal(row_b$mean_delta, 0)
  expect_equal(row_b$ks_stat, 0)
})

test_that("compare_distribution_shift() reports stronger drift for large shift", {
  old_dom <- dist_dom(dist_df())
  moderate_dom <- dist_dom(dist_df(shift_a = 2, shift_b = 0))
  large_dom <- dist_dom(dist_df(shift_a = 20, shift_b = 0))

  moderate <- compare_distribution_shift(old_dom, moderate_dom, var = "value", by = "SITEID")
  large <- compare_distribution_shift(old_dom, large_dom, var = "value", by = "SITEID")

  mod_a <- moderate[moderate$group == "A", , drop = FALSE]
  large_a <- large[large$group == "A", , drop = FALSE]

  expect_true(abs(large_a$mean_delta) > abs(mod_a$mean_delta))
  expect_true(large_a$ks_stat >= mod_a$ks_stat)
})
