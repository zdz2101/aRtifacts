test_that("rule_r003_key_type_drift returns expected DiffFlagRule", {
  rule <- rule_r003_key_type_drift(domain = "X", key_vars = c("order_id"))
  expect_true(S7::S7_inherits(rule, DiffFlagRule))
  expect_true(startsWith(rule@id, "R003"))
  expect_true(is.function(rule@fn))
})

test_that("rule_r003_key_type_drift flags changed key types", {
  rule <- rule_r003_key_type_drift(domain = "X", key_vars = c("order_id", "customer"))
  old_df <- data.frame(
    order_id = c(101L, 102L, 103L),
    customer = c("Alice", "Bob", "Carol"),
    amount = c(250, 480, 120),
    stringsAsFactors = FALSE
  )
  old_dom <- new_domain_snapshot("orders", old_df, key_vars = "order_id")
  new_df <- old_df
  new_df$order_id <- as.character(new_df$order_id)
  new_dom <- new_domain_snapshot("orders", new_df, key_vars = "order_id")

  out <- rule@fn(old_dom, new_dom)
  expect_named(out, c("key_var", "old_type", "new_type"))
  expect_equal(nrow(out), 1L)
  expect_identical(out$key_var, "order_id")
  expect_identical(out$old_type, "integer")
  expect_identical(out$new_type, "character")
})
