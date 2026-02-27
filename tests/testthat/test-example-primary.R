test_that("example_primary returns expected structure and values", {
  out <- example_primary()

  expect_s3_class(out, "data.frame")
  expect_named(out, c("entity_id", "record_id", "metric_code", "value", "group_id", "event_date"))
  expect_true(nrow(out) >= 1L)
  expect_true(all(!is.na(out$entity_id)))
  expect_true(all(out$record_id > 0))
})
