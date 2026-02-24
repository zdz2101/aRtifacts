# ---------------------------------------------------------------------------
# check_adsl()
# ---------------------------------------------------------------------------

test_that("check_adsl() returns empty issue table for clean ADSL", {
  result <- check_adsl(example_adsl())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("check_adsl() has correct column schema", {
  result <- check_adsl(example_adsl())
  expect_named(result, c("check", "severity", "message", "n"))
})

test_that("check_adsl() errors when input is not a data.frame", {
  expect_error(check_adsl(list()), regexp = "data.frame")
})

test_that("check_adsl() flags missing required variables", {
  adsl   <- example_adsl()[, c("STUDYID", "USUBJID"), drop = FALSE]
  result <- check_adsl(adsl)
  issue  <- result[result$check == "ADSL_REQUIRED", ]
  expect_true(nrow(issue) >= 1L)
  expect_equal(issue$severity, "error")
  expect_true(issue$n >= 1L)
})

test_that("check_adsl() flags duplicate USUBJID", {
  adsl          <- rbind(example_adsl(), example_adsl()[1, ])
  result        <- check_adsl(adsl)
  expect_true(any(result$check == "ADSL_USUBJID_UNIQUE"))
  expect_true(any(result$severity == "error"))
})

test_that("check_adsl() warns when no population flag is present", {
  all_fl <- grep("FL$", names(example_adsl()), value = TRUE)
  adsl   <- example_adsl()[, setdiff(names(example_adsl()), all_fl), drop = FALSE]
  result <- check_adsl(adsl)
  expect_true(any(result$check == "ADSL_POPFLAG"))
  expect_true(any(result$severity == "warning"))
})

test_that("check_adsl() flags population flag values outside Y/N", {
  adsl          <- example_adsl()
  adsl$FASFL[1] <- "X"
  result        <- check_adsl(adsl)
  expect_true(any(result$check == "ADSL_POPFLAG_VALUES"))
  expect_true(any(result$severity == "error"))
})

test_that("check_adsl() flags NA population flag values", {
  adsl          <- example_adsl()
  adsl$FASFL[1] <- NA
  result        <- check_adsl(adsl)
  expect_true(any(result$check == "ADSL_POPFLAG_VALUES"))
})

# ---------------------------------------------------------------------------
# check_bds()
# ---------------------------------------------------------------------------

test_that("check_bds() returns empty issue table for clean BDS", {
  result <- check_bds(example_advs())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)
})

test_that("check_bds() has correct column schema", {
  result <- check_bds(example_advs())
  expect_named(result, c("check", "severity", "message", "n"))
})

test_that("check_bds() errors when input is not a data.frame", {
  expect_error(check_bds("not_a_df"), regexp = "data.frame")
})

test_that("check_bds() flags missing required variables", {
  bds    <- example_advs()[, c("AVAL", "TRTP"), drop = FALSE]
  result <- check_bds(bds)
  expect_true(any(result$check == "BDS_REQUIRED"))
  expect_true(any(result$severity == "error"))
})

test_that("check_bds() flags absence of AVAL and AVALC", {
  bds    <- example_advs()[, setdiff(names(example_advs()), "AVAL"), drop = FALSE]
  result <- check_bds(bds)
  expect_true(any(result$check == "BDS_AVAL_OR_AVALC"))
})

test_that("check_bds() flags absence of treatment variable", {
  bds    <- example_advs()[, setdiff(names(example_advs()), "TRTP"), drop = FALSE]
  result <- check_bds(bds)
  expect_true(any(result$check == "BDS_TREATMENT"))
})

test_that("check_bds() flags non-1:1 PARAM/PARAMCD mapping", {
  bds           <- example_advs()
  bds$PARAM[1]  <- "Something Else"  # SYSBP PARAMCD now maps to two PARAMs
  result        <- check_bds(bds)
  expect_true(any(result$check == "BDS_PARAM_MAP"))
})

test_that("check_bds() flags PARAMCD format violations", {
  bds            <- example_advs()
  bds$PARAMCD[1] <- "123BADCD"   # starts with digit
  result         <- check_bds(bds)
  expect_true(any(result$check == "BDS_PARAMCD_FORMAT"))
})

test_that("check_bds() flags absence of ABLFL when BASE is populated", {
  bds      <- example_advs()[, setdiff(names(example_advs()), "ABLFL"), drop = FALSE]
  result   <- check_bds(bds)
  expect_true(any(result$check == "BDS_BASE_ABLFL"))
})

# ---------------------------------------------------------------------------
# validate_adam()
# ---------------------------------------------------------------------------

test_that("validate_adam() returns a list with $adsl and $bds", {
  out <- validate_adam(adsl = example_adsl(), bds = list(ADVS = example_advs()))
  expect_named(out, c("adsl", "bds"))
})

test_that("validate_adam() $adsl is a data.frame issue table", {
  out <- validate_adam(adsl = example_adsl(), bds = list(ADVS = example_advs()))
  expect_s3_class(out$adsl, "data.frame")
  expect_named(out$adsl, c("check", "severity", "message", "n"))
})

test_that("validate_adam() $bds is a named list with one entry per dataset", {
  out <- validate_adam(adsl = example_adsl(), bds = list(ADVS = example_advs()))
  expect_type(out$bds, "list")
  expect_named(out$bds, "ADVS")
  expect_s3_class(out$bds$ADVS, "data.frame")
})

test_that("validate_adam() accepts a single BDS data.frame (not a list)", {
  out <- validate_adam(bds = example_advs())
  expect_type(out$bds, "list")
  expect_length(out$bds, 1L)
})

test_that("validate_adam() works when adsl is NULL", {
  out <- validate_adam(bds = list(ADVS = example_advs()))
  expect_null(out$adsl)
  expect_named(out$bds, "ADVS")
})

test_that("validate_adam() returns empty $bds list when none supplied", {
  out <- validate_adam(adsl = example_adsl())
  expect_length(out$bds, 0L)
})
