test_that("example_adsl() returns a data.frame", {
  expect_s3_class(example_adsl(), "data.frame")
})

test_that("example_adsl() has one row per unique USUBJID", {
  adsl <- example_adsl()
  expect_equal(nrow(adsl), length(unique(adsl$USUBJID)))
})

test_that("example_adsl() contains the required ADaM ADSL columns", {
  required <- c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "AGEU",
                "SEX", "RACE", "ARM", "TRT01P")
  expect_true(all(required %in% names(example_adsl())))
})

test_that("example_adsl() FASFL values are Y or N only", {
  vals <- example_adsl()$FASFL
  expect_true(all(vals %in% c("Y", "N")))
})

test_that("example_advs() returns a data.frame", {
  expect_s3_class(example_advs(), "data.frame")
})

test_that("example_advs() contains expected BDS columns", {
  required <- c("STUDYID", "USUBJID", "PARAM", "PARAMCD", "AVAL",
                "AVISIT", "TRTP", "ABLFL", "BASE", "CHG")
  expect_true(all(required %in% names(example_advs())))
})

test_that("example_advs() has more than one row", {
  expect_gt(nrow(example_advs()), 1L)
})

test_that("example_advs() PARAMCD is 8 characters or fewer and starts with a letter", {
  paramcds <- unique(example_advs()$PARAMCD)
  expect_true(all(nchar(paramcds) <= 8L))
  expect_true(all(grepl("^[A-Za-z]", paramcds)))
})

test_that("example_sdtm_lb() returns a data.frame", {
  expect_s3_class(example_sdtm_lb(), "data.frame")
})

test_that("example_sdtm_lb() contains expected SDTM LB columns", {
  required <- c("STUDYID", "DOMAIN", "USUBJID", "LBSEQ",
                "LBTESTCD", "LBTEST", "LBORRES", "LBSTRESN", "LBDTC")
  expect_true(all(required %in% names(example_sdtm_lb())))
})

test_that("example_sdtm_lb() DOMAIN is 'LB' for all rows", {
  expect_true(all(example_sdtm_lb()$DOMAIN == "LB"))
})

test_that("example_sdtm_lb() LBDTC values are ISO format dates", {
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", example_sdtm_lb()$LBDTC)))
})

test_that("example_sdtm_lb() is unique by STUDYID + USUBJID + LBSEQ", {
  lb <- example_sdtm_lb()
  keys <- lb[, c("STUDYID", "USUBJID", "LBSEQ")]
  expect_equal(anyDuplicated(keys), 0L)
})
