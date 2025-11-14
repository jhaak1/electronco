# tests/testthat/test-diagnosis.R
library(testthat)
library(tibble)
library(dplyr)
library(lubridate)

# Preserve original bc_diag_concept if present, then inject our test set globally.
old_bc <- if (exists("bc_diag_concept", inherits = TRUE)) {
  get("bc_diag_concept", inherits = TRUE)
} else {
  NULL
}

bc_diag_concept <<- tibble::tibble(
  code = c("C50", "D05"),        # match the test data codes
  system = c("ICD10", "ICD10"),  # match normalized system
  include = c(TRUE, TRUE)
)

# Restore after all tests in this file
testthat::teardown({
  if (!is.null(old_bc)) {
    bc_diag_concept <<- old_bc
  } else {
    rm(bc_diag_concept, inherits = TRUE)
  }
})

test_that("error when required columns are missing", {
  bad_df <- tibble(patient_id = 1:3, code = c("C50","C50","C50"))
  expect_error(
    diagnosis(
      bad_df, concept = "bc",
      lookback_start = "2020-01-01", lookback_end = "2020-12-31"
    ),
    # Function message doesn't end with a period; accept substring to avoid brittleness
    regexp = "diagnoses is missing columns"
  )
})

test_that("returns empty patient_level and evidence when no matches", {
  df <- tibble(
    patient_id = c("p1","p2"),
    code = c("X99","Y88"),
    code_type = c("ICD10","ICD10"),
    diagnosis_date = c("2020-05-01","2020-06-01")
  )
  res <- diagnosis(df, concept = "bc", lookback_start = "2020-01-01", lookback_end = "2020-12-31")
  expect_true(is.list(res))
  expect_true(all(c("patient_level","evidence") %in% names(res)))
  expect_equal(nrow(res$evidence), 0)
  expect_equal(res$patient_level$diagnosis_flag, c(FALSE,FALSE))
})

test_that("matches concept codes and flags patients correctly", {
  df <- tibble(
    patient_id = c("p1","p1","p2"),
    code = c("C50","D05","C50"),
    code_type = c("ICD10","ICD10","ICD10"),
    diagnosis_date = c("2020-01-15","2020-02-01","2020-03-01")
  )
  res <- diagnosis(df, concept = "bc", lookback_start = "2020-01-01", lookback_end = "2020-12-31")
  expect_equal(res$patient_level$n_total, c(2,1))
  expect_equal(res$patient_level$diagnosis_flag, c(TRUE,TRUE))
  expect_equal(nrow(res$evidence), 3)
})

test_that("min_events threshold works", {
  df <- tibble(
    patient_id = c("p1","p1","p2"),
    code = c("C50","C50","C50"),
    code_type = c("ICD10","ICD10","ICD10"),
    diagnosis_date = c("2020-01-01","2020-02-01","2020-03-01")
  )
  res <- diagnosis(df, concept = "bc", lookback_start = "2020-01-01", lookback_end = "2020-12-31", min_events = 2)
  expect_equal(res$patient_level$diagnosis_flag[res$patient_level$patient_id=="p1"], TRUE)
  expect_equal(res$patient_level$diagnosis_flag[res$patient_level$patient_id=="p2"], FALSE)
})

test_that("lookback window filters out events outside range", {
  df <- tibble(
    patient_id = c("p1","p1"),
    code = c("C50","C50"),
    code_type = c("ICD10","ICD10"),
    diagnosis_date = c("2019-01-01","2020-05-01")
  )
  res <- diagnosis(df, concept = "bc", lookback_start = "2020-01-01", lookback_end = "2020-12-31")
  expect_equal(nrow(res$evidence), 1)
  expect_equal(res$evidence$date, as.Date("2020-05-01"))
})

test_that("non-recognized concept warns and returns no matches", {
  df <- tibble(
    patient_id = "p1",
    code = "C50",
    code_type = "ICD10",
    diagnosis_date = "2020-01-01"
  )
  expect_warning(
    res <- diagnosis(df, concept = "xyz", lookback_start = "2020-01-01", lookback_end = "2020-12-31"),
    "Concept not recognized"
  )
  expect_equal(nrow(res$evidence), 0)
  expect_equal(res$patient_level$diagnosis_flag, FALSE)
})
