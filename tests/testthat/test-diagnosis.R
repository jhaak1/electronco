# tests/testthat/test-diagnosis.R
library(testthat)
library(tibble)
library(dplyr)
library(lubridate)

# Define a minimal concept set for breast cancer (bc).
bc_diag_concept <- tibble::tibble(
  code = c("C50", "D05"),
  system = c("ICD10", "ICD10"),
  include = c(TRUE, TRUE)
)

test_that("error when required columns are missing", {
  bad_df <- tibble(patient_id = 1:3, code = c("C50","C50","C50"))
  expect_error(
    diagnosis(bad_df, concept = "bc", lookback_start = "2020-01-01", lookback_end = "2020-12-31"),
    "diagnoses is missing columns."
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
  # Patient-level Flags
  expect_equal(res$patient_level$n_total, c(2,1))
  expect_equal(res$patient_level$diagnosis_flag, c(TRUE,TRUE))
  # Evidence Rows
  expect_equal(nrow(res$evidence), 3)
  expect_true(all(res$evidence$is_canonical %in% c(TRUE,FALSE)))
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

test_that("non-recognized concept prints warning and returns no matches", {
  df <- tibble(
    patient_id = "p1",
    code = "C50",
    code_type = "ICD10",
    diagnosis_date = "2020-01-01"
  )
  res <- diagnosis(df, concept = "xyz", lookback_start = "2020-01-01", lookback_end = "2020-12-31")
  expect_equal(nrow(res$evidence), 0)
  expect_equal(res$patient_level$diagnosis_flag, FALSE)
})
