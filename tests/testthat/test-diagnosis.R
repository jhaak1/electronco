# tests/testthat/test-diagnosis.R
library(testthat)
library(tibble)
library(dplyr)
library(lubridate)
library(withr)

local({
  # Inject test concept set into the global environment so diagnosis() can find it
  bc_diag_concept <<- tibble::tibble(
    code = c("C50", "D05"),        # codes used in the tests
    system = c("ICD10", "ICD10"),  # normalized system strings
    include = c(TRUE, TRUE)
  )

  # Ensure removal when this file's tests finish; safe guard if missing
  defer({
    if (exists("bc_diag_concept", envir = .GlobalEnv)) {
      rm(bc_diag_concept, envir = .GlobalEnv)
    }
  }, envir = parent.frame())

  test_that("error when required columns are missing", {
    bad_df <- tibble(patient_id = 1:3, code = c("C50","C50","C50"))
    expect_error(
      diagnosis(
        bad_df, concept = "bc",
        lookback_start = "2020-01-01", lookback_end = "2020-12-31"
      ),
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
    res <- diagnosis(df, concept = "bc",
                     lookback_start = "2020-01-01", lookback_end = "2020-12-31")
    expect_true(is.list(res))
    expect_true(all(c("patient_level","evidence") %in% names(res)))
    expect_equal(nrow(res$evidence), 0)
    expect_equal(res$patient_level$diagnosis_flag, c(FALSE, FALSE))
  })

  test_that("normalization: case and trim ensure matches", {
    df <- tibble(
      patient_id = c("p1","p2"),
      code = c(" c50 ", "d05"),
      code_type = c("icd10", " ICD10 "),
      diagnosis_date = c("2020-06-01","2020-06-02")
    )
    res <- diagnosis(df, concept = "bc",
                     lookback_start = "2020-01-01", lookback_end = "2020-12-31")
    expect_equal(res$patient_level$n_total, c(1, 1))
    expect_true(all(res$patient_level$diagnosis_flag))
    expect_equal(nrow(res$evidence), 2)
  })

  test_that("matches concept codes and flags patients correctly", {
    df <- tibble(
      patient_id = c("p1","p1","p2"),
      code = c("C50","D05","C50"),
      code_type = c("ICD10","ICD10","ICD10"),
      diagnosis_date = c("2020-01-15","2020-02-01","2020-03-01")
    )
    res <- diagnosis(df, concept = "bc",
                     lookback_start = "2020-01-01", lookback_end = "2020-12-31")
    expect_equal(res$patient_level$n_total, c(2, 1))
    expect_equal(res$patient_level$diagnosis_flag, c(TRUE, TRUE))
    expect_equal(nrow(res$evidence), 3)
  })

  test_that("min_events threshold works", {
    df <- tibble(
      patient_id = c("p1","p1","p2"),
      code = c("C50","C50","C50"),
      code_type = c("ICD10","ICD10","ICD10"),
      diagnosis_date = c("2020-01-01","2020-02-01","2020-03-01")
    )
    res <- diagnosis(df, concept = "bc",
                     lookback_start = "2020-01-01", lookback_end = "2020-12-31",
                     min_events = 2)
    expect_equal(
      res$patient_level$diagnosis_flag[res$patient_level$patient_id == "p1"],
      TRUE
    )
    expect_equal(
      res$patient_level$diagnosis_flag[res$patient_level$patient_id == "p2"],
      FALSE
    )
  })

  test_that("lookback window filters out events outside range", {
    df <- tibble(
      patient_id = c("p1","p1"),
      code = c("C50","C50"),
      code_type = c("ICD10","ICD10"),
      diagnosis_date = c("2019-01-01","2020-05-01")
    )
    res <- diagnosis(df, concept = "bc",
                     lookback_start = "2020-01-01", lookback_end = "2020-12-31")
    expect_equal(nrow(res$evidence), 1)
    expect_equal(res$evidence$date, as.Date("2020-05-01"))
  })

  test_that("exclude entries do not contribute to evidence", {
    # Add an excluded code to concept set temporarily to test exclude behavior.
    bc_diag_concept_extra <- bc_diag_concept
    bc_diag_concept_extra <- bind_rows(
      bc_diag_concept_extra,
      tibble(code = "X999", system = "ICD10", include = FALSE)
    )
    # Temporarily override global concept for this scope
    bc_diag_concept <<- bc_diag_concept_extra
    defer({
      if (exists("bc_diag_concept", envir = .GlobalEnv)) {
        # restore original (first two rows)
        bc_diag_concept <<- bc_diag_concept[1:2, ]
      }
    }, envir = parent.frame())

    df <- tibble(
      patient_id = c("p1","p1"),
      code = c("C50","X999"),        # X999 is excluded
      code_type = c("ICD10","ICD10"),
      diagnosis_date = c("2020-05-01","2020-05-02")
    )
    res <- diagnosis(df, concept = "bc",
                     lookback_start = "2020-01-01", lookback_end = "2020-12-31")
    expect_equal(nrow(res$evidence), 1)
    expect_equal(res$evidence$code, "C50")
  })

  test_that("duplicates are collapsed by patient+code+system+date", {
    df <- tibble(
      patient_id = c("p1","p1"),
      code = c("C50","C50"),
      code_type = c("ICD10","ICD10"),
      diagnosis_date = c("2020-05-01","2020-05-01")
    )
    res <- diagnosis(df, concept = "bc",
                     lookback_start = "2020-01-01", lookback_end = "2020-12-31")
    expect_equal(nrow(res$evidence), 1)
  })

  test_that("custom column names and date_format work", {
    df <- tibble(
      id = c("p1","p2"),
      dx_code = c("C50","D05"),
      system_col = c("ICD10","ICD10"),
      dt = c("01-02-2020","05-02-2020")  # dd-mm-YYYY
    )
    res <- diagnosis(
      df, concept = "bc",
      lookback_start = "2020-01-01", lookback_end = "2020-12-31",
      patient_id_col = "id", code_col = "dx_code",
      system = "system_col", date_col = "dt",
      date_format = "%d-%m-%Y"
    )
    expect_equal(nrow(res$evidence), 2)
    expect_equal(res$patient_level$n_total, c(1, 1))
  })

  test_that("unrecognized concept warns and returns no matches", {
    df <- tibble(
      patient_id = "p1",
      code = "C50",
      code_type = "ICD10",
      diagnosis_date = "2020-01-01"
    )
    expect_warning(
      res <- diagnosis(df, concept = "xyz",
                       lookback_start = "2020-01-01", lookback_end = "2020-12-31"),
      "Concept not recognized"
    )
    expect_equal(nrow(res$evidence), 0)
    expect_equal(res$patient_level$diagnosis_flag, FALSE)
  })

  test_that("NA dates are handled in summaries (first/last become NA)", {
    df <- tibble(
      patient_id = c("p1","p1","p2"),
      code = c("C50","C50","D05"),
      code_type = c("ICD10","ICD10","ICD10"),
      diagnosis_date = c(NA, NA, "2020-06-01")
    )
    res <- diagnosis(df, concept = "bc",
                     lookback_start = "2020-01-01", lookback_end = "2020-12-31")
    # p1 should have 0 evidence because dates are NA and filtered; p2 should have 1
    expect_equal(res$patient_level$n_total[res$patient_level$patient_id == "p1"], 0)
    expect_equal(res$patient_level$n_total[res$patient_level$patient_id == "p2"], 1)
  })
})
