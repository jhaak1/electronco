# tests/testthat/test-proc.R
library(testthat)
library(tibble)
library(dplyr)
library(withr)

local({
  # Export a minimal procedure concept set into global env so proc() can find it.
  bc_proc_concept <<- tibble::tibble(
    code = c("77067", "76090", "19081", "19120", "19303", "G0202", "Punct-01"),
    group = c(
      "breast cancer screening", "breast specimen radiography", "needle biopsy",
      "mastectomy procedure", "breast reconstruction", "breast cancer screening", "needle biopsy"
    )
  )

  # Safe deferred cleanup
  defer({
    if (exists("bc_proc_concept", envir = .GlobalEnv, inherits = FALSE)) {
      rm(list = "bc_proc_concept", envir = .GlobalEnv)
    }
  }, envir = parent.frame())

  test_that("errors on missing required data columns", {
    df <- tibble(id = 1:3, cpt = c("77067","76090","77067"))
    expect_error(proc(df), regexp = "`data` missing column")
    expect_error(proc(df, patient_id_col = "id", code_col = "cpt"),
                 regexp = "`data` missing column")
  })

  test_that("date parsing with default and custom format works", {
    df1 <- tibble(
      patient_id = c("p1","p2"),
      cpt_code = c("77067","76090"),
      proc_date = as.Date(c("2020-01-01","2020-02-02"))
    )
    out1 <- proc(df1)
    expect_s3_class(out1$patient_level, "data.frame")
    expect_true(all(c("patient_id","cohort_name","cohort_count","cohort_start","cohort_end") %in% names(out1$patient_level)))

    df2 <- tibble(
      patient_id = c("p1","p2"),
      cpt_code = c("77067","76090"),
      proc_date = c("01-03-2020","02-04-2020") # dd-mm-YYYY-like, but we'll parse with format
    )
    out2 <- proc(df2, date_format = "%d-%m-%Y")
    expect_equal(nrow(out2$patient_level), 2)
  })

  test_that("code normalization removes punctuation and uppercases for matching", {
    # Punctuated code 'Punct-01' in bc_proc_concept should match input 'punct.01'
    df <- tibble(
      patient_id = c("p1","p1"),
      cpt_code = c("Punct.01", "Punct-01"),
      proc_date = c("2020-05-01","2020-05-02")
    )
    out <- proc(df)
    # needle biopsy group expected for Punct-01
    expect_true(any(out$patient_level$cohort_name == "needle biopsy"))
    expect_equal(out$patient_level$cohort_count[out$patient_level$patient_id == "p1"], 2)
  })

  test_that("only matched codes produce cohorts and unmatched_sample not erroring", {
    df <- tibble(
      patient_id = c("p1","p2","p3"),
      cpt_code = c("77067","UNKNOWN","76090"),
      proc_date = c("2020-01-01","2020-02-02","2020-03-03")
    )
    out <- proc(df)
    # Two matched patients (p1 and p3)
    expect_equal(sort(out$patient_level$patient_id), sort(c("p1","p3")))
  })

  test_that("proc_group filtering via proc_flag indicates requested groups", {
    df <- tibble(
      patient_id = c("p1","p2"),
      cpt_code = c("77067","19120"),
      proc_date = c("2020-01-01","2020-02-02")
    )
    out <- proc(df, proc_group = c("breast cancer screening"))
    # cohort for p1 should be screening and flagged TRUE, p2 different group flagged FALSE
    expect_true(any(out$patient_level$proc_flag))
    expect_true(all(out$patient_level$patient_id %in% c("p1","p2")))
  })

  test_that("min_count filters cohorts with too few procedures", {
    df <- tibble(
      patient_id = c("p1","p1","p2"),
      cpt_code = c("77067","77067","76090"),
      proc_date = c("2020-01-01","2020-02-01","2020-03-01")
    )
    out1 <- proc(df, min_count = 2)
    # Only p1 should remain (has 2 matching 77067)
    expect_equal(out1$patient_level$patient_id, "p1")
    out2 <- proc(df, min_count = 3)
    expect_equal(nrow(out2$patient_level), 0)
  })

  test_that("first_only returns only the earliest cohort_start per patient/group", {
    df <- tibble(
      patient_id = c("p1","p1","p1"),
      cpt_code = c("77067","77067","76090"),
      proc_date = c("2020-01-01","2020-06-01","2020-03-01")
    )
    out <- proc(df, first_only = TRUE)
    # p1 should have two cohort rows (one per group), each with earliest start
    expect_equal(nrow(out$patient_level), 2)
    expect_true(all(out$patient_level$cohort_start <= as.Date("2020-03-01")))
  })

  test_that("date_range filters input procedures", {
    df <- tibble(
      patient_id = c("p1","p1","p2"),
      cpt_code = c("77067","77067","76090"),
      proc_date = as.Date(c("2019-01-01","2020-05-01","2020-05-02"))
    )
    out <- proc(df, date_range = c("2020-01-01","2020-12-31"))
    # only p1 (one event in 2020) and p2 remain
    expect_true(all(out$patient_level$cohort_start >= as.Date("2020-01-01")))
  })

  test_that("custom column names for patient_id, code and date are supported", {
    df <- tibble(
      id = c("p1","p2"),
      code_col = c("77067","76090"),
      when = c("2020-07-01","2020-08-01")
    )
    out <- proc(df, patient_id_col = "id", code_col = "code_col", date_col = "when")
    expect_equal(sort(out$patient_level$patient_id), sort(c("p1","p2")))
  })
})

