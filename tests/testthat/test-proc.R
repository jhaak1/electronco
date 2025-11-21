# tests/test-proc.R
library(testthat)
library(dplyr)

# Create a minimal bc_proc_concept used by proc() when code_list is NULL
bc_proc_concept <- tibble::tibble(
  code = c("77067", "76090", "CPT-123", "abc.456", "mri001", "BX-01"),
  group = c("breast cancer screening", "diagnostic mammography", "breast ultrasound",
            "needle biopsy", "mri of the breast", "needle biopsy")
)

# Source the function under test (adjust path if needed)
# source("R/proc.R")  # uncomment and set correct path when integrating into package tests

# Helper: small dataset
make_data <- function() {
  tibble::tibble(
    patient_id = c(1,1,2,2,2,3,4,5),
    cpt_code = c("77067", "76090", "CPT-123", "cpt-123", "abc-456", "MRI001", "bx-01", "UNKNOWN"),
    proc_date = as.Date(c("2020-01-01","2020-02-02","2020-03-03","2020-03-05",
                          "2020-03-10","2020-04-01","2020-05-01","2020-06-01"))
  )
}

test_that("proc errors when required arguments are missing or invalid", {
  df <- make_data()

  # non-data.frame data
  expect_error(proc(list(a=1)), regexp = "is.data.frame")

  # missing patient id column
  expect_error(proc(df %>% rename(pid = patient_id), patient_id_col = "patient_id"),
               regexp = "missing column: patient_id")

  # missing code column
  expect_error(proc(df %>% rename(code = cpt_code), code_col = "cpt_code"),
               regexp = "missing column: cpt_code")

  # missing date column
  expect_error(proc(df %>% rename(d = proc_date), date_col = "proc_date"),
               regexp = "missing column: proc_date")

  # code_list without required columns
  bad_codes <- tibble::tibble(x = 1, y = 2)
  expect_error(proc(df, code_list = bad_codes), regexp = "must contain columns: code, group")
})

test_that("proc normalizes codes (punctuation and case) and matches correctly", {
  df <- make_data()

  res <- proc(df, code_col = "cpt_code", patient_id_col = "patient_id", date_col = "proc_date",
              code_list = bc_proc_concept)

  expect_s3_class(res, "tbl_df")
  # Patients 1,2,3,4 should have matches; patient 5 has UNKNOWN so not matched
  expect_true(all(c(1,2,3,4) %in% res$patient_id))
  expect_false(5 %in% res$patient_id)

  # Check that CPT-123 and cpt-123 collapsed into same normalized code and grouped
  patient2_rows <- res %>% filter(patient_id == 2)
  expect_true(any(patient2_rows$cohort_name == "breast ultrasound") || any(patient2_rows$cohort_name == "needle biopsy"))
  # ensure sample_codes include original raw codes (one of them)
  expect_true(grepl("CPT-123|cpt-123|bx-01|abc-456", paste(res$sample_codes, collapse=";")))
})

test_that("proc respects date_format when date column is character", {
  df <- make_data()
  df2 <- df %>% mutate(proc_date_char = format(proc_date, "%d-%m-%Y")) %>%
    select(-proc_date)
  res <- proc(df2, date_col = "proc_date_char", date_format = "%d-%m-%Y", code_list = bc_proc_concept)
  expect_true(inherits(res$cohort_start, "Date"))
  # Patient 1 cohort_start should be 2020-01-01
  p1 <- res %>% filter(patient_id == 1) %>% slice(1)
  expect_equal(as.character(p1$cohort_start), "2020-01-01")
})

test_that("proc applies date_range filtering", {
  df <- make_data()
  # restrict to dates after 2020-03-04 so only some rows remain
  res <- proc(df, date_range = c("2020-03-05", "2020-06-30"), code_list = bc_proc_concept)
  # Should include patient 2 (2020-03-05), patient 3 (2020-04-01), patient 4 (2020-05-01)
  expect_true(all(c(2,3,4) %in% unique(res$patient_id)))
  # patient 1 (Jan/Feb) should be excluded
  expect_false(1 %in% unique(res$patient_id))
})

test_that("proc honors min_count parameter", {
  # make dataset where patient 2 has 3 matched procedures and patient 1 has 1
  df <- tibble::tibble(
    patient_id = c(1,2,2,2),
    cpt_code = c("77067", "CPT-123", "cpt-123", "CPT-123"),
    proc_date = as.Date(c("2020-01-01","2020-02-01","2020-02-02","2020-02-03"))
  )
  # using code_list where CPT-123 maps to breast ultrasound
  codes <- bc_proc_concept
  res_min1 <- proc(df, min_count = 1, code_list = codes)
  expect_true(1 %in% res_min1$patient_id)
  expect_true(2 %in% res_min1$patient_id)

  res_min2 <- proc(df, min_count = 2, code_list = codes)
  expect_false(1 %in% res_min2$patient_id)
  expect_true(2 %in% res_min2$patient_id)

  res_min4 <- proc(df, min_count = 4, code_list = codes)
  expect_equal(nrow(res_min4), 0)
})

test_that("proc first_only returns single earliest record per patient/cohort", {
  df <- tibble::tibble(
    patient_id = c(10,10,10,11,11),
    cpt_code = c("BX-01","abc.456","BX-01","BX-01","abc.456"),
    proc_date = as.Date(c("2020-06-01","2020-05-01","2020-07-01","2020-01-01","2020-02-01"))
  )
  res_all <- proc(df, code_list = bc_proc_concept, first_only = FALSE)
  res_first <- proc(df, code_list = bc_proc_concept, first_only = TRUE)
  # for first_only, each patient/cohort should have exactly one row
  expect_true(all(dplyr::count(res_first, patient_id, cohort_name)$n == 1))
  # cohort_start for patient 10 should be 2020-05-01 (the earliest)
  p10 <- res_first %>% filter(patient_id == 10) %>% slice(1)
  expect_equal(as.character(p10$cohort_start), "2020-05-01")
})

test_that("proc sets proc_flag only for requested proc_group(s)", {
  df <- tibble::tibble(
    patient_id = c(20,21,22),
    cpt_code = c("77067","mri001","BX-01"),
    proc_date = as.Date(c("2020-01-01","2020-02-01","2020-03-01"))
  )
  # Only request MRI group
  res <- proc(df, proc_group = c("mri of the breast"), code_list = bc_proc_concept)
  expect_true(res %>% filter(patient_id == 21) %>% pull(proc_flag) %>% all())
  # Others should be FALSE
  expect_false(res %>% filter(patient_id == 20) %>% pull(proc_flag) %>% any())
  expect_false(res %>% filter(patient_id == 22) %>% pull(proc_flag) %>% any())
})

test_that("proc accepts a custom code_list and uses its groups", {
  df <- tibble::tibble(
    patient_id = c(30,31),
    cpt_code = c("X1","X2"),
    proc_date = as.Date(c("2021-01-01","2021-01-02"))
  )
  custom_codes <- tibble::tibble(
    code = c("x1","x2"),
    group = c("custom A","custom B")
  )
  res <- proc(df, code_list = custom_codes)
  expect_true(all(c(30,31) %in% res$patient_id))
  expect_true(all(res$cohort_name %in% c("custom A","custom B")))
})
