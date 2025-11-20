# tests/testthat/test-labs.R
library(testthat)
library(dplyr)

# sample data ---------------------------------------------------------------
sample_labs <- tibble::tibble(
  patient_id = c("a", "a", "b", "b", "c"),
  lab_date = c("2025-01-01", "2025-01-10", "2025-02-01", "2025-02-15", "2025-03-01"),
  lab_name = c("CEA", "CEA level", "CA15-3", "ca15-3", "Hemoglobin"),
  stringsAsFactors = FALSE
)

test_that("validates inputs: markers required and data must be a data.frame", {
  expect_error(labs(sample_labs, markers = character(0)), "`markers` must be a character vector")
  expect_error(labs("not a df", markers = "CEA"), "`data` must be a data.frame or tibble")
})

test_that("validates inputs: missing required columns detected", {
  df_missing <- sample_labs %>% select(-lab_name)
  expect_error(labs(df_missing, markers = "CEA"), "Missing required columns in `data`")
})

test_that("coercing invalid lab_date to Date errors", {
  df_bad_dates <- sample_labs
  df_bad_dates$lab_date <- "not-a-date"
  expect_error(labs(df_bad_dates, markers = "CEA", cohort_type = "all"))
})

test_that("match_type = exact does strict case-insensitive equality", {
  # "CEA" should match only exact "CEA" (case-insensitive) not "CEA level"
  res_exact <- labs(sample_labs, markers = "CEA", match_type = "exact", cohort_type = "all")
  expect_true(all(tolower(res_exact$lab_name) == "cea"))
  expect_equal(nrow(res_exact), 1)
  expect_equal(res_exact$patient_id, "a")
})

test_that("match_type = contains does substring matching case-insensitively", {
  res_contains <- labs(sample_labs, markers = "cea", match_type = "contains", cohort_type = "all")
  # should match "CEA" and "CEA level" (both patient a rows)
  expect_true(all(grepl("cea", tolower(res_contains$lab_name))))
  expect_equal(nrow(res_contains), 2)
  expect_setequal(res_contains$patient_id, c("a"))
})

test_that("cohort_type = any returns per-patient summary with first/last/n_tests", {
  res_any <- labs(sample_labs, markers = c("CEA", "CA15-3"), match_type = "contains", cohort_type = "any")
  expect_true(all(c("patient_id", "first_lab_date", "last_lab_date", "n_tests") %in% names(res_any)))
  # patients a and b have matching markers
  expect_setequal(res_any$patient_id, c("a", "b"))
  # check counts: patient a has 2 CEA matches (CEA, CEA level) -> n_tests 2
  expect_equal(res_any %>% filter(patient_id == "a") %>% pull(n_tests), 2)
})

test_that("cohort_type = first returns earliest matched event with n_tests", {
  res_first <- labs(sample_labs, markers = c("CEA", "CA15-3"), match_type = "contains", cohort_type = "first")
  expect_true(all(c("patient_id", "lab_date", "lab_name", "n_tests") %in% names(res_first)))
  # earliest for patient b (CA15-3 on 2025-02-01)
  expect_equal(as.character(res_first %>% filter(patient_id == "b") %>% pull(lab_date)), "2025-02-01")
})

test_that("cohort_type = last returns most recent matched event with n_tests", {
  res_last <- labs(sample_labs, markers = c("CEA", "CA15-3"), match_type = "contains", cohort_type = "last")
  expect_true(all(c("patient_id", "lab_date", "lab_name", "n_tests") %in% names(res_last)))
  # last for patient a is 2025-01-10
  expect_equal(as.character(res_last %>% filter(patient_id == "a") %>% pull(lab_date)), "2025-01-10")
})

test_that("cohort_type = all returns every matched event; preserves order when min_tests = 1", {
  res_all <- labs(sample_labs, markers = "ca15-3", match_type = "contains", cohort_type = "all")
  expect_true(all(c("patient_id", "lab_date", "lab_name") %in% names(res_all)))
  expect_equal(nrow(res_all), 2) # two rows for CA15-3 variants
  expect_true(all(res_all$lab_name %in% c("CA15-3", "ca15-3")))
})

test_that("min_tests filters patients with fewer than required occurrences", {
  # patient a has 2 CEA matches; require min_tests = 2 should keep a, min_tests = 3 removes
  res_min2_any <- labs(sample_labs, markers = "CEA", match_type = "contains", cohort_type = "any", min_tests = 2)
  expect_equal(nrow(res_min2_any), 1)
  expect_equal(res_min2_any$patient_id, "a")
  res_min3_any <- labs(sample_labs, markers = "CEA", match_type = "contains", cohort_type = "any", min_tests = 3)
  expect_equal(nrow(res_min3_any), 0)
  # for cohort_type = all, min_tests should drop rows for patients with insufficient counts
  res_all_min2 <- labs(sample_labs, markers = "CEA", match_type = "contains", cohort_type = "all", min_tests = 2)
  expect_true(all(res_all_min2$patient_id == "a"))
  expect_equal(nrow(res_all_min2), 2)
})

test_that("date_range filters results and returns appropriate empty tibble shapes", {
  # date window that excludes all matches
  out_any <- labs(sample_labs, markers = "CEA", match_type = "contains", cohort_type = "any",
                  date_range = c("2024-01-01", "2024-12-31"))
  expect_equal(nrow(out_any), 0)
  expect_true(all(c("patient_id", "first_lab_date", "last_lab_date", "n_tests") %in% names(out_any)))

  out_first <- labs(sample_labs, markers = "CEA", match_type = "contains", cohort_type = "first",
                    date_range = c("2024-01-01", "2024-12-31"))
  expect_equal(nrow(out_first), 0)
  expect_true(all(c("patient_id", "lab_date", "lab_name", "n_tests") %in% names(out_first)))
})

test_that("date_range argument must be length 2 and coercible to Date", {
  expect_error(labs(sample_labs, markers = "CEA", date_range = "2025-01-01"), "`date_range` must be length 2")
  expect_error(labs(sample_labs, markers = "CEA", date_range = c("bad", "2025-01-31")))
})

# end of tests
