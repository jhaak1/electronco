# tests/testthat/test-meds.R
library(testthat)
library(dplyr)
library(lubridate)

# helper sample data
sample_med_df <- tibble::tibble(
  patient_id = c("p1", "p1", "p2", "p3", "p4", "p5"),
  order_date = c("2025-01-01", "2025-02-01", "2025-01-15", "2025-03-01", "2025-01-10", "2025-01-05"),
  med_name = c("  Paclitaxel", "paclitaxel", "Trastuzumab", "5-fluorouracil", "Anastrazole ", "CAPECITABINE"),
  route = c("IV", "iv", "Subcutaneous", "IV", "PO", "po"),
  dose = c("50 mg", "50 mg", "100 mg", "500 mg", "10 mg", "500 mg"),
  stringsAsFactors = FALSE
)

test_that("validates inputs: non-data.frame", {
  expect_error(meds("not a df", drugs = "paclitaxel"),
               "`data` must be a data.frame or tibble")
})

test_that("validates inputs: missing required columns", {
  df <- sample_med_df %>% select(-med_name)
  expect_error(meds(df, drugs = "paclitaxel"),
               "Missing required columns")
})

test_that("validates inputs: route_filter provided but route column missing", {
  df <- sample_med_df %>% select(-route)
  expect_error(meds(df, drugs = "paclitaxel", route_filter = "IV"),
               "`route` column not found in data, but route_filter provided")
})

test_that("validates inputs: dose column argument must exist if provided", {
  df <- sample_med_df %>% select(-dose)
  expect_error(meds(df, drugs = "paclitaxel", dose = "dose"),
               "`dose` column not found in data but dose argument provided")
})

test_that("validates inputs: date_range must be length-2", {
  expect_error(meds(sample_med_df, drugs = "paclitaxel", date_range = c("2025-01-01")),
               "`date_range` must be a length-2 vector")
})

test_that("validates inputs: drugs must be provided and must be allowed", {
  expect_error(meds(sample_med_df, drugs = character(0)),
               "`drugs` must be a non-empty character vector")
  expect_error(meds(sample_med_df, drugs = c("not-a-drug")),
               "Some requested drugs are not in the allowed list")
})

test_that("basic matching is case-insensitive and trims whitespace", {
  res <- meds(sample_med_df, drugs = c("paclitaxel"))
  expect_true(all(res$drug_matched == "paclitaxel"))
  expect_equal(nrow(res), 2)
  expect_setequal(res$patient_id, c("p1"))
})

test_that("matches multiple drugs and normalizes names", {
  res <- meds(sample_med_df, drugs = c("trastuzumab", "5-fluorouracil", "anastrazole", "capecitabine"))
  expect_equal(nrow(res), 4)
  expect_setequal(res$drug_matched, c("trastuzumab", "5-fluorouracil", "anastrazole", "capecitabine"))
})

test_that("route_filter filters rows case-insensitively even when route not returned", {
  # The function filters by route internally but the slim output does not include route unless inc_original_cols = TRUE.
  res_iv <- meds(sample_med_df, drugs = c("paclitaxel", "5-fluorouracil"), route_filter = c("iv"))
  # paclitaxel has two IV rows, 5-fluorouracil has IV -> total 3
  expect_equal(nrow(res_iv), 3)
  # verify that drug_matched values are a subset of requested drugs
  expect_true(all(res_iv$drug_matched %in% c("paclitaxel", "5-fluorouracil")))
})

test_that("date_range filters correctly and rejects unparseable dates", {
  res_jan <- meds(sample_med_df, drugs = c("capecitabine", "5-fluorouracil", "paclitaxel"),
                  date_range = c("2025-01-01", "2025-01-31"))
  # capecitabine (p5 on 2025-01-05) and paclitaxel (p1 on 2025-01-01) should remain
  expect_equal(nrow(res_jan), 2)
  expect_setequal(res_jan$patient_id, c("p1", "p5"))
  # invalid date string in date_range should produce the intended error; suppress warnings from lubridate during test
  expect_error(suppressWarnings(meds(sample_med_df, drugs = "paclitaxel", date_range = c("bad", "2025-01-31"))),
               "date_range values must be parseable as dates")
})

test_that("first_only keeps earliest exposure per patient x drug", {
  res_first <- meds(sample_med_df, drugs = c("paclitaxel"), first_only = TRUE)
  expect_equal(nrow(res_first), 1)
  expect_equal(res_first$order_date, "2025-01-01")
})

test_that("inc_original_cols toggles output columns", {
  res_slim <- meds(sample_med_df, drugs = c("capecitabine"))
  expect_true(all(c("patient_id", "order_date", "drug_matched", "cohort_flag") %in% names(res_slim)))
  expect_false("route" %in% names(res_slim))
  res_full <- meds(sample_med_df, drugs = c("capecitabine"), inc_original_cols = TRUE)
  expect_true(all(c("route", "dose", "patient_id", "order_date", "med_name", "drug_matched", "cohort_flag") %in% names(res_full)))
})

test_that("preserves POSIX and Date order_date inputs", {
  df2 <- sample_med_df
  df2$order_date <- as.Date(df2$order_date)
  res_date <- meds(df2, drugs = c("capecitabine"))
  expect_equal(nrow(res_date), 1)
  df3 <- sample_med_df
  df3$order_date <- as.POSIXct(df3$order_date, tz = "UTC")
  res_posix <- meds(df3, drugs = c("capecitabine"))
  expect_equal(nrow(res_posix), 1)
})
