library(testthat)
library(dplyr)
library(tibble)
library(lubridate)

# sample dataset
sample_data <- tibble::tibble(
  patient_id = c("p1", "p1", "p2", "p2", "p3"),
  order_date = c("2020-01-01", "2020-02-01", "2020-01-15", "2021-03-01", "2020-05-05"),
  med_name = c("Aspirin", "aspirin", "5-Fluorouracil", "Trastuzumab", "ASPIRIN"),
  route = c("Oral", "oral", "IV", "iv", NA_character_),
  dose = c("100 mg", "100 mg", "500 mg", "600 mg", "50 mg")
)

test_that("accepts any drug and matches case-insensitively", {
  out1 <- meds(sample_data, drugs = "aspirin",
               patient_id = "patient_id", order_date = "order_date", med_name = "med_name")
  expect_s3_class(out1, "tbl_df")
  expect_equal(nrow(out1), 3)
  expect_true(all(tolower(out1$drug_matched) == "aspirin"))

  out2 <- meds(sample_data, drugs = "ASPIRIN",
               patient_id = "patient_id", order_date = "order_date", med_name = "med_name")
  expect_equal(nrow(out2), 3)
  expect_true(all(tolower(out2$drug_matched) == "aspirin"))

  out3 <- meds(sample_data, drugs = "ibuprofen",
               patient_id = "patient_id", order_date = "order_date", med_name = "med_name")
  expect_equal(nrow(out3), 0)
})

test_that("multiple drugs input works and drug_matched reflects normalized med_name", {
  out <- meds(sample_data, drugs = c("aspirin", "Trastuzumab"),
              patient_id = "patient_id", order_date = "order_date", med_name = "med_name")
  expect_equal(nrow(out), 4)
  expect_true(all(out$drug_matched %in% c("aspirin", "trastuzumab")))
})

test_that("route_filter is applied case-insensitively and original route can be returned", {
  # Request original columns so route is present in output
  out_oral <- meds(sample_data, drugs = "aspirin", route_filter = "ORAL",
                   patient_id = "patient_id", order_date = "order_date", med_name = "med_name",
                   route = "route", inc_original_cols = TRUE)
  # Should only include the two oral rows
  expect_equal(nrow(out_oral), 2)
  expect_true("route" %in% names(out_oral))
  expect_true(all(tolower(out_oral$route) == "oral"))

  # route_filter NULL should not filter by route
  out_all <- meds(sample_data, drugs = "aspirin",
                  patient_id = "patient_id", order_date = "order_date", med_name = "med_name", route = "route")
  expect_equal(nrow(out_all), 3)
})

test_that("date_range filters rows correctly", {
  out <- meds(sample_data, drugs = c("aspirin", "5-fluorouracil"),
              date_range = c("2020-01-01", "2020-12-31"),
              patient_id = "patient_id", order_date = "order_date", med_name = "med_name")
  expect_false(any(out[[ "order_date" ]] == "2021-03-01"))
  expect_equal(nrow(out), 4)
})

test_that("first_only keeps earliest exposure per patient x drug", {
  out_first <- meds(sample_data, drugs = "aspirin", first_only = TRUE,
                    patient_id = "patient_id", order_date = "order_date", med_name = "med_name")
  # For aspirin: p1 and p3 should remain -> 2 rows
  expect_equal(nrow(out_first), 2)
  # Check that p1's date is the earliest (use string column name to avoid non-standard evaluation)
  p1_row <- out_first %>% filter(patient_id == "p1")
  expect_equal(as.character(p1_row[["order_date"]]), "2020-01-01")
})

test_that("inc_original_cols includes original columns and provenance columns are present", {
  out <- meds(sample_data, drugs = "aspirin", inc_original_cols = TRUE,
              patient_id = "patient_id", order_date = "order_date", med_name = "med_name", route = "route", dose = "dose")
  expect_true(all(c("patient_id", "order_date", "med_name", "route", "dose", "cohort_flag", "drug_matched") %in% names(out)))
  expect_true(all(out$cohort_flag))
})

test_that("defensive checks: non-data.frame input and missing required columns", {
  expect_error(meds(123, drugs = "aspirin"), "`data` must be a data.frame or tibble")

  bad_df <- sample_data %>% select(-med_name)
  expect_error(meds(bad_df, drugs = "aspirin"), "Missing required columns")

  bad_df2 <- sample_data %>% select(-route)
  expect_error(meds(bad_df2, drugs = "aspirin", route_filter = "oral", route = "route"),
               "`route` column not found in data, but route_filter provided")
})

test_that("invalid date_range argument errors (suppress lubridate parse warnings)", {
  expect_error(suppressWarnings(meds(sample_data, drugs = "aspirin", date_range = c("2020-01-01"))),
               "`date_range` must be a length-2 vector")
  expect_error(suppressWarnings(meds(sample_data, drugs = "aspirin", date_range = c("not-a-date", "2020-01-01"))),
               "date_range values must be parseable as dates")
})

test_that("no match returns empty tibble with expected columns", {
  out <- meds(sample_data, drugs = "nonexistentdrug",
              patient_id = "patient_id", order_date = "order_date", med_name = "med_name")
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("drug_matched", "cohort_flag") %in% names(out)))
  expect_equal(nrow(out), 0)
})
