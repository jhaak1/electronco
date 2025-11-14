# tests/testthat/test-codeversions.R
library(testthat)
library(tibble)
library(yaml)

test_that("error when file path is empty string", {
  expect_error(codeversions(""),
               "VERSIONS.yaml not found.")
})

test_that("error when file does not exist", {
  fakefile <- tempfile(fileext = ".yaml")
  expect_error(codeversions(fakefile),
               "VERSIONS file does not exist.")
})

test_that("error when YAML cannot be parsed", {
  badfile <- tempfile(fileext = ".yaml")
  writeLines(":::: not valid yaml ::::", badfile)
  expect_error(codeversions(badfile),
               "Failed to read YAML VERSIONS file.")
})

test_that("empty tibble returned when YAML is empty", {
  emptyfile <- tempfile(fileext = ".yaml")
  writeLines("", emptyfile)
  df <- codeversions(emptyfile)
  expect_true(is_tibble(df))
  expect_equal(nrow(df), 0)
  expect_equal(names(df), c("dataset", "data_version", "retrieved"))
})

test_that("error when YAML structure is not list of records", {
  wrongfile <- tempfile(fileext = ".yaml")
  writeLines(as.yaml(list(dataset = "foo", data_version = "1.0")), wrongfile)
  expect_error(codeversions(wrongfile),
               "Unexpected VERSIONS.yaml structure.")
})

test_that("valid YAML produces tibble with correct fields", {
  goodfile <- tempfile(fileext = ".yaml")
  records <- list(
    list(dataset = "ds1", data_version = "v1", retrieved = "2025-01-01"),
    list(dataset = "ds2", data_version = "v2", retrieved = "2025-02-01")
  )
  writeLines(as.yaml(records), goodfile)

  df <- codeversions(goodfile)
  expect_true(is_tibble(df))
  expect_equal(nrow(df), 2)
  expect_equal(df$dataset, c("ds1", "ds2"))
  expect_equal(df$data_version, c("v1", "v2"))
  expect_equal(df$retrieved, c("2025-01-01", "2025-02-01"))
})

test_that("missing fields are converted to NA", {
  partialfile <- tempfile(fileext = ".yaml")
  records <- list(
    list(dataset = "ds1"), # Missing data_version and retrieved.
    list(data_version = "v2") # Missing dataset and retrieved.
  )
  writeLines(as.yaml(records), partialfile)

  df <- codeversions(partialfile)
  expect_true(all(c("dataset", "data_version", "retrieved") %in% names(df)))
  expect_true(anyNA(df$dataset))
  expect_true(anyNA(df$data_version))
  expect_true(anyNA(df$retrieved))
})

test_that("whitespace and 'NA' strings are normalized to NA", {
  normfile <- tempfile(fileext = ".yaml")
  records <- list(
    list(dataset = " ds1 ", data_version = "NA", retrieved = "")
  )
  writeLines(as.yaml(records), normfile)

  df <- codeversions(normfile)
  expect_equal(df$dataset, "ds1")
  expect_true(is.na(df$data_version))
  expect_true(is.na(df$retrieved))
})
