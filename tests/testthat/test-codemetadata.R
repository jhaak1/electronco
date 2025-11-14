# tests/testthat/test-codemetadata.R
library(testthat)
library(yaml)

test_that("error when dataset argument is missing or empty", {
  expect_error(codemetadata(), "`dataset` must be supplied")
  expect_error(codemetadata(""), "`dataset` must be supplied")
})

test_that("error when file path is empty string", {
  expect_error(codemetadata("icd9", file = ""),
               "VERSIONS.yaml not found.")
})

test_that("error when file does not exist", {
  fakefile <- tempfile(fileext = ".yaml")
  expect_error(codemetadata("icd9", file = fakefile),
               "VERSIONS file does not exist.")
})

test_that("error when YAML cannot be parsed", {
  badfile <- tempfile(fileext = ".yaml")
  writeLines(":::: not valid yaml ::::", badfile)
  expect_error(codemetadata("icd9", file = badfile),
               "Failed to read VERSIONS.yaml.")
})

test_that("warning and NULL when YAML is empty", {
  emptyfile <- tempfile(fileext = ".yaml")
  writeLines("", emptyfile)
  expect_warning(
    res <- codemetadata("icd9", file = emptyfile),
    "VERSIONS file contains no entries"
  )
  expect_null(res)
})

test_that("error when YAML structure is not list of records", {
  wrongfile <- tempfile(fileext = ".yaml")
  writeLines(as.yaml(list(dataset = "icd9", data_version = "1.0")), wrongfile)
  expect_error(codemetadata("icd9", file = wrongfile),
               "Unexpected VERSIONS.yaml structure.")
})

test_that("warning and NULL when dataset not found", {
  goodfile <- tempfile(fileext = ".yaml")
  records <- list(
    list(dataset = "icd10", data_version = "v1")
  )
  writeLines(as.yaml(records), goodfile)
  expect_warning(
    res <- codemetadata("icd9", file = goodfile),
    "No provenance record found"
  )
  expect_null(res)
})

test_that("valid YAML returns provenance record with normalized fields", {
  goodfile <- tempfile(fileext = ".yaml")
  records <- list(
    list(dataset = "icd9", data_version = "v1", retrieved = "2025-01-01")
  )
  writeLines(as.yaml(records), goodfile)

  rec <- codemetadata("icd9", file = goodfile)
  expect_type(rec, "list")
  expect_equal(rec$dataset, "icd9")
  expect_equal(rec$data_version, "v1")
  expect_equal(rec$retrieved, "2025-01-01")
  expect_true("_versions_file" %in% names(rec))
})

test_that("sources list is normalized and missing fields filled with NA", {
  srcfile <- tempfile(fileext = ".yaml")
  records <- list(
    list(dataset = "cpt", sources = list(
      list(source_name = "CMS", source_url = "http://cms.gov")
    ))
  )
  writeLines(as.yaml(records), srcfile)

  rec <- codemetadata("cpt", file = srcfile)
  expect_true(is.list(rec$sources))
  src <- rec$sources[[1]]
  expect_equal(src$source_name, "CMS")
  expect_equal(src$source_url, "http://cms.gov")
  expect_true(all(c("original_filename", "retrieved") %in% names(src)))
  expect_true(is.na(src$original_filename))
  expect_true(is.na(src$retrieved))
})

test_that("dataset can be matched by list names", {
  namedfile <- tempfile(fileext = ".yaml")
  records <- list(
    icd10 = list(dataset = "icd10", data_version = "v2")
  )
  writeLines(as.yaml(records), namedfile)

  rec <- codemetadata("icd10", file = namedfile)
  expect_equal(rec$dataset, "icd10")
  expect_equal(rec$data_version, "v2")
})
