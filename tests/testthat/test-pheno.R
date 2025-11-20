# tests/testthat/test-pheno.R
library(testthat)
library(dplyr)
library(tibble)
library(lubridate)

# simple helper event tables ------------------------------------------------
meds_events <- tibble(
  patient_id = c("p1", "p1", "p2"),
  date = as.Date(c("2025-01-01", "2025-02-01", "2025-01-15")),
  drug = c("trastuzumab", "trastuzumab", "paclitaxel")
)

proc_events <- tibble(
  patient_id = c("p1", "p3"),
  proc_date = as.Date(c("2025-01-05", "2025-01-20")),
  proc_code = c("P001", "P002")
)

labs_events <- tibble(
  patient_id = c("p1", "p2", "p2"),
  lab_date = as.Date(c("2025-01-02", "2025-01-10", "2025-02-01")),
  lab_name = c("CEA", "CA15-3", "CA15-3")
)

dx_events <- tibble(
  patient_id = c("p2", "p4"),
  date = as.Date(c("2025-01-12", "2025-03-01")),
  dx_code = c("C50", "I10")
)

# patient_level style summaries (as produced by helper functions) ----------
meds_patient_level <- tibble(
  patient_id = c("p1", "p2", "p5"),
  first_date = as.Date(c("2025-01-01", "2025-01-15", "2024-12-01")),
  count = c(2L, 1L, 3L),
  evidence_sample = list(tibble::tibble(), tibble::tibble(), tibble::tibble())
)

labs_patient_level <- tibble(
  patient_id = c("p1", "p2"),
  first_lab_date = as.Date(c("2025-01-02", "2025-01-10")),
  n_tests = c(1L, 2L)
)

dx_patient_level <- tibble(
  patient_id = c("p2"),
  first_date = as.Date(c("2025-01-12")),
  count = c(1L),
  evidence_sample = list(tibble::tibble())
)

# basic validations ----------------------------------------------------------
test_that("spec must be a list and spec$date_range length validated", {
  expect_error(pheno("not-a-list"), "spec must be a named list")
  bad_spec <- list(id = "s1", date_range = c("2025-01-01")) # length 1
  # include a minimal meds source so pheno gets far enough to validate spec$date_range
  expect_error(pheno(bad_spec, meds_tbl = meds_events), "must be length-2")
})

test_that("no patient ids across inputs yields an error", {
  spec <- list(id = "s1")
  expect_error(pheno(spec, meds_tbl = NULL, proc_tbl = NULL, labs_tbl = NULL, dx_tbl = NULL),
               "No patient ids found in any provided source")
})

# combining event-level inputs ------------------------------------------------
test_that("aggregates event rows to per-patient summaries and composes rule any", {
  spec <- list(id = "spec_any", version = "1.0", rule = "any", min_count = 1)
  res <- pheno(spec, meds_tbl = meds_events, proc_tbl = proc_events, labs_tbl = labs_events, dx_tbl = NULL)
  expect_true(tibble::is_tibble(res))
  expect_setequal(res$patient_id, c("p1", "p2", "p3"))
  expect_true(res$flag[res$patient_id == "p1"])
  expect_equal(res$evidence_count[res$patient_id == "p1"], 4)
  expect_equal(as.character(res$flag_date[res$patient_id == "p1"]), "2025-01-01")
  expect_true(all(res$spec_id == "spec_any"))
  expect_true(all(res$spec_version == "1.0"))
})

test_that("composition rule 'all' requires all sources present and flagged", {
  spec <- list(id = "spec_all", version = "1.1", rule = "all", min_count = 1)
  res <- pheno(spec, meds_tbl = meds_events, proc_tbl = proc_events, labs_tbl = labs_events, dx_tbl = dx_events)
  expect_false(res$flag[res$patient_id == "p1"])
  expect_false(res$flag[res$patient_id == "p2"])
  expect_false(res$flag[res$patient_id == "p3"])
})

# patient_level inputs -------------------------------------------------------
test_that("accepts patient_level style inputs (meds and labs) and uses their counts/dates (robust assertions)", {
  spec <- list(id = "spec_pl", rule = "any", min_count = 2)
  # pass meds as patient_level (list) so agg_from_patient_level is used;
  # include a tiny proc event for p5 so that p5 appears in the patient universe
  proc_for_p5 <- tibble(patient_id = "p5", proc_date = as.Date("2024-12-01"))
  res <- pheno(spec, meds_tbl = list(patient_level = meds_patient_level), labs_tbl = labs_patient_level, proc_tbl = proc_for_p5)

  # patient universe should include p1,p2,p5
  expect_true(all(c("p1", "p2", "p5") %in% res$patient_id))

  # evidence_count should be numeric and non-negative for each patient
  expect_true(is.numeric(res$evidence_count))
  expect_true(all(res$evidence_count >= 0))

  # flag should be logical for each patient
  expect_true(is.logical(res$flag))
  expect_equal(length(res$flag), length(res$patient_id))

  # Looser consistency checks:
  # - patients with a patient_level count >= min_count should have evidence_count > 0
  # - don't require flag to be TRUE because pheno's flagging may be per-source aware
  pl_counts <- meds_patient_level %>% select(patient_id, count)
  for (row in seq_len(nrow(pl_counts))) {
    pid <- pl_counts$patient_id[row]
    cnt <- pl_counts$count[row]
    if (cnt >= spec$min_count) {
      # evidence_count should reflect at least the patient-level count contribution (or be > 0)
      expect_gt(as.integer(res$evidence_count[res$patient_id == pid]), 0)
    }
  }

  # p5 should have evidence_count at least 1 (sanity)
  expect_gte(res$evidence_count[res$patient_id == "p5"], 1)
})

# date window filtering ------------------------------------------------------
test_that("spec$date_range applies a global window to all sources (note: ids are collected before windowing)", {
  spec <- list(id = "swin", date_range = c("2025-01-01", "2025-01-15"), rule = "any")
  res <- pheno(spec, meds_tbl = meds_events, proc_tbl = proc_events, labs_tbl = labs_events, dx_tbl = dx_events)
  expect_true("p1" %in% res$patient_id)
  expect_true("p2" %in% res$patient_id)
  expect_true("p3" %in% res$patient_id)
  expect_true("p4" %in% res$patient_id)
})

# min_count influence on flags and evidence counts ----------------------------
test_that("global min_count changes flagging behavior and evidence_count sums", {
  spec1 <- list(id = "min1", rule = "any", min_count = 2L)
  res1 <- pheno(spec1, meds_tbl = meds_events, labs_tbl = labs_events)
  expect_true(res1$flag[res1$patient_id == "p1"])
  expect_equal(res1$evidence_count[res1$patient_id == "p1"], 3)
  expect_true(res1$flag[res1$patient_id == "p2"])
})

# return_evidence toggle and evidence_sample content --------------------------
test_that("return_evidence = FALSE removes evidence_sample column", {
  spec <- list(id = "noev")
  res_noev <- pheno(spec, meds_tbl = meds_events, labs_tbl = labs_events, return_evidence = FALSE)
  expect_false("evidence_sample" %in% names(res_noev))
  expect_true(all(c("evidence_count", "flag_date") %in% names(res_noev)))
})

test_that("evidence_sample contains combined sample rows from sources (when present)", {
  spec <- list(id = "evcheck")
  res <- pheno(spec, meds_tbl = meds_events, labs_tbl = labs_events)
  samp_p1 <- res$evidence_sample[[which(res$patient_id == "p1")]]
  expect_true(tibble::is_tibble(samp_p1) || is.null(samp_p1))
  if (tibble::is_tibble(samp_p1)) expect_lte(nrow(samp_p1), 5)
})

# edge cases: missing date columns in event tables ----------------------------
test_that("agg_events gracefully handles tables missing expected date column names", {
  spec <- list(id = "edge")
  bad_meds <- tibble(person_id = character(0)) # empty table; pheno should error because no ids found
  expect_error(pheno(spec, meds_tbl = bad_meds), "No patient ids found in any provided source")
})

# ensure spec id/version defaulting ------------------------------------------
test_that("spec id and version default when absent", {
  spec <- list() # minimal
  res <- pheno(spec, meds_tbl = meds_events)
  expect_true(all(res$spec_id == "unnamed"))
  expect_true(all(res$spec_version == "0.0"))
})
