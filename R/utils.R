## utils.R
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "code", "bc_diag_concept", "patient_id", "n_total", "meets_min_event",
    "first_date", "last_date", "diagnosis_flag", "is_canonical",
    "bc_proc_concept", ".data", ".proc_code_raw", "code_norm", "group",
    "cohort_name", "cohort_count", "cohort_start", ".order_date_parsed",
    ".med_name_norm", "drug_matched", "cohort_flag", "desc", "n_tests", "first_lab_date",
    "flag","evidence_sample"
  ))
}
