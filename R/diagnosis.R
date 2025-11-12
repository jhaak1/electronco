#' Extract diagnosis phenotype from diagnosis data.
#'
#' Inputs:
#'  - diagnoses: tibble or data frame with the following columns: patient_id, code, code_type, diagnosis_date.
#'  - concept_set (included with package): tibble with the following columns: code, code_type, include (logical TRUE=include, FALSE=exclude).
#'
#' Output: list(patient_level, evidence, metadata).
#' @param diagnoses Dataset imported from a database or csv file.
#' @param concept Concept to look for. For breast cancer, specify 'bc'.
#' @param lookback_start Beginning date of date range to look at in YYYY-MM-DD (year-month-day) format.
#' @param lookback_end End date of date range to look at in YYYY-MM-DD (year-month-day) format.
#' @param min_events The minimum number of occurrences of a given concept.
#' @param patient_id_col Name of the patient_id column in the "diagnoses" dataset.
#' @param code_col Name of the code column in the "diagnoses" dataset.
#' @param system Name of the code_type column in the "diagnoses" dataset.
#' @param date_col Name of the diagnosis_date column in the "diagnoses" dataset.
#' @export
#' @importFrom dplyr mutate across all_of rename left_join group_by summarize arrange distinct n case_when select
#' @importFrom tidyr replace_na
#' @importFrom lubridate as_date
diagnosis <- function(diagnoses,
                      concept,
                      lookback_start,
                      lookback_end,
                      min_events = 1,
                      patient_id_col = "patient_id",
                      code_col = "code",
                      system = "code_type",
                      date_col = "diagnosis_date") {

  # Convert lookback_start and lookback_end to date objects.
  lookback_start = as.Date(lookback_start, '%Y-%m-%d')
  lookback_end = as.Date(lookback_end, '%Y-%m-%d')

  # validate input columns early
  required <- c(patient_id_col, code_col, system, date_col)
  missing_cols <- setdiff(required, names(diagnoses))
  if (length(missing_cols) > 0L) {
    stop("diagnoses is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # normalize types and values using programmatic column selection
  diagnoses <- diagnoses %>%
    dplyr::mutate(
      # ensure patient id string
      dplyr::across(dplyr::all_of(patient_id_col), as.character),
      # uppercase code and system columns (operate on the actual columns named by the params)
      dplyr::across(dplyr::all_of(code_col), ~ toupper(.x)),
      dplyr::across(dplyr::all_of(system),   ~ toupper(.x)),
      # parse date column into Date
      dplyr::across(dplyr::all_of(date_col), ~ as.Date(.x, format = "%Y-%m-%d"))
    )

  # canonicalize names to patient_id / code / system / date for downstream code
  diag <- diagnoses %>%
    dplyr::rename(
      patient_id = dplyr::all_of(patient_id_col),
      code       = dplyr::all_of(code_col),
      system     = dplyr::all_of(system),
      date       = dplyr::all_of(date_col)
    )

  diag <- diag %>%
    dplyr::mutate(code = toupper(trimws(code)), system = toupper(trimws(system)))



  # Get concept set.
  if(concept == 'bc'){
    concept_set = bc_diag_concept
    } else {
    print('Warning: concept not recognized.')
    concept_set = NULL
    }

  concept_set <- concept_set %>%
    dplyr::mutate(code = toupper(trimws(code)), system = toupper(trimws(system)))

  # Join diagnoses with concept set.
  evidence <- diag %>%
    dplyr::left_join(concept_set, by = c("code", "system")) %>%
    dplyr::mutate(
      match = dplyr::case_when(
        include == TRUE  ~ "include",
        include == FALSE ~ "exclude",
        TRUE               ~ "nomatch"
      )
    )

  # Filter for lookback window dates.
  evidence_window <- evidence %>%
    dplyr::filter(date >= lookback_start, date <= lookback_end)

  # Keep rows whose effective match is "include" only.
  evidence_keep <- evidence_window %>%
    dplyr::filter(match == "include") %>%
    dplyr::arrange(patient_id, date)

  # Collapse duplicates based on patient + date + code.
  evidence_collapsed <- evidence_keep %>%
    dplyr::distinct(patient_id, code, system, date) %>%
    dplyr::group_by(patient_id) %>%
    dplyr::mutate(evidence_id = dplyr::row_number()) %>%
    dplyr::ungroup()

  # Patient-level Flags
  if (nrow(evidence_collapsed) == 0L) {
    patient_flags <- tibble::tibble(
      patient_id = character(),
      n_total = integer(),
      first_date = as.Date(character()),
      last_date = as.Date(character()),
      meets_min_event = logical(),
      diagnosis_flag = logical()
    )
  } else {
    patient_flags <- evidence_collapsed %>%
      dplyr::group_by(patient_id) %>%
      dplyr::summarize(
        n_total = dplyr::n(),
        first_date = if (all(is.na(date))) as.Date(NA) else min(date, na.rm = TRUE),
        last_date  = if (all(is.na(date))) as.Date(NA) else max(date, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        meets_min_event = n_total >= min_events,
        diagnosis_flag = meets_min_event
      )
  }

  # Include patients with zero matches as FALSE.
  all_patients <- diagnoses %>%
    dplyr::distinct(patient_id) %>%
    dplyr::rename(patient_id = patient_id) %>%
    dplyr::mutate(patient_id = as.character(patient_id))

  patient_flags <- patient_flags %>%
    dplyr::mutate(patient_id = as.character(patient_id))

  patient_level <- all_patients %>%
    dplyr::left_join(patient_flags, by = "patient_id") %>%
    dplyr::mutate(
      n_total = tidyr::replace_na(n_total, 0L),
      first_date = lubridate::as_date(first_date),
      last_date  = lubridate::as_date(last_date),
      meets_min_event = tidyr::replace_na(meets_min_event, FALSE),
      diagnosis_flag  = tidyr::replace_na(diagnosis_flag, FALSE)
    )

  # Metadata
  meta1 = yaml::read_yaml(system.file('extdata', 'VERSIONS.yaml', package = 'electronco'))

  metadata <- list(
    lookback_start = lookback_start,
    lookback_end = lookback_end,
    min_events = min_events,
    concept_set_used = concept,
    extraction_time = Sys.time(),
    dataset = meta1[[concept]][['dataset']],
    data_version = meta1[[concept]][['data_version']],
    retrieved = meta1[[concept]][['retrieved']]
  )

  # Evidence Out
  evidence_out <- evidence_collapsed %>%
    dplyr::left_join(patient_flags %>% dplyr::select(patient_id, first_date), by = "patient_id") %>%
    dplyr::mutate(is_canonical = (date == first_date)) %>%
    dplyr::select(patient_id, code, system, date, is_canonical)

  # Return patient-level data, evidence, and metadata.
  list(
    patient_level = patient_level,
    evidence = evidence_out,
    metadata = metadata
  )
}
