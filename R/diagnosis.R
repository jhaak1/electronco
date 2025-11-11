#' Extract diagnosis phenotype from diagnosis rows.
#'
#' Inputs:
#'  - diagnoses: tibble with the following columns: patient_id, code, code_system, date (Date).
#'  - concept_set: tibble with the following columns: code, code_system, include (logical TRUE=include, FALSE=exclude).
#'
#' Output: list(patient_level, evidence, metadata).
#' @param diagnoses Dataset imported from a database or csv file.
#' @param concept Concept to look for.  For breast cancer, specify 'bc'.
#' @param lookback_start Beginning date of date range to look at, in the format YYYY-MM-DD (year-month-day).
#' @param lookback_end End data of date range to look at, in the format YYYY-MM-DD (year-month-day).
#' @param min_events The minimum number of occurences of a given concept.
#' @param patient_id_col Name of the patient_id column in the "diagnoses" dataset.
#' @param code_col Name of the code column in the "diagnoses" dataset.
#' @param date_col Name of the diagnosis_date column in the "diagnoses" dataset.
#' @param system_col Name of the code_type column in the "diagnoses" dataset.
#' @export
#' @importFrom dplyr rename mutate left_join filter group_by ungroup arrange distinct summarize select count
#' @importFrom lubridate as_date
#' @importFrom rlang sym
#' @importFrom tidyr replace_na
#######################################################################################
# Helper function to parse date.
.parse_to_date <- function(x, name = "date") {
  # preserve length and return Date vector where possible
  n <- if (is.null(x)) 1L else length(x)
  # handle NULL
  if (is.null(x)) return(as.Date(NA))

  # Dates and POSIX
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))

  # Character: try base as.Date (ISO), then lubridate ymd / ymd_hms if available
  if (is.character(x)) {
    x2 <- trimws(x)
    # try vectorized base ISO parse
    base_try <- suppressWarnings(as.Date(x2))           # accepts "YYYY-MM-DD" and some variants
    if (!all(is.na(base_try))) {
      # return parsed vector but preserve NA positions
      return(base_try)
    }
    # try lubridate ymd / ymd_hms heuristics if available
    if (requireNamespace("lubridate", quietly = TRUE)) {
      # try ymd (handles "YYYYMMDD", "YYYY-MM-DD", etc.)
      lub_try <- suppressWarnings(lubridate::ymd(x2, quiet = TRUE))
      if (!all(is.na(lub_try))) return(as.Date(lub_try))
      lub_try2 <- suppressWarnings(lubridate::ymd_hms(x2, quiet = TRUE))
      if (!all(is.na(lub_try2))) return(as.Date(lub_try2))
      # try dmy / mdy fallbacks (less preferred)
      lub_try3 <- suppressWarnings(lubridate::dmy(x2, quiet = TRUE))
      if (!all(is.na(lub_try3))) return(as.Date(lub_try3))
      lub_try4 <- suppressWarnings(lubridate::mdy(x2, quiet = TRUE))
      if (!all(is.na(lub_try4))) return(as.Date(lub_try4))
    }
    warning(sprintf("Unable to parse %s string(s) to Date (first values shown): %s", name, paste(head(x2, 3), collapse = ", ")), call. = FALSE)
    return(as.Date(rep(NA, n)))
  }

  # Numeric: handle YYYYMMDD or epoch days
  if (is.numeric(x)) {
    # treat integers > 1e6 as YYYYMMDD
    if (all(x > 1e6, na.rm = TRUE)) {
      parsed1 <- suppressWarnings(as.Date(as.character(x), format = "%Y%m%d"))
      if (!all(is.na(parsed1))) {
        # for mixed numeric vectors (with NA), return parsed result
        return(parsed1)
      }
    }
    # fallback: treat as days since 1970-01-01
    parsed2 <- suppressWarnings(as.Date(x, origin = "1970-01-01"))
    if (!all(is.na(parsed2))) return(parsed2)

    warning(sprintf("Unable to coerce numeric %s to Date; returning NA", name), call. = FALSE)
    return(as.Date(rep(NA, n)))
  }

  # Other types: return NA with warning
  warning(sprintf("Unsupported type for %s; returning NA", name), call. = FALSE)
  as.Date(rep(NA, n))
}

######################################################################################
# Main function to make cohort based on diagnosis.

diagnosis <- function(diagnoses,
                      concept,
                      lookback_start,
                      lookback_end,
                      min_events,
                      patient_id_col = "patient_id",
                      code_col = "code",
                      system_col = "code_type",
                      date_col = "diagnosis_date") {

  # Convert lookback_start and lookback_end to dates.
  lookback_start = .parse_to_date(lookback_start, 'lookback_start')
  lookback_end = .parse_to_date(lookback_end, 'lookback_start')

  # Standardize column names for internal use.
  diag <- diagnoses %>%
    rename(
      .patient_id = !!sym(patient_id_col),
      .code = !!sym(code_col),
      .system = !!sym(system_col),
      .date = !!sym(date_col),
    ) %>%
    mutate(
      .code = toupper(gsub("\\.", "", as.character(.code))),
      .system = toupper(as.character(.system)),
      .date = as_date(.date)
    )

  # Get applicable concept set.
  if(concept == 'bc'){
    concept_set = bc_diag_concept
  } else {
    print('Warning: concept not recognized.')
    concept_set = NULL
  }

  cs <- concept_set %>%
    mutate(
      .code = toupper(gsub("\\.", "", as.character(code))),
      .system = toupper(as.character(code_system)),
      .include = as.logical(include)
    ) %>%
    select(.code, .system, .include)

  # Join to label each diagnosis row as include, exclude, or nomatch.
  evidence <- diag %>%
    left_join(cs, by = c(".code" = ".code", ".system" = ".system")) %>%
    mutate(
      .match = case_when(
        .include == T ~ "include",
        .include == F ~ "exclude",
        T ~ "nomatch"
      )
    )

  # Filter to lookback window.
  evidence_window <- evidence %>%
    filter(.date >= lookback_start, .date <= lookback_end)

  # Resolve exclusions at encounter level: if any exclusion code in same encounter, then mark row excluded.
  evidence_window <- evidence_window %>%
    group_by(.patient_id) %>%
    mutate(
      .encounter_has_exclusion = any(.match == "exclude", na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(.effective_match = if_else(.encounter_has_exclusion & .match == "include", "excluded_by_encounter", .match))

  # Keep only include evidence (not excluded_by_encounter).
  evidence_keep <- evidence_window %>%
    filter(.effective_match == "include") %>%
    arrange(.patient_id, .date)

  # Collapse duplicates per patient+date+code and compute counts.
  evidence_collapsed <- evidence_keep %>%
    distinct(.patient_id, .code, .system, .date) %>%
    group_by(.patient_id) %>%
    mutate(
      evidence_id = row_number()
    ) %>%
    ungroup()

  # Determine patient-level flag according to rules.
  patient_flags <- evidence_collapsed %>%
    dplyr::group_by(.patient_id) %>%
    dplyr::summarize(
      n_total = dplyr::n(),
      first_date = if (all(is.na(.date))) as.Date(NA) else min(.date, na.rm = TRUE),
      last_date  = if (all(is.na(.date))) as.Date(NA) else max(.date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      meets_min_event = n_total >= min_events,
      diagnosis_flag = meets_min_event
    )

  # Include patients with zero matches as FALSE.
  all_patients <- diagnoses %>%
    distinct(!!sym(patient_id_col)) %>%
    rename(.patient_id = !!sym(patient_id_col))

  patient_level <- all_patients %>%
    left_join(patient_flags, by = ".patient_id") %>%
    mutate(
      n_total = replace_na(n_total, 0),
      first_date = as_date(first_date),
      last_date = as_date(last_date),
      diagnosis_flag = replace_na(diagnosis_flag, FALSE)
    )

  metadata <- list(
    lookback_start = lookback_start,
    lookback_end = lookback_end,
    min_events = min_events,
    concept_set_used = cs,
    extraction_time = Sys.time()
  )

  # Return evidence rows (with canonical flag for first date).
  evidence_out <- evidence_collapsed %>%
    left_join(patient_flags %>% select(.patient_id, first_date), by = ".patient_id") %>%
    mutate(is_canonical = (.date == first_date)) %>%
    select(.patient_id, .code, .system, .date, is_canonical)

  list(
    patient_level = patient_level,
    evidence = evidence_out,
    metadata = metadata
  )
}
