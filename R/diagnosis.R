#' Extract diagnosis phenotype from diagnosis rows.
#'
#' Inputs:
#'  - diagnoses: tibble with the following columns: patient_id, code, code_system, date (Date).
#'  - concept_set: tibble with the following columns: code, code_system, include (logical TRUE=include, FALSE=exclude).
#'  - params: list(lookback_start, lookback_end, min_occurrences)
#'
#' Output: list(patient_level, evidence, metadata).
#' @param diagnoses Dataset imported from a database or csv file.
#' @param concept Concept to look for.  For breast cancer, specify 'bc'.
#' @param params List of parameters supplied by the user (lookback_start, lookback_end, and min_occurences).
#' @param patient_id_col Name of the patient_id column in the "diagnoses" dataset.
#' @param code_col Name of the code column in the "diagnoses" dataset.
#' @param date_col Name of the diagnosis_date column in the "diagnoses" dataset.
#' @param system_col Name of the code_type column in the "diagnoses" dataset.
#' @export
#' @importFrom dplyr rename mutate left_join filter group_by ungroup arrange distinct summarise select count
#' @importFrom lubridate as_date
#' @importFrom rlang sym
#' @importFrom tidyr replace_na
# Internal helper: coerce common inputs to Date
.parse_to_date <- function(x, name = "date") {
  if (is.null(x)) return(NA_Date_)
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))

  # character: try ISO formats first, then try lubridate ymd/ymd_hms if available
  if (is.character(x)) {
    # trim whitespace
    x2 <- trimws(x)
    # try fast ISO parse
    iso_try <- try(as.Date(x2), silent = TRUE)
    if (!inherits(iso_try, "try-error") && !any(is.na(iso_try))) return(iso_try)

    # try common flexible formats via lubridate if available
    if (requireNamespace("lubridate", quietly = TRUE)) {
      lub_try <- try(lubridate::ymd(x2), silent = TRUE)
      if (!inherits(lub_try, "try-error") && !all(is.na(lub_try))) return(as.Date(lub_try))
      lub_try2 <- try(lubridate::ymd_hms(x2), silent = TRUE)
      if (!inherits(lub_try2, "try-error") && !all(is.na(lub_try2))) return(as.Date(lub_try2))
    }

    # fall through to NA
    warning(sprintf("Unable to parse %s string to Date: %s", name, paste(head(x2, 3), collapse = ", ")), call. = FALSE)
    return(as.Date(NA))
  }

  # numeric: treat common cases
  if (is.numeric(x)) {
    # If single large integer like 20230101 assume yyyyMMdd
    if (all(x > 1e6)) {
      parsed <- try(as.Date(as.character(x), format = "%Y%m%d"), silent = TRUE)
      if (!inherits(parsed, "try-error") && !all(is.na(parsed))) return(parsed)
    }
    # If small integers (days since epoch) use as.Date
    parsed2 <- try(as.Date(x, origin = "1970-01-01"), silent = TRUE)
    if (!inherits(parsed2, "try-error") && !all(is.na(parsed2))) return(parsed2)

    warning(sprintf("Unable to coerce numeric %s to Date; returning NA", name), call. = FALSE)
    return(as.Date(NA))
  }

  warning(sprintf("Unsupported type for %s; returning NA", name), call. = FALSE)
  as.Date(NA)
}

diagnosis <- function(diagnoses,
                      concept,
                      params = list(
                        lookback_start = as.Date("1900-01-01"),
                        lookback_end   = Sys.Date(),
                        min_occurrences = 1,
                      ),
                      patient_id_col = "patient_id",
                      code_col = "code",
                      system_col = "code_type",
                      date_col = "diagnosis_date") {

  # Default Params
  .default_params <- list(
    lookback_start = as.Date("1900-01-01"),
    lookback_end   = Sys.Date(),
    min_occurrences = 1L
  )

  # Ensure params is a list.
  if (!is.list(params)) params <- list(params)

  # Allow positional (unnamed) list: map first three positions to names.
  if (is.null(names(params)) || all(names(params) == "")) {
    pos_names <- c("lookback_start", "lookback_end", "min_occurrences")
    names(params)[seq_len(min(length(params), length(pos_names)))] <- pos_names[seq_len(min(length(params), length(pos_names)))]
  }

  # Merge with defaults so every field exists.
  params <- modifyList(.default_params, params)

  # Coerce and validate min_occurrences (ensure length 1 integer >= 1).
  params$min_occurrences <- as.integer(params$min_occurrences)
  if (length(params$min_occurrences) != 1 || is.na(params$min_occurrences) || params$min_occurrences < 1L) {
    stop("params$min_occurrences must be a single integer >= 1")
  }

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
    filter(.date >= params$lookback_start, .date <= params$lookback_end)

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

  # Collapse duplicates per patient+date+code+encounter and compute counts.
  evidence_collapsed <- evidence_keep %>%
    distinct(.patient_id, .code, .system, .date) %>%
    group_by(.patient_id) %>%
    mutate(
      evidence_id = row_number()
    ) %>%
    ungroup()

  # Determine patient-level flag according to rules.
  patient_flags <- evidence_collapsed %>%
    group_by(.patient_id) %>%
    summarise(
      n_total = n(),
      first_date = min(.date, na.rm = TRUE),
      last_date = max(.date, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      meets_min_occurrence = n_total >= params$min_occurrences,
      diagnosis_flag = meets_min_occurrence
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
    params = params,
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
