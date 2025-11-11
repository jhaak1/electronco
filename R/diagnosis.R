#' Extract diagnosis phenotype from diagnosis rows.
#'
#' Inputs:
#'  - diagnoses: tibble with the following columns: patient_id, code, code_system, date (Date).
#'  - concept_set: tibble with the following columns: code, code_system, include (logical TRUE=include, FALSE=exclude).
#'
#' Output: list(patient_level, evidence, metadata).
#' @param diagnoses Dataset imported from a database or csv file.
#' @param concept Concept to look for. For breast cancer, specify 'bc'.
#' @param lookback_start Beginning date of date range to look at (Date or coercible string).
#' @param lookback_end End date of date range to look at (Date or coercible string).
#' @param min_events The minimum number of occurrences of a given concept.
#' @param patient_id_col Name of the patient_id column in the "diagnoses" dataset.
#' @param code_col Name of the code column in the "diagnoses" dataset.
#' @param system_col Name of the code_type column in the "diagnoses" dataset.
#' @param date_col Name of the diagnosis_date column in the "diagnoses" dataset.
#' @export
diagnosis <- function(diagnoses,
                      concept,
                      lookback_start,
                      lookback_end,
                      min_events = 1,
                      patient_id_col = "patient_id",
                      code_col = "code",
                      system_col = "code_type",
                      date_col = "diagnosis_date") {

  # -- internal helper: flexible date coercion ---------------------------------------
  .parse_to_date <- function(x, name = "date") {
    if (is.null(x)) return(as.Date(NA))
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))

    if (is.character(x)) {
      x2 <- trimws(x)
      parsed <- suppressWarnings(as.Date(x2))
      if (!all(is.na(parsed))) return(parsed)
      if (requireNamespace("lubridate", quietly = TRUE)) {
        parsed <- suppressWarnings(lubridate::ymd(x2, quiet = TRUE))
        if (!all(is.na(parsed))) return(as.Date(parsed))
        parsed <- suppressWarnings(lubridate::ymd_hms(x2, quiet = TRUE))
        if (!all(is.na(parsed))) return(as.Date(parsed))
      }
      stop(sprintf("Unable to parse %s to Date (example values: %s)", name, paste(head(x2, 3), collapse = ", ")))
    }

    if (is.numeric(x)) {
      if (all(x > 1e6, na.rm = TRUE)) {
        parsed <- suppressWarnings(as.Date(as.character(x), format = "%Y%m%d"))
        if (!all(is.na(parsed))) return(parsed)
      }
      parsed2 <- suppressWarnings(as.Date(x, origin = "1970-01-01"))
      if (!all(is.na(parsed2))) return(parsed2)
      stop(sprintf("Unable to coerce numeric %s to Date", name))
    }

    stop(sprintf("Unsupported type for %s; provide Date, POSIXt, character, or numeric YYYYMMDD", name))
  }

  # -- coerce and validate lookback dates ------------------------------------------
  lookback_start <- .parse_to_date(lookback_start, "lookback_start")
  lookback_end   <- .parse_to_date(lookback_end,   "lookback_end")
  if (is.na(lookback_start) || is.na(lookback_end)) stop("lookback_start and lookback_end must be valid dates")
  if (lookback_start > lookback_end) stop("lookback_start must be <= lookback_end")

  # -- validate min_events ----------------------------------------------------------
  min_events <- as.integer(min_events)
  if (length(min_events) != 1L || is.na(min_events) || min_events < 1L) stop("min_events must be a single integer >= 1")

  # -- standardize input columns ----------------------------------------------------
  diag <- diagnoses %>%
    dplyr::rename(
      .patient_id = !!rlang::sym(patient_id_col),
      .code = !!rlang::sym(code_col),
      .system = !!rlang::sym(system_col),
      .date = !!rlang::sym(date_col)
    ) %>%
    dplyr::mutate(
      .code = toupper(gsub("\\.", "", as.character(.code))),
      .system = toupper(as.character(.system)),
      .date = lubridate::as_date(.date)
    )

  # report if many missing dates (optional warning)
  if (sum(is.na(diag$.date)) > 0) {
    warning("Some diagnosis rows have missing or unparsable dates; these rows will be excluded by the lookback window")
  }

  # -- get concept set --------------------------------------------------------------
  if (identical(concept, "bc")) {
    if (exists("bc_diag_concept", envir = .GlobalEnv)) concept_set <- get("bc_diag_concept", envir = .GlobalEnv)
    else stop("Concept set 'bc_diag_concept' not found in the global environment")
  } else {
    stop("Unknown concept: ", as.character(concept))
  }

  cs <- concept_set %>%
    dplyr::mutate(
      .code = toupper(gsub("\\.", "", as.character(code))),
      .system = toupper(as.character(code_system)),
      .include = as.logical(include)
    ) %>%
    dplyr::select(.code, .system, .include)

  # -- label evidence (include/exclude/nomatch) ------------------------------------
  evidence <- diag %>%
    dplyr::left_join(cs, by = c(".code" = ".code", ".system" = ".system")) %>%
    dplyr::mutate(
      .match = dplyr::case_when(
        .include == TRUE  ~ "include",
        .include == FALSE ~ "exclude",
        TRUE               ~ "nomatch"
      )
    )

  # -- filter to lookback window ----------------------------------------------------
  evidence_window <- evidence %>%
    dplyr::filter(.date >= lookback_start, .date <= lookback_end)

  ####################################################################################
  evidence_window %>% dplyr::count(.match, .include, .system, .code, sort = TRUE)

  # print the full evidence_window using base-friendly print
  print(as.data.frame(evidence_window[, c(".patient_id", ".date", ".code", ".system", ".include", ".match")]))

  ####################################################################################

  # -- SKIP encounter-level exclusion (no encounter data available) -----------------
  # Keep rows whose effective match is "include" only; do not apply encounter-level exclusion.
  evidence_keep <- evidence_window %>%
    dplyr::filter(.match == "include") %>%
    dplyr::arrange(.patient_id, .date)

  # -- collapse duplicates per patient+date+code ------------------------------------
  evidence_collapsed <- evidence_keep %>%
    dplyr::distinct(.patient_id, .code, .system, .date) %>%
    dplyr::group_by(.patient_id) %>%
    dplyr::mutate(evidence_id = dplyr::row_number()) %>%
    dplyr::ungroup()

  # -- patient-level flags ----------------------------------------------------------
  if (nrow(evidence_collapsed) == 0L) {
    patient_flags <- tibble::tibble(
      .patient_id = character(),
      n_total = integer(),
      first_date = as.Date(character()),
      last_date = as.Date(character()),
      meets_min_event = logical(),
      diagnosis_flag = logical()
    )
  } else {
    patient_flags <- evidence_collapsed %>%
      dplyr::group_by(.patient_id) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        first_date = if (all(is.na(.date))) as.Date(NA) else min(.date, na.rm = TRUE),
        last_date  = if (all(is.na(.date))) as.Date(NA) else max(.date, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        meets_min_event = n_total >= min_events,
        diagnosis_flag = meets_min_event
      )
  }

  # -- include patients with zero matches as FALSE ----------------------------------
  all_patients <- diagnoses %>%
    dplyr::distinct(!!rlang::sym(patient_id_col)) %>%
    dplyr::rename(.patient_id = !!rlang::sym(patient_id_col)) %>%
    dplyr::mutate(.patient_id = as.character(.patient_id))

  patient_flags <- patient_flags %>% dplyr::mutate(.patient_id = as.character(.patient_id))

  patient_level <- all_patients %>%
    dplyr::left_join(patient_flags, by = ".patient_id") %>%
    dplyr::mutate(
      n_total = tidyr::replace_na(n_total, 0L),
      first_date = lubridate::as_date(first_date),
      last_date = lubridate::as_date(last_date),
      diagnosis_flag = tidyr::replace_na(diagnosis_flag, FALSE)
    )

  # -- metadata ---------------------------------------------------------------------
  metadata <- list(
    lookback_start = lookback_start,
    lookback_end = lookback_end,
    min_events = min_events,
    concept_set_used = cs,
    extraction_time = Sys.time()
  )

  # -- evidence_out -----------------------------------------------------------------
  evidence_out <- evidence_collapsed %>%
    dplyr::left_join(patient_flags %>% dplyr::select(.patient_id, first_date), by = ".patient_id") %>%
    dplyr::mutate(is_canonical = (.date == first_date)) %>%
    dplyr::select(.patient_id, .code, .system, .date, is_canonical)

  # -- return -----------------------------------------------------------------------
  list(
    patient_level = patient_level,
    evidence = evidence_out,
    metadata = metadata
  )
}
