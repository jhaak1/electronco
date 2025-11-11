#' Extract diagnosis phenotype from diagnosis rows.
#'
#' Inputs:
#'  - diagnoses: tibble with the following columns: patient_id, code, code_system, date (Date).
#'  - concept_set: tibble with the following columns: code, code_system, include (logical TRUE=include, FALSE=exclude).
#'  - params: list(lookback_start, lookback_end, min_occurrences)
#'
#' Output: list(patient_level, evidence, metadata) by default. If return_separate = TRUE,
#' the function assigns three objects into `target_env` named `patient_level`, `evidence`, and `metadata`
#' and returns invisibly NULL.
#'
#' @param diagnoses Dataset imported from a database or csv file.
#' @param concept Concept to look for. For breast cancer, specify "bc".
#' @param params NULL, a named list, or a positional list (lookback_start, lookback_end, min_occurrences).
#'   Date-like inputs are coerced internally.
#' @param patient_id_col Name of the patient_id column in the `diagnoses` dataset.
#' @param code_col Name of the code column in the `diagnoses` dataset.
#' @param system_col Name of the code_type column in the `diagnoses` dataset.
#' @param date_col Name of the diagnosis_date column in the `diagnoses` dataset.
#' @param return_separate Logical. If TRUE assign `patient_level`, `evidence`, `metadata` into `target_env` and return invisibly NULL.
#' @param target_env Environment in which to place separate data frames when return_separate = TRUE. Defaults to parent.frame().
#' @export
#' @importFrom dplyr rename mutate left_join filter group_by ungroup arrange distinct summarise select n row_number
#' @importFrom lubridate as_date
#' @importFrom rlang sym
#' @importFrom tidyr replace_na
diagnosis <- function(diagnoses,
                      concept,
                      params = NULL,
                      patient_id_col = "patient_id",
                      code_col = "code",
                      system_col = "code_type",
                      date_col = "diagnosis_date",
                      return_separate = FALSE,
                      target_env = parent.frame()) {

  # --- internal helper: coerce common inputs to Date ---------------------------------
  .parse_to_date <- function(x, name = "date") {
    if (is.null(x)) return(as.Date(NA))
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))

    if (is.character(x)) {
      x2 <- trimws(x)
      iso_try <- try(as.Date(x2), silent = TRUE)
      if (!inherits(iso_try, "try-error") && !all(is.na(iso_try))) return(iso_try)

      if (requireNamespace("lubridate", quietly = TRUE)) {
        lub_try <- try(lubridate::ymd(x2), silent = TRUE)
        if (!inherits(lub_try, "try-error") && !all(is.na(lub_try))) return(as.Date(lub_try))
        lub_try2 <- try(lubridate::ymd_hms(x2), silent = TRUE)
        if (!inherits(lub_try2, "try-error") && !all(is.na(lub_try2))) return(as.Date(lub_try2))
      }

      warning(sprintf("Unable to parse %s string to Date: %s", name, paste(head(x2, 3), collapse = ", ")), call. = FALSE)
      return(as.Date(NA))
    }

    if (is.numeric(x)) {
      if (all(x > 1e6)) {
        parsed <- try(as.Date(as.character(x), format = "%Y%m%d"), silent = TRUE)
        if (!inherits(parsed, "try-error") && !all(is.na(parsed))) return(parsed)
      }
      parsed2 <- try(as.Date(x, origin = "1970-01-01"), silent = TRUE)
      if (!inherits(parsed2, "try-error") && !all(is.na(parsed2))) return(parsed2)

      warning(sprintf("Unable to coerce numeric %s to Date; returning NA", name), call. = FALSE)
      return(as.Date(NA))
    }

    warning(sprintf("Unsupported type for %s; returning NA", name), call. = FALSE)
    as.Date(NA)
  }

  # --- defaults and params normalization -------------------------------------------
  .default_params <- list(
    lookback_start = as.Date("1900-01-01"),
    lookback_end   = Sys.Date(),
    min_occurrences = 1L
  )

  if (is.null(params)) {
    params_user <- list()
  } else if (!is.list(params)) {
    params_user <- list(params)
  } else {
    params_user <- params
  }

  # allow positional unnamed list
  if (is.null(names(params_user)) || all(names(params_user) == "")) {
    pos_names <- c("lookback_start", "lookback_end", "min_occurrences")
    npos <- min(length(params_user), length(pos_names))
    if (npos > 0) names(params_user)[seq_len(npos)] <- pos_names[seq_len(npos)]
  }

  # fill missing from defaults (user values override)
  params <- list()
  for (nm in names(.default_params)) {
    if (!is.null(params_user[[nm]])) {
      params[[nm]] <- params_user[[nm]]
    } else {
      params[[nm]] <- .default_params[[nm]]
    }
  }

  # coerce and validate min_occurrences
  params$min_occurrences <- as.integer(params$min_occurrences)
  if (length(params$min_occurrences) != 1L || is.na(params$min_occurrences) || params$min_occurrences < 1L) {
    stop("params$min_occurrences must be a single integer >= 1")
  }

  # coerce and validate dates
  params$lookback_start <- .parse_to_date(params$lookback_start, "params$lookback_start")
  params$lookback_end   <- .parse_to_date(params$lookback_end,   "params$lookback_end")
  if (is.na(params$lookback_start) || is.na(params$lookback_end) || params$lookback_start > params$lookback_end) {
    stop("params$lookback_start and params$lookback_end must be valid Dates with start <= end")
  }

  # --- standardize input columns ----------------------------------------------------
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

  # --- get concept set --------------------------------------------------------------
  if (identical(concept, "bc")) {
    if (!exists("bc_diag_concept", envir = environment())) {
      # try globalenv or package namespace; allow flexibility
      if (exists("bc_diag_concept", envir = .GlobalEnv)) {
        concept_set <- get("bc_diag_concept", envir = .GlobalEnv)
      } else if (exists("bc_diag_concept", envir = parent.env(environment()))) {
        concept_set <- get("bc_diag_concept", envir = parent.env(environment()))
      } else {
        stop("`bc_diag_concept` not found in environment; provide the concept set or ensure data is available")
      }
    } else {
      concept_set <- get("bc_diag_concept", envir = environment())
    }
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

  # --- label evidence ---------------------------------------------------------------
  evidence <- diag %>%
    dplyr::left_join(cs, by = c(".code" = ".code", ".system" = ".system")) %>%
    dplyr::mutate(
      .match = dplyr::case_when(
        .include == TRUE  ~ "include",
        .include == FALSE ~ "exclude",
        TRUE               ~ "nomatch"
      )
    )

  # --- windowing --------------------------------------------------------------------
  evidence_window <- evidence %>%
    dplyr::filter(.date >= params$lookback_start, .date <= params$lookback_end)

  # --- resolve exclusions (patient-level; encounter-level can be added separately) ---
  evidence_window <- evidence_window %>%
    dplyr::group_by(.patient_id) %>%
    dplyr::mutate(
      .encounter_has_exclusion = any(.match == "exclude", na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      .effective_match = dplyr::if_else(.encounter_has_exclusion & .match == "include",
                                        "excluded_by_encounter",
                                        .match)
    )

  # --- keep includes ----------------------------------------------------------------
  evidence_keep <- evidence_window %>%
    dplyr::filter(.effective_match == "include") %>%
    dplyr::arrange(.patient_id, .date)

  # --- collapse duplicates ----------------------------------------------------------
  evidence_collapsed <- evidence_keep %>%
    dplyr::distinct(.patient_id, .code, .system, .date) %>%
    dplyr::group_by(.patient_id) %>%
    dplyr::mutate(evidence_id = dplyr::row_number()) %>%
    dplyr::ungroup()

  # --- patient-level flags ----------------------------------------------------------
  if (nrow(evidence_collapsed) == 0L) {
    patient_flags <- tibble::tibble(
      .patient_id = character(),
      n_total = integer(),
      first_date = as.Date(character()),
      last_date = as.Date(character()),
      meets_min_occurrence = logical(),
      diagnosis_flag = logical()
    )
  } else {
    patient_flags <- evidence_collapsed %>%
      dplyr::group_by(.patient_id) %>%
      dplyr::summarise(
        n_total = dplyr::n(),
        first_date = min(.date, na.rm = TRUE),
        last_date = max(.date, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        meets_min_occurrence = n_total >= params$min_occurrences,
        diagnosis_flag = meets_min_occurrence
      )
  }

  # --- include patients with zero matches ------------------------------------------
  all_patients <- diagnoses %>%
    dplyr::distinct(!!rlang::sym(patient_id_col)) %>%
    dplyr::rename(.patient_id = !!rlang::sym(patient_id_col))

  patient_level <- all_patients %>%
    dplyr::left_join(patient_flags, by = ".patient_id") %>%
    dplyr::mutate(
      n_total = tidyr::replace_na(n_total, 0L),
      first_date = lubridate::as_date(first_date),
      last_date = lubridate::as_date(last_date),
      diagnosis_flag = tidyr::replace_na(diagnosis_flag, FALSE)
    )

  # --- metadata ---------------------------------------------------------------------
  metadata <- list(
    params = params,
    concept_set_used = cs,
    extraction_time = Sys.time()
  )

  # --- evidence_out -----------------------------------------------------------------
  evidence_out <- evidence_collapsed %>%
    dplyr::left_join(patient_flags %>% dplyr::select(.patient_id, first_date), by = ".patient_id") %>%
    dplyr::mutate(is_canonical = (.date == first_date)) %>%
    dplyr::select(.patient_id, .code, .system, .date, is_canonical)

  # --- return behavior -------------------------------------------------------------
  if (isTRUE(return_separate)) {
    assign("patient_level", patient_level, envir = target_env)
    assign("evidence", evidence_out, envir = target_env)
    assign("metadata", metadata, envir = target_env)
    return(invisible(NULL))
  }

  list(
    patient_level = patient_level,
    evidence = evidence_out,
    metadata = metadata
  )
}
