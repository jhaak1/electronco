#' Extract procedure cohorts from local data
#'
#' @param data Data frame or tibble with procedure records.
#' @param code_list data.frame/tibble or path to CSV/YAML with columns: CODE, proc_label, proc_group.
#' @param patient_id_col name of patient id column (default "patient_id").
#' @param code_col name of procedure code column (default "cpt_code").
#' @param date_col name of procedure date column (default "proc_date").
#' @param date_format optional date format for as.Date if date_col is character.
#' @param cohort_by one of "proc_group" or "proc_label" (default "proc_group").
#' @param date_range optional c(start, end) to limit procedures considered.
#' @param index_dates optional data.frame/tibble with columns patient_id and index_date for windowed cohorts.
#' @param window_days numeric length of window after index_date to include (if index_dates provided). Use NA for no window.
#' @param min_count minimum number of matching procedures to qualify for cohort (default 1).
#' @param first_only logical; if TRUE cohort_start is first match and only that is used for inclusion (default TRUE).
#' @param verbose logical print progress (default TRUE).
#' @return tibble of cohorts (patient_id, cohort_name, cohort_start, cohort_end, cohort_count) with attribute "provenance".
#' @export
#' @importFrom stats setNames
extract_procedure_cohorts_local <- function(data,
                                            code_list,
                                            patient_id_col = "patient_id",
                                            code_col = "cpt_code",
                                            date_col = "proc_date",
                                            date_format = NULL,
                                            cohort_by = c("proc_group", "proc_label"),
                                            date_range = NULL,
                                            index_dates = NULL,
                                            window_days = NA,
                                            min_count = 1,
                                            first_only = TRUE,
                                            verbose = TRUE) {
  # Imports used inside
  stopifnot(is.data.frame(data))
  cohort_by <- match.arg(cohort_by)

  # -- load code_list (path or data.frame)
  codes <- NULL
  if (is.character(code_list) && length(code_list) == 1) {
    path <- code_list
    if (grepl("\\.ya?ml$", path, ignore.case = TRUE)) {
      codes <- yaml::read_yaml(path)
      if (is.list(codes) && !is.data.frame(codes)) {
        # try to coerce simple lists-of-lists to data.frame
        codes <- as.data.frame(do.call(rbind, lapply(codes, function(x) { as.data.frame(x, stringsAsFactors = FALSE) })), stringsAsFactors = FALSE)
      }
    } else if (grepl("\\.csv$", path, ignore.case = TRUE)) {
      codes <- utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character")
    } else {
      stop("`code_list` path must end with .csv or .yaml/.yml")
    }
  } else if (is.data.frame(code_list)) {
    codes <- as.data.frame(code_list, stringsAsFactors = FALSE)
  } else {
    stop("`code_list` must be a data.frame/tibble or path to CSV/YAML.")
  }

  # require expected columns
  required_code_cols <- c("CODE", "proc_label", "proc_group")
  if (!all(required_code_cols %in% colnames(codes))) {
    stop("`code_list` must contain columns: CODE, proc_label, proc_group.")
  }

  # ensure data columns exist
  if (!patient_id_col %in% colnames(data)) stop(paste0("`data` missing column: ", patient_id_col))
  if (!code_col %in% colnames(data)) stop(paste0("`data` missing column: ", code_col))
  if (!date_col %in% colnames(data)) stop(paste0("`data` missing column: ", date_col))

  # -- prepare tibble and parse dates
  df <- dplyr::as_tibble(data)
  if (!inherits(df[[date_col]], "Date")) {
    if (!is.null(date_format)) {
      df <- dplyr::mutate(df, !!date_col := as.Date(.data[[date_col]], format = date_format))
    } else {
      df <- dplyr::mutate(df, !!date_col := as.Date(.data[[date_col]]))
    }
  }

  # optional date_range filter
  if (!is.null(date_range)) {
    start_d <- as.Date(date_range[1])
    end_d <- as.Date(date_range[2])
    df <- dplyr::filter(df, .data[[date_col]] >= start_d & .data[[date_col]] <= end_d)
  }

  # normalization helper
  normalize_code <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- gsub("[[:punct:]]+", "", x)
    toupper(x)
  }

  codes$code_norm <- normalize_code(codes$CODE)
  df <- df %>%
    dplyr::mutate(.proc_code_raw = as.character(.data[[code_col]]),
                  .code_norm = normalize_code(.proc_code_raw))

  # perform match
  matched <- df %>%
    dplyr::inner_join(dplyr::select(dplyr::as_tibble(codes), code_norm, proc_label, proc_group),
                      by = c(".code_norm" = "code_norm"))

  n_input <- nrow(df)
  n_matched <- nrow(matched)
  unmatched_sample <- df %>%
    dplyr::filter(!.data$.code_norm %in% codes$code_norm) %>%
    dplyr::distinct(.data$.code_norm, .data$.proc_code_raw) %>%
    utils::head(20)

  if (isTRUE(verbose)) {
    message(sprintf("Procedures input=%d; matched=%d; unique unmatched sample=%d",
                    n_input, n_matched, nrow(unmatched_sample)))
  }

  # If index_dates provided, join them for windowing
  if (!is.null(index_dates)) {
    if (!is.data.frame(index_dates) || !all(c(patient_id_col, "index_date") %in% colnames(index_dates))) {
      stop("`index_dates` must be a data.frame with columns patient_id and index_date (use same name as patient_id_col).")
    }
    index_tbl <- dplyr::as_tibble(index_dates) %>%
      dplyr::mutate(index_date = as.Date(.data$index_date))
    matched <- matched %>%
      dplyr::left_join(index_tbl, by = setNames(patient_id_col, patient_id_col))
    if (!is.na(window_days)) {
      matched <- matched %>%
        dplyr::filter(.data[[date_col]] >= .data$index_date & .data[[date_col]] <= (.data$index_date + as.difftime(window_days, units = "days")))
    }
  }

  # group into cohorts
  cohort_col <- ifelse(cohort_by == "proc_group", "proc_group", "proc_label")
  cohorts <- matched %>%
    dplyr::group_by(!!rlang::sym(patient_id_col), !!rlang::sym(cohort_col)) %>%
    dplyr::summarise(
      cohort_count = dplyr::n(),
      cohort_start = min(.data[[date_col]], na.rm = TRUE),
      cohort_end = max(.data[[date_col]], na.rm = TRUE),
      sample_codes = paste(unique(.data$.proc_code_raw)[seq_len(min(3, length(unique(.data$.proc_code_raw))))], collapse = ";"),
      .groups = "drop"
    ) %>%
    dplyr::rename(patient_id = !!rlang::sym(patient_id_col),
                  cohort_name = !!rlang::sym(cohort_col))

  # apply min_count and first_only
  if (!is.null(min_count) && min_count > 1) {
    cohorts <- dplyr::filter(cohorts, cohort_count >= min_count)
  }
  if (isTRUE(first_only)) {
    cohorts <- cohorts %>%
      dplyr::mutate(cohort_start = as.Date(cohort_start)) %>%
      dplyr::group_by(patient_id, cohort_name) %>%
      dplyr::slice_min(order_by = cohort_start, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup()
  }

  # Build provenance/metadata
  prov <- list(
    extraction_time = Sys.time(),
    input_rows = n_input,
    matched_rows = n_matched,
    code_list_rows = nrow(codes),
    min_count = min_count,
    first_only = first_only,
    unmatched_sample = if (nrow(unmatched_sample) > 0) as.data.frame(unmatched_sample) else NULL
  )
  # try to capture code_list version if present
  prov$code_list_version <- if (!is.null(attr(code_list, "version"))) attr(code_list, "version") else NULL

  cohorts <- dplyr::as_tibble(cohorts)
  attr(cohorts, "provenance") <- prov
  class(cohorts) <- c("oncopheno_proc_cohorts", class(cohorts))

  return(cohorts)
}
