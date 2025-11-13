#' Extract procedure cohorts from local data.
#'
#' @param data Data frame or tibble with procedure records.
#' @param proc_group The procedure group or groups to be included.  Options are
#'  'breast cancer screening', 'diagnostic mammography', 'breast ultrasound',
#'  'mri of the breast', 'needle biopsy', 'breast specimen radiography', 'pathology',
#'  'tumor marker testing', 'breast-conserving surgery', 'mastectomy procedure',
#'  'breast reconstruction', 'lymph node procedure', 'radiation therapy', 'chemotherapy',
#'  'follow-up care', and 'breast cancer screening'.
#' @param patient_id_col Name of patient id column (default "patient_id").
#' @param code_col Name of procedure code column (default "cpt_code").
#' @param date_col Name of procedure date column (default "proc_date").
#' @param date_format Optional date format for as.Date if date_col is character.
#' @param date_range Optional c(start, end) to limit procedures considered.
#' @param min_count Minimum number of matching procedures to qualify for cohort (default 1).
#' @param first_only Logical; if TRUE cohort_start is first match and only that is used for inclusion (default FALSE).
#' @return tibble of cohorts (patient_id, cohort_name, cohort_start, cohort_end, cohort_count) and metadata.
#' @export
#' @importFrom stats setNames
#' @importFrom dplyr as_tibble mutate filter inner_join select group_by summarize rename slice_min ungroup
#' @importFrom rlang sym
#' @importFrom yaml read_yaml
proc <- function(data,
                  proc_group = c('breast cancer screening', 'diagnostic mammography', 'breast ultrasound',
                    'mri of the breast', 'needle biopsy', 'breast specimen radiography', 'pathology',
                    'tumor marker testing', 'breast-conserving surgery', 'mastectomy procedure',
                    'breast reconstruction', 'lymph node procedure', 'radiation therapy', 'chemotherapy',
                    'follow-up care', 'breast cancer screening'),
                  patient_id_col = "patient_id",
                  code_col = "cpt_code",
                  date_col = "proc_date",
                  date_format = NULL,
                  date_range = NULL,
                  min_count = 1,
                  first_only = FALSE) {
  # Imports used inside.
  stopifnot(is.data.frame(data))

  # Load code_list.
  codes <- bc_proc_concept

  # Require expected columns.
  required_code_cols <- c("code", "group")
  if (!all(required_code_cols %in% colnames(codes))) {
    stop("`code_list` must contain columns: code, group.")
  }

  # Ensure data columns exist.
  if (!patient_id_col %in% colnames(data)) stop(paste0("`data` missing column: ", patient_id_col))
  if (!code_col %in% colnames(data)) stop(paste0("`data` missing column: ", code_col))
  if (!date_col %in% colnames(data)) stop(paste0("`data` missing column: ", date_col))

  # Prepare tibble and parse dates.
  df <- dplyr::as_tibble(data)
  if (!inherits(df[[date_col]], "Date")) {
    if (!is.null(date_format)) {
      df <- dplyr::mutate(df, !!date_col := as.Date(.data[[date_col]], format = date_format))
    } else {
      df <- dplyr::mutate(df, !!date_col := as.Date(.data[[date_col]]))
    }
  }

  # Optional date_range filter.
  if (!is.null(date_range)) {
    start_d <- as.Date(date_range[1])
    end_d <- as.Date(date_range[2])
    df <- dplyr::filter(df, .data[[date_col]] >= start_d & .data[[date_col]] <= end_d)
  }

  # Normalization Helper
  normalize_code <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- gsub("[[:punct:]]+", "", x)
    toupper(x)
  }

  codes$code_norm <- normalize_code(codes$code)
  df <- df %>%
    dplyr::mutate(.proc_code_raw = as.character(.data[[code_col]]),
                  .code_norm = normalize_code(.proc_code_raw))

  # Perform match.
  matched <- df %>%
    dplyr::inner_join(dplyr::select(dplyr::as_tibble(codes), code_norm, group),
                      by = c(".code_norm" = "code_norm"))

  n_input <- nrow(df)
  n_matched <- nrow(matched)
  unmatched_sample <- df %>%
    dplyr::filter(!.data$.code_norm %in% codes$code_norm) %>%
    dplyr::distinct(.data$.code_norm, .data$.proc_code_raw) %>%
    utils::head(20)

  # Group into cohorts.
  cohort_col <- 'group'
  cohorts <- matched %>%
    dplyr::group_by(!!rlang::sym(patient_id_col), !!rlang::sym(cohort_col)) %>%
    dplyr::summarize(
      cohort_count = dplyr::n(),
      cohort_start = min(.data[[date_col]], na.rm = TRUE),
      cohort_end = max(.data[[date_col]], na.rm = TRUE),
      sample_codes = paste(unique(.data$.proc_code_raw)[seq_len(min(3, length(unique(.data$.proc_code_raw))))], collapse = ";"),
      .groups = "drop"
    ) %>%
    dplyr::rename(patient_id = !!rlang::sym(patient_id_col),
                  cohort_name = !!rlang::sym(cohort_col))

  # Apply min_count and first_only
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

  # Build metadata.
  meta1 = yaml::read_yaml(system.file('extdata', 'VERSIONS.yaml', package = 'electronco'))

  metadata <- list(
    date_range = date_range,
    min_events = min_count,
    concept_set_used = proc_group,
    extraction_time = Sys.time(),
    dataset = meta1[[proc_group]][['dataset']],
    data_version = meta1[[proc_group]][['data_version']],
    retrieved = meta1[[proc_group]][['retrieved']]
  )

  # Return patient-level data and metadata.
  list(
    patient_level = cohorts,
    metadata = metadata
  )
}
