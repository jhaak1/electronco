#' Extract procedures from local data
#'
#' @description
#' Match local procedure records to a CPT code list and return a standardized
#' tibble with provenance metadata.
#'
#' @param data Data frame or tibble containing procedure records.
#' @param patient_id_col Character name of patient id column in data; default "patient_id".
#' @param code_col Character name of procedure code column in data; default "cpt_code".
#' @param date_col Character name of procedure date column in data; default "proc_date".
#' @param date_format Character accepted by as.Date if date_col is character; default NULL.
#' @param verbose Logical; print progress messages. Default TRUE.
#' @return tibble of matched procedures with attribute "metadata" (list).
#' @examples
#' # res <- proc(df, patient_id_col = "patient_id")
#' @importFrom dplyr as_tibble mutate transmute row_number arrange inner_join select filter distinct
#' @importFrom rlang .data
#' @importFrom tibble as_tibble
#' @importFrom yaml read_yaml
proc <- function(data,
                 patient_id_col = "patient_id",
                 code_col = "cpt_code",
                 date_col = "proc_date",
                 date_format = NULL,
                 verbose = TRUE) {

  # Basic Validation
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame or tibble.")
  }

  # Load code_list.
  codes <- cpt

  # Make sure required code columns are present.
  required_code_cols <- c("CODE", "Description")
  if (!all(required_code_cols %in% colnames(codes))) {
    stop("`code_list` must contain columns: CODE, Description.")
  }

  # Ensure procedure data has required columns.
  if (!patient_id_col %in% colnames(data)) stop(paste0("`data` missing column: ", patient_id_col))
  if (!code_col %in% colnames(data)) stop(paste0("`data` missing column: ", code_col))
  if (!date_col %in% colnames(data)) stop(paste0("`data` missing column: ", date_col))

  # Prepare tibble.
  df <- dplyr::as_tibble(data)

  # Parse date column robustly.
  if (!inherits(df[[date_col]], "Date")) {
    if (!is.null(date_format)) {
      df <- dplyr::mutate(df, !!date_col := as.Date(.data[[date_col]], format = date_format))
    } else {
      df <- dplyr::mutate(df, !!date_col := as.Date(.data[[date_col]]))
    }
  }

  # Normalize code fields for deterministic matching.
  normalize_code <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    x <- gsub("[[:punct:]]+", "", x)   # Remove punctuation.
    toupper(x)
  }

  codes$CODE <- as.character(codes$CODE)
  codes$code_norm <- normalize_code(codes$CODE)

  df <- df %>%
    dplyr::mutate(.proc_code_raw = as.character(.data[[code_col]]),
                  .code_norm = normalize_code(.proc_code_raw))

  # Inner Join on Normalized Code
  matched <- df %>%
    dplyr::inner_join(
      dplyr::select(dplyr::as_tibble(codes), CODE, Description, code_norm),
      by = c(".code_norm" = "code_norm")
    )

  # Counts and Unmatched Sample
  n_input <- nrow(df)
  n_matched <- nrow(matched)
  unmatched_codes <- df %>%
    dplyr::filter(!.data$.code_norm %in% codes$code_norm) %>%
    dplyr::distinct(.data$.code_norm, .data$.proc_code_raw) %>%
    utils::head(20)

  if (isTRUE(verbose)) {
    message(sprintf("Procedures: input rows=%d; matched rows=%d; unique unmatched sample=%d",
                    n_input, n_matched, nrow(unmatched_codes)))
  }

  # Build output tibble.
  out <- matched %>%
    dplyr::transmute(
      patient_id = .data[[patient_id_col]],
      proc_date = .data[[date_col]],
      code = .data$.proc_code_raw,
      code_norm = .data$.code_norm,
      proc_label = .data$Description,
      source_row_id = dplyr::row_number()
    ) %>%
    dplyr::arrange(.data$patient_id, .data$proc_date)

  # Metadata: try to read package VERSIONS.yaml if it exists (adjust package name as needed).
  metadata <- list(
    extraction_time = Sys.time(),
    dataset = "cpt",
    code_list_rows = nrow(codes),
    unmatched_sample = if (nrow(unmatched_codes) > 0) as.data.frame(unmatched_codes) else NULL
  )

  # Attempt to read extdata VERSIONS.yaml safely (non-fatal).
  try({
    meta_path <- system.file("extdata", "VERSIONS.yaml", package = "electronco")
    if (nzchar(meta_path)) {
      meta1 <- yaml::read_yaml(meta_path)
      if (is.list(meta1) && !is.null(meta1[["cpt"]])) {
        metadata$data_version <- meta1[["cpt"]][["data_version"]]
        metadata$retrieved <- meta1[["cpt"]][["retrieved"]]
      }
    }
  }, silent = TRUE)

  out <- tibble::as_tibble(out)
  attr(out, "metadata") <- metadata
  class(out) <- c("oncopheno_procedures", class(out))

  return(out)
}
