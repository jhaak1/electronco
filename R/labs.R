#' Extract lab marker cohorts.
#'
#' @param data Dataframe or tibble with input data.
#' @param markers Vector of the markers to look for.
#' @param match_type How to match the values specified in "markers".
#' Can be 'exact' or 'contains'.
#' @param patient_id_col Name of the patient_id column in data.
#' @param lab_date_col Name of the lab_date column in data.
#' @param lab_name_col Name of the lab_name column in data.
#' @param date_range Beginning and ending dates to look for.
#' @param cohort_type Type of cohort to find. One of:
#' - any.  Returns one row per patient matching the query.
#' - first.  Returns the first time a patient had that lab.
#' - last.  Returns the last time the patient had that lab.
#' - all.  Returns all occurrences that match the query.

#' @param min_tests Minimum number of occurrences per patient to filter for.
#'
#' @returns a tibble whose shape depends on cohort_type
#' @export
#'
#' @importFrom dplyr tibble n everything group_by summarise filter arrange slice_head ungroup left_join count select everything
#' @importFrom stringr str_detect fixed
#' @importFrom rlang sym
labs <- function(data,
                 markers,
                 match_type = c("exact", "contains"),
                 patient_id_col = "patient_id",
                 lab_date_col = "lab_date",
                 lab_name_col = "lab_name",
                 date_range = NULL,        # NULL or c(start, end) (Date or string coercible to Date)
                 cohort_type = c("any", "first", "last", "all"),
                 min_tests = 1) {

  match_type <- match.arg(match_type)
  cohort_type <- match.arg(cohort_type)

  if (missing(markers) || length(markers) == 0) {
    stop("`markers` must be a character vector with at least one pattern/name.")
  }
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")

  required_cols <- c(patient_id_col, lab_date_col, lab_name_col)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in `data`: ", paste(missing_cols, collapse = ", "))
  }

  # Coerce date column to Date.
  data[[lab_date_col]] <- as.Date(data[[lab_date_col]])
  if (all(is.na(data[[lab_date_col]]))) {
    warning("All values in lab_date_col are NA after coercion to Date.")
  }

  # Apply date range if provided.
  if (!is.null(date_range)) {
    if (length(date_range) != 2) stop("`date_range` must be length 2: c(start, end).")
    start <- as.Date(date_range[1])
    end <- as.Date(date_range[2])
    if (is.na(start) || is.na(end)) stop("`date_range` values must be coercible to Date.")
    data <- data[!is.na(data[[lab_date_col]]) & data[[lab_date_col]] >= start & data[[lab_date_col]] <= end, , drop = FALSE]
    if (nrow(data) == 0) {
      if (cohort_type == "any") {
        return(dplyr::tibble(!!patient_id_col := character(), first_lab_date = as.Date(character()), last_lab_date = as.Date(character()), n_tests = integer()))
      } else if (cohort_type %in% c("first", "last")) {
        return(dplyr::tibble(!!patient_id_col := character(), !!lab_date_col := as.Date(character()), !!lab_name_col := character(), n_tests = integer()))
      } else {
        return(dplyr::tibble(!!patient_id_col := character(), !!lab_date_col := as.Date(character()), !!lab_name_col := character()))
      }
    }
  }

  # Match markers (case-insensitive).
  lab_vec <- as.character(data[[lab_name_col]])
  matches <- rep(FALSE, length(lab_vec))

  if (match_type == "exact") {
    # case-insensitive exact match: compare lowercased values
    lab_lower <- tolower(lab_vec)
    markers_set <- unique(tolower(as.character(markers)))
    matches <- lab_lower %in% markers_set
  } else if (match_type == "contains") {
    # Literal, case-insensitive substring matching without regex.
    # Use stringr::fixed to avoid regex interpretation and set ignore_case = TRUE.
    for (m in as.character(markers)) {
      matches <- matches | stringr::str_detect(lab_vec, stringr::fixed(m, ignore_case = TRUE))
    }
  }

  data_matched <- data[matches, , drop = FALSE]
  if (nrow(data_matched) == 0) {
    if (cohort_type == "any") {
      return(dplyr::tibble(!!patient_id_col := character(), first_lab_date = as.Date(character()), last_lab_date = as.Date(character()), n_tests = integer()))
    } else if (cohort_type %in% c("first", "last")) {
      return(dplyr::tibble(!!patient_id_col := character(), !!lab_date_col := as.Date(character()), !!lab_name_col := character(), n_tests = integer()))
    } else {
      return(dplyr::tibble(!!patient_id_col := character(), !!lab_date_col := as.Date(character()), !!lab_name_col := character()))
    }
  }

  # Use dplyr and rlang for grouping and selection.
  pid <- rlang::sym(patient_id_col)
  ldate <- rlang::sym(lab_date_col)
  lname <- rlang::sym(lab_name_col)

  if (cohort_type == "any") {
    result <- data_matched %>%
      dplyr::group_by(!!pid) %>%
      dplyr::summarise(
        first_lab_date = min(!!ldate, na.rm = TRUE),
        last_lab_date = max(!!ldate, na.rm = TRUE),
        n_tests = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(n_tests >= min_tests)
    return(result)
  }

  if (cohort_type == "first") {
    counts <- data_matched %>% dplyr::count(!!pid, name = "n_tests")
    result <- data_matched %>%
      dplyr::group_by(!!pid) %>%
      dplyr::arrange(!!ldate, .by_group = TRUE) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(counts, by = setNames("n", patient_id_col)) %>%
      dplyr::filter(n_tests >= min_tests) %>%
      dplyr::select(!!pid, !!ldate, !!lname, n_tests)
    return(result)
  }

  if (cohort_type == "last") {
    counts <- data_matched %>% dplyr::count(!!pid, name = "n_tests")
    result <- data_matched %>%
      dplyr::group_by(!!pid) %>%
      dplyr::arrange(desc(!!ldate), .by_group = TRUE) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(counts, by = setNames("n", patient_id_col)) %>%
      dplyr::filter(n_tests >= min_tests) %>%
      dplyr::select(!!pid, !!ldate, !!lname, n_tests)
    return(result)
  }

  # cohort_type == "all"
  if (cohort_type == "all") {
    result <- data_matched %>% dplyr::arrange(!!pid, !!ldate)
    if (min_tests > 1) {
      counts <- data_matched %>% dplyr::count(!!pid, name = "n_tests")
      result <- result %>% dplyr::left_join(counts, by = setNames("n", patient_id_col)) %>% dplyr::filter(n_tests >= min_tests)
    }
    return(result %>% dplyr::select(!!pid, !!ldate, !!lname, dplyr::everything()))
  }

}
