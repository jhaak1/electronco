#' Extract medication cohorts.
#'
#' @param data Dataframe or tibble of medication data.
#' @param drugs A vector of drugs to filter for. Options include:
#' 5-fluorouracil, ado-trastuzumab emtansine, Anastrazole, bevacizumab,
#' Capecitabine, Carboplatin, Cisplatin, Cyclophosphamide, Docetaxel,
#' Doxorubicin, Epirubicin, Everolimus, Exemestane, Letrozole, Oxaliplatin,
#' Paclitaxel, pembrolizumab, pertuzumab, Tamoxifen, Trastuzumab.
#' @param patient_id Name of the patient_id column in data.
#' @param order_date Name of the order_date column in data.
#' @param med_name Name of the med_name column in data.
#' @param route Name of the route column in data.
#' @param dose Name of the dose column in data.
#' @param route_filter Character vector of allowed routes (case-insensitive); NULL to skip.
#' @param date_range Date window c(start, end) where dates are in YYYY-MM-DD format (e.g. 2025-01-01).
#' @param first_only If TRUE keep only the earliest exposure per patient x drug.
#' @param inc_original_cols If TRUE include original input columns in the output; otherwise return a slim cohort table.
#'
#' @returns Tibble of matched rows with provenance columns: cohort_flag, drug_matched.
#' @export
#' @importFrom dplyr mutate filter arrange group_by slice_head ungroup select everything all_of rename
#' @importFrom lubridate ymd ymd_hms
#' @importFrom stringr str_to_lower str_trim
meds <- function(data,
                 drugs,
                 patient_id = "patient_id",
                 order_date = "order_date",
                 med_name = "med_name",
                 route = "route",
                 dose = "dose",
                 route_filter = NULL,
                 date_range = NULL,
                 first_only = FALSE,
                 inc_original_cols = FALSE) {
  # Defensive checks
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  req_cols <- c(patient_id, order_date, med_name)
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols) > 0) stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  if (!is.null(route_filter) && !route %in% names(data)) stop("`route` column not found in data, but route_filter provided.")
  if (!is.null(dose) && !dose %in% names(data)) stop("`dose` column not found in data but dose argument provided.")
  if (!is.null(date_range)) {
    if (!(length(date_range) == 2)) stop("`date_range` must be a length-2 vector (start, end).")
  }

  # Canonical allowed drug list (lowercase, trimmed)
  allowed_drugs <- c(
    "5-fluorouracil", "anastrazole", "capecitabine", "carboplatin",
    "cisplatin", "docetaxel", "doxorubicin", "letrozole", "oxaliplatin",
    "paclitaxel", "tamoxifen", "trastuzumab", "cyclophosphamide",
    "everolimus", "epirubicin", "exemestane", "ado-trastuzumab emtansine",
    "bevacizumab", "pembrolizumab", "pertuzumab"
  )

  # Validate user drugs
  if (missing(drugs) || length(drugs) == 0) stop("`drugs` must be a non-empty character vector.")
  user_drugs_norm <- as.character(unlist(drugs)) %>%
    stringr::str_to_lower() %>%
    stringr::str_trim()
  not_allowed <- setdiff(user_drugs_norm, allowed_drugs)
  if (length(not_allowed) > 0) {
    stop("Some requested drugs are not in the allowed list: ", paste(not_allowed, collapse = ", "))
  }

  # Prepare working data with helper columns (will be removed before return)
  df_work <- data %>%
    dplyr::mutate(
      !!rlang::sym(patient_id) := .data[[patient_id]],
      .order_date_parsed = {
        od <- .data[[order_date]]
        if (inherits(od, "Date") || inherits(od, "POSIXt")) {
          od
        } else {
          parsed <- suppressWarnings(lubridate::ymd(od))
          if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::ymd_hms(od))
          if (all(is.na(parsed))) parsed <- suppressWarnings(as.POSIXct(od, tz = "UTC"))
          parsed
        }
      },
      .med_name_norm = stringr::str_to_lower(stringr::str_trim(as.character(.data[[med_name]])))
    )

  # Optional date range filter
  if (!is.null(date_range)) {
    start <- lubridate::ymd(date_range[1])
    end   <- lubridate::ymd(date_range[2])
    if (is.na(start) || is.na(end)) stop("date_range values must be parseable as dates (YYYY-MM-DD recommended).")
    df_work <- df_work %>% dplyr::filter(.order_date_parsed >= start & .order_date_parsed <= end)
  }

  # Optional route filter (case-insensitive exact)
  if (!is.null(route_filter)) {
    if (!route %in% names(data)) stop("route column not present in data for filtering.")
    rf_norm <- stringr::str_to_lower(stringr::str_trim(route_filter))
    df_work <- df_work %>% dplyr::filter(stringr::str_to_lower(stringr::str_trim(.data[[route]])) %in% rf_norm)
  }

  # Exact normalized match against the validated user drugs
  df_matched <- df_work %>%
    dplyr::filter(.med_name_norm %in% user_drugs_norm) %>%
    dplyr::mutate(
      cohort_flag = TRUE,
      drug_matched = .med_name_norm
    )

  # If first_only is TRUE, keep earliest exposure per patient x drug_matched
  if (first_only) {
    df_matched <- df_matched %>%
      dplyr::arrange(.data[[patient_id]], .order_date_parsed) %>%
      dplyr::group_by(.data[[patient_id]], drug_matched) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup()
  }

  # Drop internal helper columns before preparing final output
  df_matched <- df_matched %>%
    dplyr::select(-dplyr::any_of(c(".order_date_parsed", ".med_name_norm")))

  # Prepare output: include provenance columns and optionally original columns only
  if (inc_original_cols) {
    out <- df_matched %>% dplyr::select(dplyr::everything(), cohort_flag, drug_matched)
  } else {
    out <- df_matched %>%
      dplyr::select(dplyr::all_of(patient_id), dplyr::all_of(order_date), drug_matched, cohort_flag)
  }

  tibble::as_tibble(out)
}
