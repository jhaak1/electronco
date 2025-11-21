#' Produce Composite Phenotype Flags
#'
#' @param spec A list of id, version, date_range, rule, and min_count.
#' @param meds_tbl The output of meds() or a vector of medications to filter for.
#' @param proc_tbl The output of proc() or a vector of procedures to filter for.
#' @param labs_tbl The output of labs() or a vector of labs to filter for.
#' @param dx_tbl The output of diagnosis() or a vector of diagnoses to look for.
#' If using the output of diagnosis(), be sure to specify the patient_level list
#' (e.g. dx_tbl = diagnosis_output$patient_level).
#' @param return_evidence Logical; if TRUE include evidence_sample list-column in the result.
#'
#' @export
#'
#' @return A tibble with columns: patient_id, flag, flag_date, evidence_count, evidence_sample, spec_id, spec_version.
#' @importFrom dplyr filter mutate select rename arrange group_by summarise left_join slice_head slice_min n cur_data_all coalesce
#' @importFrom purrr map map_lgl map_int map_dbl map_chr pmap reduce compact
#' @importFrom rlang sym expr `!!`
#' @importFrom lubridate as_date
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @importFrom stats na.omit
#' @importFrom utils head
pheno <- function(spec,
                  meds_tbl = NULL,    # meds() output rows (cohort_flag/drug_matched/date) or raw meds events
                  proc_tbl = NULL,    # proc() output list(patient_level) or raw proc events
                  labs_tbl = NULL,    # labs() patient_level or raw lab events
                  dx_tbl = NULL,      # diagnosis() output list(patient_level) or raw dx events
                  return_evidence = TRUE) {

  # Minimal validation and spec fields used
  if (!is.list(spec)) stop("spec must be a named list")
  spec_id      <- spec$id %||% "unnamed"
  spec_version <- spec$version %||% "0.0"

  # Optional date window (if you want to constrain by date for all sources)
  window <- NULL
  if (!is.null(spec$date_range)) {
    if (length(spec$date_range) != 2) stop("spec$date_range must be length-2: c(start, end)")
    window <- list(start = as.Date(spec$date_range[1]), end = as.Date(spec$date_range[2]))
  }

  rule <- spec$rule %||% "any"
  if (!rule %in% c("any", "all")) stop("spec$rule must be 'any' or 'all'")
  global_min <- as.integer(spec$min_count %||% 1)

  # Helpers embedded (small, resilient)
  in_window <- function(df, date_col = "date", window = NULL) {
    if (is.null(window) || !date_col %in% names(df)) return(df)
    df %>% dplyr::filter(!!rlang::sym(date_col) >= window$start & !!rlang::sym(date_col) <= window$end)
  }

  # Aggregate event rows to patient-level summary
  agg_events <- function(df, patient_col = "patient_id", date_col = "date", min_count = 1, window = NULL) {
    if (is.null(df) || !is.data.frame(df)) {
      return(tibble::tibble(
        patient_id = character(0),
        flag = logical(0),
        first_date = as.Date(character(0)),
        count = integer(0),
        evidence_sample = list()
      ))
    }

    df <- in_window(df, date_col = date_col, window = window)

    # ensure patient id column present and normalized
    if (!patient_col %in% names(df)) {
      pid_guess <- intersect(c("patient_id","person_id","subject_id"), names(df))[1]
      if (!is.null(pid_guess)) df <- df %>% dplyr::rename(!!patient_col := !!rlang::sym(pid_guess))
    }
    if (!date_col %in% names(df)) {
      return(tibble::tibble(
        patient_id = character(0),
        flag = logical(0),
        first_date = as.Date(character(0)),
        count = integer(0),
        evidence_sample = list()
      ))
    }

    # split into groups (returns a list of tibbles)
    groups <- df %>% dplyr::group_by(!!rlang::sym(patient_col)) %>% dplyr::group_split(.keep = TRUE)
    # compute summary per group
    out <- purrr::map_dfr(groups, function(g) {
      pid <- as.character(g[[patient_col]][1])
      # ensure date column is Date
      g[[date_col]] <- as.Date(g[[date_col]])
      cnt <- nrow(g)
      first_dt <- if (cnt == 0) as.Date(NA) else min(g[[date_col]], na.rm = TRUE)
      sample_rows <- if (cnt == 0) tibble::tibble() else head(g, 5)
      tibble::tibble(
        patient_id = pid,
        flag = cnt >= min_count,
        first_date = first_dt,
        count = as.integer(cnt),
        evidence_sample = list(sample_rows)
      )
    })

    # ensure class and return
    out
  }

  # Convert aggregated patient_level data (cohort outputs) to standard summary
  agg_from_patient_level <- function(df, first_col = NULL, count_col = NULL, min_count = 1, window = NULL) {
    if (is.null(df) || !is.data.frame(df)) {
      return(tibble::tibble(
        patient_id = character(0),
        flag = logical(0),
        first_date = as.Date(character(0)),
        count = integer(0),
        evidence_sample = list()
      ))
    }

    # Normalize patient id column
    if (!"patient_id" %in% names(df)) {
      pid_guess <- intersect(c("patient_id","person_id","subject_id"), names(df))[1]
      if (!is.null(pid_guess)) df <- df %>% dplyr::rename(patient_id = !!rlang::sym(pid_guess))
    }
    if (!"patient_id" %in% names(df)) {
      return(tibble::tibble(
        patient_id = character(0),
        flag = logical(0),
        first_date = as.Date(character(0)),
        count = integer(0),
        evidence_sample = list()
      ))
    }

    # Prepare per-row helper columns ._first and ._count where possible
    df2 <- df
    if (!is.null(first_col) && first_col %in% names(df2)) {
      df2[["_first"]] <- as.Date(df2[[first_col]])
    } else if ("first_date" %in% names(df2)) {
      df2[["_first"]] <- as.Date(df2[["first_date"]])
    } else if ("cohort_start" %in% names(df2)) {
      df2[["_first"]] <- as.Date(df2[["cohort_start"]])
    } else {
      # leave ._first NA if not available
      df2[["_first"]] <- as.Date(NA)
    }

    if (!is.null(count_col) && count_col %in% names(df2)) {
      df2[["_count"]] <- as.integer(df2[[count_col]])
    } else if ("cohort_count" %in% names(df2)) {
      df2[["_count"]] <- as.integer(df2[["cohort_count"]])
    } else if ("n_total" %in% names(df2)) {
      df2[["_count"]] <- as.integer(df2[["n_total"]])
    } else {
      df2[["_count"]] <- 1L
    }

    # Apply window if provided (use _first or try a date-like column)
    date_for_window <- if ("_first" %in% names(df2)) "_first" else intersect(c("date","cohort_start","first_date"), names(df2))[1]
    if (!is.null(window) && !is.null(date_for_window) && date_for_window %in% names(df2)) {
      df2 <- df2 %>% dplyr::filter(as.Date(!!rlang::sym(date_for_window)) >= window$start & as.Date(!!rlang::sym(date_for_window)) <= window$end)
    }

    # Split by patient and summarize each group robustly
    groups <- df2 %>% dplyr::group_by(patient_id) %>% dplyr::group_split(.keep = TRUE)
    out <- purrr::map_dfr(groups, function(g) {
      pid <- as.character(g$patient_id[1])
      # total count is sum of ._count (fallback to number of rows if weird)
      cnt <- tryCatch(sum(as.integer(g[["_count"]]), na.rm = TRUE), error = function(e) nrow(g))
      # earliest first date from ._first (or NA if none)
      first_dt <- {
        if (all(is.na(g[["_first"]]))) as.Date(NA) else min(as.Date(g[["_first"]], origin = "1970-01-01"), na.rm = TRUE)
      }
      sample_rows <- if (nrow(g) == 0) tibble::tibble() else head(g, 5)
      tibble::tibble(
        patient_id = pid,
        flag = cnt >= min_count,
        first_date = first_dt,
        count = as.integer(cnt),
        evidence_sample = list(sample_rows)
      )
    })

    out
  }

  # Derive patient universe from provided helper outputs
  ids_from_df <- function(df) {
    if (is.null(df) || !is.data.frame(df)) return(character(0))
    idcol <- intersect(c("patient_id","person_id","subject_id"), names(df))[1]
    if (is.null(idcol)) return(character(0))
    as.character(na.omit(unique(df[[idcol]])))
  }
  ids <- unique(c(
    ids_from_df(meds_tbl),
    ids_from_df(proc_tbl),
    ids_from_df(labs_tbl),
    ids_from_df(dx_tbl),
    if (is.list(proc_tbl) && "patient_level" %in% names(proc_tbl) && is.data.frame(proc_tbl$patient_level)) as.character(unique(proc_tbl$patient_level$patient_id)) else character(0),
    if (is.list(dx_tbl) && "patient_level" %in% names(dx_tbl) && is.data.frame(dx_tbl$patient_level)) as.character(unique(dx_tbl$patient_level$patient_id)) else character(0)
  ))
  if (length(ids) == 0) stop("No patient ids found in any provided source.")

  # Build summaries directly from helper outputs (no spec-level filtering)
  summaries <- list()

  # meds_tbl: prefer meds() patient-level rows (cohort outputs) else aggregate event rows
  if (!is.null(meds_tbl)) {
    if (is.list(meds_tbl) && "patient_level" %in% names(meds_tbl) && is.data.frame(meds_tbl$patient_level)) {
      summaries$meds <- agg_from_patient_level(meds_tbl$patient_level, first_col = intersect(c("first_date","cohort_start","date"), names(meds_tbl$patient_level))[1], count_col = intersect(c("n_total","cohort_count"), names(meds_tbl$patient_level))[1], min_count = global_min, window = window)
    } else if (is.data.frame(meds_tbl)) {
      # guess date col names commonly used
      date_col <- intersect(c("date","order_date","order_date_parsed"), names(meds_tbl))[1] %||% "date"
      summaries$meds <- agg_events(meds_tbl, date_col = date_col, min_count = global_min, window = window)
    } else {
      summaries$meds <- tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list())
    }
  }

  # proc_tbl
  if (!is.null(proc_tbl)) {
    if (is.list(proc_tbl) && "patient_level" %in% names(proc_tbl) && is.data.frame(proc_tbl$patient_level)) {
      summaries$procs <- agg_from_patient_level(proc_tbl$patient_level, first_col = intersect(c("cohort_start","first_date","date"), names(proc_tbl$patient_level))[1], count_col = intersect(c("cohort_count","n_total"), names(proc_tbl$patient_level))[1], min_count = global_min, window = window)
    } else if (is.data.frame(proc_tbl)) {
      date_col <- intersect(c("date","proc_date"), names(proc_tbl))[1] %||% "date"
      summaries$procs <- agg_events(proc_tbl, date_col = date_col, min_count = global_min, window = window)
    } else {
      summaries$procs <- tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list())
    }
  }

  # labs_tbl
  if (!is.null(labs_tbl)) {
    # accept labs patient_level (first_lab_date, n_tests) or raw events
    if (is.data.frame(labs_tbl) && all(c("first_lab_date","n_tests") %in% names(labs_tbl))) {
      df <- labs_tbl %>% dplyr::rename(first_date = first_lab_date, count = n_tests)
      summaries$labs <- agg_from_patient_level(df, first_col = "first_date", count_col = "count", min_count = global_min, window = window)
    } else if (is.data.frame(labs_tbl)) {
      date_col <- intersect(c("lab_date","date"), names(labs_tbl))[1] %||% "lab_date"
      summaries$labs <- agg_events(labs_tbl, date_col = date_col, min_count = global_min, window = window)
    } else {
      summaries$labs <- tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list())
    }
  }

  # dx_tbl
  if (!is.null(dx_tbl)) {
    if (is.list(dx_tbl) && "patient_level" %in% names(dx_tbl) && is.data.frame(dx_tbl$patient_level)) {
      summaries$dx <- agg_from_patient_level(dx_tbl$patient_level, first_col = intersect(c("first_date","cohort_start","date"), names(dx_tbl$patient_level))[1], count_col = intersect(c("n_total","cohort_count"), names(dx_tbl$patient_level))[1], min_count = global_min, window = window)
    } else if (is.data.frame(dx_tbl)) {
      date_col <- intersect(c("date","diagnosis_date"), names(dx_tbl))[1] %||% "date"
      summaries$dx <- agg_events(dx_tbl, date_col = date_col, min_count = global_min, window = window)
    } else {
      summaries$dx <- tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list())
    }
  }

  # Ensure all requested sources exist in summaries (empty if absent)
  for (k in c("meds","procs","dx","labs")) {
    if (is.null(summaries[[k]])) summaries[[k]] <- tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list())
  }

  # Build wide table of patients and attach per-source fields
  base <- tibble::tibble(patient_id = ids)
  for (nm in names(summaries)) {
    s <- summaries[[nm]]
    if (nrow(s) == 0) {
      base <- base %>% dplyr::mutate(!!paste0(nm, "_flag") := FALSE,
                                     !!paste0(nm, "_date") := as.Date(NA),
                                     !!paste0(nm, "_count") := 0L,
                                     !!paste0(nm, "_evidence") := list(NULL))
    } else {
      s2 <- s %>% dplyr::rename(!!paste0("tmp_", nm, "_flag") := flag,
                                !!paste0("tmp_", nm, "_date") := first_date,
                                !!paste0("tmp_", nm, "_count") := count,
                                !!paste0("tmp_", nm, "_evidence") := evidence_sample)
      names(s2)[names(s2) == "patient_id"] <- "patient_id"
      base <- base %>% dplyr::left_join(s2, by = "patient_id") %>%
        dplyr::mutate(!!paste0(nm, "_flag") := coalesce(!!rlang::sym(paste0("tmp_", nm, "_flag")), FALSE),
                      !!paste0(nm, "_date") := !!rlang::sym(paste0("tmp_", nm, "_date")),
                      !!paste0(nm, "_count") := coalesce(!!rlang::sym(paste0("tmp_", nm, "_count")), 0L),
                      !!paste0(nm, "_evidence") := !!rlang::sym(paste0("tmp_", nm, "_evidence"))) %>%
        dplyr::select(-dplyr::matches(paste0("^tmp_", nm, "_")))
    }
  }

  # Apply composition rule across sources
  flag_cols <- grep("_flag$", names(base), value = TRUE)
  if (length(flag_cols) == 0) {
    flags <- rep(FALSE, nrow(base))
  } else {
    rows <- base %>% dplyr::select(all_of(flag_cols)) %>% purrr::pmap(function(...) list(...))
    flags <- purrr::map_lgl(rows, function(r) {
      vals <- unlist(r)
      if (length(vals) == 0) return(FALSE)
      if (rule == "any") any(vals) else all(vals)
    })
  }

  # Earliest contributing date
  rows_full <- base %>% purrr::pmap(function(...) list(...))
  earliest_dates_chr <- purrr::map_chr(rows_full, function(row) {
    date_cols <- grep("_date$", names(row), value = TRUE)
    dates <- as.Date(unlist(row[date_cols]))
    dates <- dates[!is.na(dates)]
    if (length(dates) == 0) NA_character_ else as.character(min(dates))
  })
  earliest_dates <- as.Date(earliest_dates_chr)

  # Evidence counts and samples
  get_count <- function(row) {
    ccols <- grep("_count$", names(row), value = TRUE)
    counts <- unlist(row[ccols])
    sum(as.integer(counts), na.rm = TRUE)
  }
  evidence_counts <- purrr::map_int(rows_full, get_count)

  get_sample <- function(row) {
    ecols <- grep("_evidence$", names(row), value = TRUE)
    samples <- row[ecols] %>% purrr::compact()
    if (length(samples) == 0) return(NULL)
    dplyr::bind_rows(!!!samples) %>% head(5L)
  }
  evidence_samples <- purrr::map(rows_full, get_sample)

  # Final result
  result <- tibble::tibble(
    patient_id = base$patient_id,
    flag = flags,
    flag_date = earliest_dates,
    evidence_count = evidence_counts,
    evidence_sample = evidence_samples,
    spec_id = spec_id,
    spec_version = spec_version
  )

  if (!isTRUE(return_evidence)) result <- result %>% dplyr::select(-evidence_sample)
  result
}
