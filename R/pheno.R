#' Produce Composite Phenotype Flags
#'
#' @param spec A list of id, version, date_range, meds, procs, dx, labs, rule, and min_count.
#' @param meds_tbl The output of meds() or a vector of medications to filter for.
#' @param proc_tbl The output of proc() or a vector of procedures to filter for.
#' @param labs_tbl The output of labs() or a vector of labs to filter for.
#' @param dx_tbl The output of diagnosis() or a vector of diagnoses to look for.
#' @param return_evidence
#'
#' @export
#'
#' @return A tibble with columns: patient_id, flag, flag_date, evidence_count, evidence_sample, spec_id, spec_version, spec_hash.
#' @importFrom dplyr filter mutate select rename arrange group_by summarise left_join slice_head slice_min n cur_data_all coalesce
#' @importFrom purrr map map_lgl map_int map_dbl map_chr pmap reduce compact
#' @importFrom rlang sym expr `!!`
#' @importFrom digest digest
#' @importFrom lubridate as_date
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
pheno <- function(spec,
                  meds_tbl = NULL,    # meds() output rows or raw meds events
                  proc_tbl = NULL,    # proc() output list(patient_level) or raw proc events
                  labs_tbl = NULL,    # labs() output shapes or raw lab events
                  dx_tbl = NULL,      # diagnosis() output list(patient_level,evidence) or raw dx events
                  return_evidence = TRUE) {

  # required packages: dplyr, purrr, rlang, lubridate, tidyr, digest
  if (!is.list(spec)) stop("spec must be a named list")
  spec_id <- spec$id %||% "unnamed"
  spec_version <- spec$version %||% "0.0"
  spec_hash <- digest::digest(spec)

  # date window (either date_range c(start,end) or anchor + within_days)
  window <- NULL
  if (!is.null(spec$date_range)) {
    if (length(spec$date_range) != 2) stop("spec$date_range must be length-2: c(start, end)")
    window <- list(start = as.Date(spec$date_range[1]), end = as.Date(spec$date_range[2]))
  } else if (!is.null(spec$anchor) && !is.null(spec$within_days)) {
    anchor <- as.Date(spec$anchor)
    window <- list(start = anchor - as.integer(spec$within_days), end = anchor + as.integer(spec$within_days))
  }

  rule <- spec$rule %||% "any"
  if (!rule %in% c("any","all")) stop("spec$rule must be 'any' or 'all'")
  global_min <- as.integer(spec$min_count %||% 1)

  # ---------- embedded helpers ----------
  in_window <- function(df, date_col = "date", window = NULL) {
    if (is.null(window) || !date_col %in% names(df)) return(df)
    df %>% dplyr::filter(!!rlang::sym(date_col) >= window$start & !!rlang::sym(date_col) <= window$end)
  }

  agg_events <- function(df, patient_col = "patient_id", date_col = "date", filter_expr = NULL, min_count = 1, window = NULL) {
    if (is.null(df)) return(tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list()))
    if (!is.null(filter_expr)) df <- df %>% dplyr::filter(!!filter_expr)
    df <- in_window(df, date_col = date_col, window = window)
    if (!patient_col %in% names(df)) {
      pid_guess <- intersect(c("patient_id","person_id","subject_id"), names(df))[1]
      if (!is.null(pid_guess)) df <- df %>% dplyr::rename(!!patient_col := !!rlang::sym(pid_guess))
    }
    if (!date_col %in% names(df)) return(tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list()))
    df %>%
      dplyr::group_by(!!rlang::sym(patient_col)) %>%
      dplyr::summarise(
        count = dplyr::n(),
        first_date = min(!!rlang::sym(date_col), na.rm = TRUE),
        evidence_sample = list(head(dplyr::cur_data_all(), 5L)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(flag = count >= min_count) %>%
      dplyr::rename(patient_id = !!rlang::sym(patient_col)) %>%
      dplyr::select(patient_id, flag, first_date, count, evidence_sample)
  }

  agg_aggregated <- function(df, patient_col = "patient_id", first_col = NULL, count_col = NULL, filter_expr = NULL, min_count = 1, window = NULL) {
    if (is.null(df) || !is.data.frame(df)) return(tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list()))
    if (!is.null(filter_expr)) df <- df %>% dplyr::filter(!!filter_expr)
    if (!patient_col %in% names(df)) {
      pid_guess <- intersect(c("patient_id","person_id"), names(df))[1]
      if (!is.null(pid_guess)) df <- df %>% dplyr::rename(!!patient_col := !!rlang::sym(pid_guess))
    }
    if (!is.null(first_col) && first_col %in% names(df)) {
      df2 <- df %>%
        dplyr::mutate(._first = as.Date(!!rlang::sym(first_col)),
                      ._count = if (!is.null(count_col) && count_col %in% names(df)) as.integer(!!rlang::sym(count_col)) else 1L) %>%
        dplyr::group_by(!!rlang::sym(patient_col)) %>%
        dplyr::summarise(count = sum(._count, na.rm = TRUE),
                         first_date = min(._first, na.rm = TRUE),
                         evidence_sample = list(head(dplyr::cur_data_all(), 5L)),
                         .groups = "drop") %>%
        dplyr::mutate(flag = count >= min_count) %>%
        dplyr::rename(patient_id = !!rlang::sym(patient_col)) %>%
        dplyr::select(patient_id, flag, first_date, count, evidence_sample)
      return(df2)
    }
    agg_events(df, patient_col = patient_col, date_col = "date", filter_expr = filter_expr, min_count = min_count, window = window)
  }

  # ---------- collect patient ids from all inputs ----------
  ids_from_events <- function(df, id_cols = c("patient_id","person_id","subject_id")) {
    if (is.null(df) || !is.data.frame(df)) return(character(0))
    col <- intersect(id_cols, names(df))[1]
    if (is.null(col)) return(character(0))
    as.character(na.omit(unique(df[[col]])))
  }
  ids <- character(0)
  ids <- c(ids, ids_from_events(meds_tbl), ids_from_events(labs_tbl), ids_from_events(proc_tbl), ids_from_events(dx_tbl))
  # also accept aggregated patient_level outputs
  if (is.list(proc_tbl) && "patient_level" %in% names(proc_tbl) && is.data.frame(proc_tbl$patient_level)) ids <- c(ids, as.character(unique(proc_tbl$patient_level$patient_id)))
  if (is.list(dx_tbl) && "patient_level" %in% names(dx_tbl) && is.data.frame(dx_tbl$patient_level)) ids <- c(ids, as.character(unique(dx_tbl$patient_level$patient_id)))
  if (is.data.frame(labs_tbl) && "patient_id" %in% names(labs_tbl)) ids <- c(ids, as.character(unique(labs_tbl$patient_id)))
  ids <- unique(ids)
  if (length(ids) == 0) stop("No patient ids found in any provided source (meds_tbl, proc_tbl, labs_tbl, dx_tbl).")

  # ---------- build per-source summaries based on known helper outputs ----------
  summaries <- list()

  # MEDS: spec$meds vector of normalized med names; meds_tbl may be meds() output rows (cohort_flag, drug_matched) or raw meds
  if (!is.null(spec$meds)) {
    meds_req <- tolower(trimws(as.character(spec$meds)))
    minc <- as.integer(spec$meds_min_count %||% global_min)
    if (!is.null(meds_tbl) && is.data.frame(meds_tbl) && all(c("cohort_flag","drug_matched") %in% names(meds_tbl))) {
      if (!"date" %in% names(meds_tbl)) {
        guess <- intersect(c("order_date","order_date_parsed","date"), names(meds_tbl))[1]
        if (!is.null(guess)) meds_tbl <- meds_tbl %>% dplyr::rename(date = !!rlang::sym(guess))
      }
      f <- expr(cohort_flag == TRUE & tolower(trimws(drug_matched)) %in% !!meds_req)
      summaries$meds <- agg_events(meds_tbl, date_col = "date", filter_expr = f, min_count = minc, window = window)
    } else if (!is.data.frame(meds_tbl) && is.null(meds_tbl)) {
      summaries$meds <- tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list())
    } else {
      # raw meds events
      med_col <- intersect(c("med_name","drug_name","drug"), names(meds_tbl))[1]
      if (!is.null(med_col)) {
        f <- expr(tolower(trimws(!!rlang::sym(med_col))) %in% !!meds_req)
        summaries$meds <- agg_events(meds_tbl, date_col = "date", filter_expr = f, min_count = minc, window = window)
      } else {
        summaries$meds <- tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list())
      }
    }
  }

  # PROCS: spec$procs vector of cohort names or codes; proc_tbl may be proc() output list(patient_level) or raw events
  if (!is.null(spec$procs)) {
    procs_req <- as.character(spec$procs)
    minc <- as.integer(spec$procs_min_count %||% global_min)
    if (is.list(proc_tbl) && "patient_level" %in% names(proc_tbl) && is.data.frame(proc_tbl$patient_level)) {
      cohort_df <- proc_tbl$patient_level
      extra <- if ("cohort_name" %in% names(cohort_df)) expr(cohort_name %in% !!procs_req) else NULL
      summaries$procs <- agg_aggregated(cohort_df, first_col = "cohort_start", count_col = "cohort_count", filter_expr = extra, min_count = minc, window = window)
    } else if (is.data.frame(proc_tbl)) {
      code_col <- intersect(c("code","cpt_code","proc_code"), names(proc_tbl))[1]
      if (!is.null(code_col)) {
        f <- expr(!!rlang::sym(code_col) %in% !!procs_req)
        summaries$procs <- agg_events(proc_tbl, date_col = "date", filter_expr = f, min_count = minc, window = window)
      } else {
        group_col <- intersect(c("group","cohort_name","proc_group"), names(proc_tbl))[1]
        if (!is.null(group_col)) {
          f <- expr(!!rlang::sym(group_col) %in% !!procs_req)
          summaries$procs <- agg_events(proc_tbl, date_col = "date", filter_expr = f, min_count = minc, window = window)
        } else {
          summaries$procs <- tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list())
        }
      }
    } else {
      summaries$procs <- tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list())
    }
  }

  # DX: spec$dx vector of diagnosis codes or cohort names; dx_tbl may be diagnosis() output list(patient_level) or raw events
  if (!is.null(spec$dx)) {
    dx_req <- as.character(spec$dx)
    minc <- as.integer(spec$dx_min_count %||% global_min)
    if (is.list(dx_tbl) && "patient_level" %in% names(dx_tbl) && is.data.frame(dx_tbl$patient_level)) {
      cohort_df <- dx_tbl$patient_level
      extra <- if ("cohort_name" %in% names(cohort_df)) expr(cohort_name %in% !!dx_req) else NULL
      first_col <- intersect(c("first_date","cohort_start"), names(cohort_df))[1]
      count_col <- intersect(c("n_total","cohort_count"), names(cohort_df))[1]
      summaries$dx <- agg_aggregated(cohort_df, first_col = first_col, count_col = count_col, filter_expr = extra, min_count = minc, window = window)
    } else if (is.data.frame(dx_tbl)) {
      code_col <- intersect(c("code","diagnosis_code"), names(dx_tbl))[1]
      if (!is.null(code_col)) {
        f <- expr(!!rlang::sym(code_col) %in% !!dx_req)
        summaries$dx <- agg_events(dx_tbl, date_col = "date", filter_expr = f, min_count = minc, window = window)
      } else {
        summaries$dx <- tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list())
      }
    } else {
      summaries$dx <- tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list())
    }
  }

  # LABS: spec$labs list of rules or a single rule; labs_tbl may be labs() output or raw events
  if (!is.null(spec$labs)) {
    labs_spec <- spec$labs
    if (!is.list(labs_spec) || (is.list(labs_spec) && !is.list(labs_spec[[1]]))) labs_spec <- list(labs_spec)
    lab_preds <- purrr::map(labs_spec, function(rule) {
      test_name <- rule$test %||% rule$lab %||% NULL
      comparator <- rule$comparator %||% ">"
      threshold <- rule$threshold %||% NULL
      minc <- as.integer(rule$min_count %||% spec$labs_min_count %||% global_min)
      if (is.data.frame(labs_tbl) && all(c("first_lab_date","n_tests") %in% names(labs_tbl))) {
        df <- labs_tbl %>% dplyr::rename(first_date = first_lab_date, count = n_tests)
        agg_aggregated(df, first_col = "first_date", count_col = "count", min_count = minc, window = window)
      } else if (is.data.frame(labs_tbl) && all(c("lab_date","lab_name") %in% names(labs_tbl))) {
        df <- labs_tbl
        if (!is.null(test_name)) {
          name_col <- intersect(c("lab_name","lab_test","code","test"), names(df))[1]
          if (!is.null(name_col)) df <- df %>% dplyr::filter(tolower(trimws(!!rlang::sym(name_col))) == tolower(trimws(test_name)))
        }
        if (!is.null(threshold) && "value" %in% names(df)) {
          cmp <- switch(comparator, ">" = expr(value > !!threshold), ">=" = expr(value >= !!threshold),
                        "<" = expr(value < !!threshold), "<=" = expr(value <= !!threshold), "==" = expr(value == !!threshold),
                        stop("unsupported comparator"))
          df <- df %>% dplyr::filter(!!cmp)
        }
        agg_events(df, date_col = "lab_date", min_count = minc, window = window)
      } else {
        tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list())
      }
    })
    labs_comb <- dplyr::bind_rows(lab_preds) %>%
      dplyr::group_by(patient_id) %>%
      dplyr::summarise(count = sum(as.integer(count), na.rm = TRUE),
                       first_date = if (all(is.na(first_date))) as.Date(NA) else min(first_date, na.rm = TRUE),
                       evidence_sample = list(head(dplyr::bind_rows(!!!purrr::map(lab_preds, ~ .x$evidence_sample %||% list(NULL)) %>% purrr::compact()), 5L)),
                       .groups = "drop") %>%
      dplyr::mutate(flag = count >= as.integer(spec$labs_min_count %||% global_min))
    summaries$labs <- labs_comb
  }

  # Ensure requested sources get an entry (empty shape if not found)
  for (k in c("meds","procs","dx","labs")) {
    if (!is.null(spec[[k]]) && is.null(summaries[[k]])) {
      summaries[[k]] <- tibble::tibble(patient_id = character(0), flag = logical(0), first_date = as.Date(character(0)), count = integer(0), evidence_sample = list())
    }
  }

  # ---------- assemble wide table of patients with source columns ----------
  base <- tibble::tibble(patient_id = ids)
  for (nm in names(summaries)) {
    s <- summaries[[nm]]
    if (nrow(s) == 0) {
      base <- base %>%
        dplyr::mutate(!!paste0(nm, "_flag") := FALSE,
                      !!paste0(nm, "_date") := as.Date(NA),
                      !!paste0(nm, "_count") := 0,
                      !!paste0(nm, "_evidence") := list(NULL))
    } else {
      s2 <- s %>% dplyr::rename(!!paste0("tmp_", nm, "_flag") := flag,
                                !!paste0("tmp_", nm, "_date") := first_date,
                                !!paste0("tmp_", nm, "_count") := count,
                                !!paste0("tmp_", nm, "_evidence") := evidence_sample)
      names(s2)[names(s2) == "patient_id"] <- "patient_id"
      base <- base %>%
        dplyr::left_join(s2, by = "patient_id") %>%
        dplyr::mutate(!!paste0(nm, "_flag") := coalesce(!!rlang::sym(paste0("tmp_", nm, "_flag")), FALSE),
                      !!paste0(nm, "_date") := !!rlang::sym(paste0("tmp_", nm, "_date")),
                      !!paste0(nm, "_count") := coalesce(!!rlang::sym(paste0("tmp_", nm, "_count")), 0L),
                      !!paste0(nm, "_evidence") := !!rlang::sym(paste0("tmp_", nm, "_evidence"))) %>%
        dplyr::select(-dplyr::matches(paste0("^tmp_", nm, "_")))
    }
  }

  # Evaluate composition rule
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

  # Earliest contributing date, evidence_count, evidence_sample
  get_earliest <- function(row) {
    dcols <- grep("_date$", names(row), value = TRUE)
    dates <- unlist(row[dcols])
    dates <- as.Date(dates)
    dates <- dates[!is.na(dates)]
    if (length(dates) == 0) NA_Date_ else min(dates)
  }
  rows_full <- base %>% purrr::pmap(function(...) list(...))
  earliest_dates_chr <- purrr::map_chr(rows_full, function(row) {
    date_cols <- grep("_date$", names(row), value = TRUE)
    dates <- unlist(row[date_cols])
    dates <- as.Date(dates)
    dates <- dates[!is.na(dates)]
    if (length(dates) == 0) NA_character_ else as.character(min(dates))
  })
  earliest_dates <- as.Date(earliest_dates_chr)

  get_count <- function(row) {
    ccols <- grep("_count$", names(row), value = TRUE)
    counts <- unlist(row[ccols])
    sum(as.integer(counts), na.rm = TRUE)
  }
  evidence_counts <- purrr::map_int(rows_full, get_count)

  get_sample <- function(row) {
    ecols <- grep("_evidence$", names(row), value = TRUE)
    samples <- row[ecols]
    samples <- purrr::compact(samples)
    if (length(samples) == 0) return(NULL)
    dplyr::bind_rows(!!!samples) %>% head(5L)
  }
  evidence_samples <- purrr::map(rows_full, get_sample)

  result <- tibble::tibble(
    patient_id = base$patient_id,
    flag = flags,
    flag_date = earliest_dates,
    evidence_count = evidence_counts,
    evidence_sample = evidence_samples,
    spec_id = spec_id,
    spec_version = spec_version,
    spec_hash = spec_hash
  )

  if (!isTRUE(return_evidence)) result <- result %>% dplyr::select(-evidence_sample)
  result
}
