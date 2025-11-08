#' Retrieve full provenance for a packaged code list (ICD9, ICD10, or CPT).
#'
#' Reads VERSIONS.yaml (installed extdata) and returns the provenance
#' record for a single dataset as a named list. If the dataset was produced from
#' multiple sources, the returned list preserves the `sources` sub-list and
#' includes combination metadata when present.
#'
#' @param dataset Character. The dataset name to look up.  This can be one of three
#' possibilities: 'icd9', 'icd10', or 'cpt'.
#' @param file Character. Path to VERSIONS.yaml. Defaults to the installed package:
#'        system.file("extdata", "VERSIONS.yaml", package = "electronco").
#' @return Named list with the provenance record for `dataset`. If no match is
#'         found, invisible NULL is returned and a warning is issued.
#' @examples
#' codemetadata("icd10")
codemetadata <- function(dataset, file = system.file("extdata", "VERSIONS.yaml", package = "electronco")) {
  if (missing(dataset) || !nzchar(dataset)) stop("`dataset` must be supplied as a non-empty string.")
  if (!nzchar(file)) stop("VERSIONS.yaml not found in installed package. Supply `file` argument.")
  if (!file.exists(file)) stop("VERSIONS file does not exist: ", file)

  parsed <- tryCatch(
    yaml::read_yaml(file),
    error = function(e) stop("Failed to read VERSIONS.yaml: ", conditionMessage(e))
  )

  if (length(parsed) == 0) {
    warning("VERSIONS file contains no entries.")
    return(invisible(NULL))
  }

  # Support both named list (names -> dataset) and list-of-records.
  # Normalize to list of records.
  records <- parsed
  if (!is.list(records) || (is.list(records) && !is.list(records[[1]]))) {
    stop("Unexpected VERSIONS.yaml structure: expected a list of records.")
  }

  # Find record by dataset field or by list name if provided.
  match_idx <- which(vapply(records, function(rec) {
    !is.null(rec[["dataset"]]) && identical(as.character(rec[["dataset"]]), as.character(dataset))
  }, logical(1)))

  # If not found by dataset field, try matching by list names.
  if (length(match_idx) == 0 && !is.null(names(records))) {
    match_idx <- which(names(records) == dataset)
  }

  if (length(match_idx) == 0) {
    warning("No provenance record found for dataset: ", dataset)
    return(invisible(NULL))
  }

  # Pick first match.
  rec <- records[[match_idx[[1]]]]

  # Return the raw record but ensure consistent character conversion.
  normalize_field <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.atomic(x) && length(x) == 1) return(as.character(x))
    if (is.list(x)) return(lapply(x, normalize_field))
    x
  }

  rec <- lapply(rec, normalize_field)

  # If there is a 'sources' list, ensure each source has standardized fields.
  if (!is.null(rec[["sources"]]) && is.list(rec[["sources"]])) {
    rec[["sources"]] <- lapply(rec[["sources"]], function(s) {
      s <- lapply(s, normalize_field)
      # Ensure common fields exist.
      for (f in c("source_name", "source_url", "original_filename", "retrieved")) {
        if (is.null(s[[f]])) s[[f]] <- NA_character_
      }
      s
    })
  }

  # Add the file path used to locate the VERSIONS file.
  rec[["_versions_file"]] <- as.character(file)

  return(rec)
}
