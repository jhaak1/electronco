#' Read VERSIONS.yaml and return a data frame of key fields.
#'
#' Returns a tibble with the following columns: dataset, data_version, and retrieved.
#' Looks for VERSIONS.yaml under inst/extdata of the installed package.
#'
#' @param file Path to VERSIONS.yaml. Defaults to system file in the installed package.
#' @return tibble with the following columns: dataset, data_version, and retrieved.
#' @examples
#' codeversions()
#'
codeversions <- function(file = system.file("extdata", "VERSIONS.yaml", package = "electronco")) {
  # lazy-load required packages to avoid adding hard Depends
  if (!nzchar(file)) stop("VERSIONS.yaml not found in installed package. Check installation or supply `file`.")
  if (!file.exists(file)) stop("VERSIONS file does not exist: ", file)

  # Parse YAML.
  yaml_parsed <- tryCatch({
    yaml::read_yaml(file)
  }, error = function(e) {
    stop("Failed to read YAML VERSIONS file: ", conditionMessage(e))
  })

  # yaml::read_yaml will return a list of records; coerce to data frame safely.
  # Support either a named list or an unnamed list of records.
  records <- yaml_parsed
  if (is.null(records) || length(records) == 0) {
    return(tibble::tibble(dataset = character(), data_version = character(), retrieved = character()))
  }

  # Normalize into a list of lists.
  if (!is.list(records) || (is.list(records) && !is.list(records[[1]]))) {
    stop("Unexpected VERSIONS.yaml structure: expected a list of records.")
  }

  # Extract fields with safe lookup.
  extract_field <- function(entry, field) {
    val <- entry[[field]]
    if (is.null(val)) NA_character_ else as.character(val)
  }

  rows <- lapply(records, function(rec) {
    # If rec contains 'sources' (combined sources), prefer a top-level data_version and retrieve.
    list(
      dataset = extract_field(rec, "dataset"),
      data_version = extract_field(rec, "data_version"),
      retrieved = extract_field(rec, "retrieved")
    )
  })

  # Bind into tibble.
  df <- tryCatch({
    do.call(rbind, lapply(rows, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
  }, error = function(e) {
    stop("Failed to construct data frame from VERSIONS entries: ", conditionMessage(e))
  })

  # Convert to tibble and ensure columns exist.
  df <- tibble::as_tibble(df)
  # Ensure columns present.
  for (col in c("dataset", "data_version", "retrieved")) {
    if (!(col %in% names(df))) df[[col]] <- NA_character_
  }

  # Trim whitespace and return.
  df[] <- lapply(df, function(x) ifelse(is.na(x), NA_character_, trimws(as.character(x))))
  df
}
