#' Read VERSIONS.yaml and return a data frame of key fields.
#'
#' Returns a tibble with the following columns: dataset, data_version, and retrieved.
#' Looks for VERSIONS.yaml under inst/extdata of the installed package.
#'
#' @param file Path to VERSIONS.yaml. Defaults to system file in the installed package.
#' @return tibble with the following columns: dataset, data_version, and retrieved.
#' @examples
#' codeversions()
#' @export
#' @importFrom yaml read_yaml
#' @importFrom tibble as_tibble tibble
codeversions <- function(file = system.file("extdata", "VERSIONS.yaml", package = "electronco")) {
  if (!nzchar(file)) {
    stop("VERSIONS.yaml not found in installed package. Check installation or supply `file`.")
  }
  if (!file.exists(file)) {
    stop("VERSIONS file does not exist: ", file)
  }

  # Parse YAML.
  yaml_parsed <- tryCatch(
    yaml::read_yaml(file),
    error = function(e) stop("Failed to read YAML VERSIONS file: ", conditionMessage(e))
  )

  # Empty result: return consistent empty tibble.
  if (is.null(yaml_parsed) || length(yaml_parsed) == 0) {
    return(tibble::tibble(dataset = character(), data_version = character(), retrieved = character()))
  }

  # Expect a list of records.
  records <- yaml_parsed
  if (!is.list(records) || (is.list(records) && !is.list(records[[1]]))) {
    stop("Unexpected VERSIONS.yaml structure: expected a list of records.")
  }

  # Safe Field Extractor
  extract_field <- function(entry, field) {
    val <- entry[[field]]
    if (is.null(val)) return(NA_character_)
    as.character(val)
  }

  # Build rows as a list of named vectors (safe for binding).
  rows <- lapply(records, function(rec) {
    c(
      dataset = extract_field(rec, "dataset"),
      data_version = extract_field(rec, "data_version"),
      retrieved = extract_field(rec, "retrieved")
    )
  })

  # Bind into tibble robustly.
  df <- tryCatch({
    # Convert each named character vector to one-row tibble, then bind.
    do.call(rbind, lapply(rows, function(x) as.data.frame(as.list(x), stringsAsFactors = FALSE)))
  }, error = function(e) {
    stop("Failed to construct data frame from VERSIONS entries: ", conditionMessage(e))
  })

  df <- tibble::as_tibble(df)

  # Ensure columns exist and are character.
  for (col in c("dataset", "data_version", "retrieved")) {
    if (!(col %in% names(df))) df[[col]] <- NA_character_
    df[[col]] <- as.character(df[[col]])
  }

  # Trim whitespace and normalize NA.
  df[] <- lapply(df, function(x) {
    x <- trimws(as.character(x))
    x[x == "NA" | x == ""] <- NA_character_
    x
  })

  df
}
