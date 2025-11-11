#' Breast Cancer Diagnosis Concept Set.
#'
#' A data frame of diagnosis codes used to identify breast cancer in EHRs.
#'
#' @format A data frame with 67 rows and the following columns:
#' \describe{
#'   \item{code}{character. code string.}
#'   \item{code_system}{character. vocabulary name (e.g., "icd10").}
#'   \item{include}{logical. T for included codes, F for explicit exclusions.}
#' }
#' @source Created for the electronco package.
#' @examples
#' head(bc_diag_concept)
#' @name bc_diag_concept
#' @docType data
NULL
