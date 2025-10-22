#' Example Data Keys for Animal Imports Risk Assessment
#'
#' A hierarchical data structure containing test sensitivity, animal import, and regional
#' prevalence information, each with defined columns and keys.
#'
#' @format A list with three components:
#' \describe{
#'   \item{test_sensitivity}{List containing column names for test sensitivity data and "pathogen" as key}
#'   \item{animal_imports}{List containing column names for animal import data and "origin" as key}
#'   \item{prevalence_region}{List containing column names for prevalence data with "pathogen" and "origin" as keys}
#' }
#'
#' @source Simulated data for demonstration purposes
"imports_data_keys"
