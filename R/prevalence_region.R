#' Regional Prevalence Data
#'
#' A dataset containing prevalence information for two pathogens across three regions
#'
#' @format ## prevalence_region
#' A data frame with 6 rows and 4 columns:
#' \describe{
#'   \item{pathogen}{Pathogen identifier (a or b)}
#'   \item{origin}{Region of origin (nord, south, east)}
#'   \item{h_prev_min}{Minimum herd prevalence value}
#'   \item{h_prev_max}{Maximum herd prevalence value}
#'   \item{w_prev_min}{Minimum within-herd prevalence value}
#'   \item{w_prev_max}{Maximum within-herd prevalence value}
#'   \item{test_origin}{Test used to detect infected animals at origin}
#' }
#'
#' @source Simulated data for demonstration purposes
"prevalence_region"
