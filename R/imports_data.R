#' Merged Import Data for Risk Assessment
#'
#' A dataset combining information about animal imports, pathogen prevalence, and test sensitivity across regions
#'
#' @format ## imports_data
#' A data frame with 6 rows and 12 columns:
#' \describe{
#'   \item{pathogen}{Pathogen identifier (a or b)}
#'   \item{origin}{Region of origin (nord, south, east)}
#'   \item{h_prev_min}{Minimum herd prevalence value}
#'   \item{h_prev_max}{Maximum herd prevalence value}
#'   \item{w_prev_min}{Minimum within-herd prevalence value}
#'   \item{w_prev_max}{Maximum within-herd prevalence value}
#'   \item{farms_n}{Number of farms exporting animals}
#'   \item{animals_n_mean}{Mean number of animals exported per farm}
#'   \item{animals_n_sd}{Standard deviation of animals exported per farm}
#'   \item{test_origin}{Test used to detect infected animals at origin}
#'   \item{test_sensi_min}{Minimum test sensitivity value}
#'   \item{test_sensi_mode}{Most likely test sensitivity value}
#'   \item{test_sensi_max}{Maximum test sensitivity value}
#' }
#'
#' @source Simulated data for demonstration purposes
"imports_data"
