#' Example mcmodule object containing Monte Carlo simulation results Animal Imports Risk Assessment
#'
#' A list containing simulation results for pathogen testing of animal imports
#' from different origins, including:
#' - Within-herd prevalence (w_prev)
#' - Test sensitivity (test_sensi)
#' - Test origin probability (test_origin)
#' - Infection probability (inf_a)
#' - False negative probability (false_neg_a)
#' - No test probability (no_test_a)
#' - Non-detection probability (no_detect_a)
#'
#' @format An mcmodule object with the following components:
#' \describe{
#'   \item{data}{Input data frame with 6 rows and 13 variables}
#'   \item{exp}{Model expressions for calculating probabilities}
#'   \item{node_list}{List of Monte Carlo nodes with simulation results}
#'   \item{modules}{Character vector of module names}
#' }
#'
#' @source Simulated data for demonstration purposes
"imports_mcmodule"
