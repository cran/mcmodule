#' Example Monte Carlo Input Table for Import Risk Assessment
#'
#' A configured table of Monte Carlo nodes used for modeling import risk scenarios,
#' particularly focused on animal disease transmission pathways.
#'
#' @format ## imports_mctable
#' A data frame with 7 rows and 6 columns:
#' \describe{
#'   \item{mcnode}{Node identifier used in Monte Carlo simulations}
#'   \item{description}{Human-readable description of what the node represents}
#'   \item{mc_func}{R function used for random number generation (e.g., runif, rnorm, rpert)}
#'   \item{from_variable}{Dependency reference to other variables if applicable}
#'   \item{transformation}{Mathematical transformations applied to the node values}
#'   \item{sensi_analysis}{Logical flag indicating if node is included in sensitivity analysis}
#' }
#'
#' @source Simulated data for demonstration purposes
"imports_mctable"
