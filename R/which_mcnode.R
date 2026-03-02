#' Find mcnodes Matching a Condition
#'
#' Applies a test function to each mcnode in an mcmodule and returns the
#' names of nodes where the test returns TRUE. Useful for identifying nodes
#' with specific properties (e.g., NA values, negative values).
#'
#' @param mcmodule (mcmodule object). Module containing node_list with mcnodes.
#' @param test_func (function). Function that takes an mcnode and returns logical;
#'   TRUE if the condition is met.
#'
#' @return Character vector of mcnode names where `test_func` returns TRUE.
#'   Empty vector if no nodes meet the condition.
#'
#' @examples
#' # Find nodes with negative values
#' which_mcnode(imports_mcmodule, function(x) any(x < 0, na.rm = TRUE))
#'
#' # Find nodes with values greater than 1
#' which_mcnode(imports_mcmodule, function(x) any(x > 1, na.rm = TRUE))
#'
#' @export
#'
#' @seealso [which_mcnode_na()], [which_mcnode_inf()]
which_mcnode <- function(mcmodule, test_func) {
  # Validate input
  if (!is.list(mcmodule) || is.null(mcmodule$node_list)) {
    stop("mcmodule must be a list with a node_list component")
  }

  if (!is.function(test_func)) {
    stop("test_func must be a function")
  }

  node_names <- names(mcmodule$node_list)

  # Handle empty node_list
  if (length(node_names) == 0) {
    return(character(0))
  }

  # Apply test function to each node
  test_results <- sapply(mcmodule$node_list, function(node) {
    if (is.null(node[["mcnode"]])) {
      return(FALSE)
    }
    tryCatch(
      {
        test_func(node[["mcnode"]])
      },
      error = function(e) {
        FALSE
      }
    )
  })

  # Return names of nodes where test is TRUE
  return(names(test_results[test_results == TRUE]))
}


#' Find `mcnode`s with Missing Values
#'
#' Find mcnodes with Missing Values
#'
#' Identifies which mcnodes within an mcmodule contain NA values.
#' Useful for troubleshooting and debugging Monte Carlo models.
#'
#' @param mcmodule (mcmodule object). Module containing node_list.
#'
#' @return Character vector of mcnode names containing NA values. Returns empty
#'   vector if no NAs found.
#'
#' @examples
#' # Find nodes with NAs in the imports_mcmodule
#' which_mcnode_na(imports_mcmodule)
#'
#' # Create a test mcmodule with NAs
#' test_mcnode_na <- mcdata(c(0.1, NA, 0.3), type = "0", nvariates = 3)
#' test_mcnode_clean <- mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3)
#' test_mcmodule <- list(
#'   node_list = list(
#'     node_a = list(mcnode = test_mcnode_na),
#'     node_b = list(mcnode = test_mcnode_clean)
#'   )
#' )
#' which_mcnode_na(test_mcmodule)
#'
#' @export
#'
#' @seealso [which_mcnode()], [which_mcnode_inf()], [mcnode_na_rm()]
which_mcnode_na <- function(mcmodule) {
  which_mcnode(mcmodule, function(x) any(is.na(x)))
}


#' Find mcnodes with Infinite Values
#'
#' Identifies which mcnodes within an mcmodule contain infinite values
#' (Inf or -Inf). Useful for troubleshooting and debugging Monte Carlo models.
#'
#' @param mcmodule (mcmodule object). Module containing node_list.
#'
#' @return Character vector of mcnode names containing infinite values. Returns
#'   empty vector if no infinite values found.
#'
#' @examples
#' # Find nodes with infinite values in the imports_mcmodule
#' which_mcnode_inf(imports_mcmodule)
#'
#' # Create a test mcmodule with Inf values
#' test_mcnode_inf <- mcdata(c(0.1, Inf, 0.3), type = "0", nvariates = 3)
#' test_mcnode_clean <- mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3)
#' test_mcmodule <- list(
#'   node_list = list(
#'     node_a = list(mcnode = test_mcnode_inf),
#'     node_b = list(mcnode = test_mcnode_clean)
#'   )
#' )
#' which_mcnode_inf(test_mcmodule)
#'
#' @export
#'
#' @seealso [which_mcnode()], [which_mcnode_na()], [mcnode_na_rm()]
which_mcnode_inf <- function(mcmodule) {
  which_mcnode(mcmodule, function(x) any(is.infinite(x)))
}
