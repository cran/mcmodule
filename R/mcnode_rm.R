#' Replace NA and Infinite Values in mcnode Objects
#'
#' Replaces NA and infinite values in mcnode objects with a specified value.
#'
#' @param mcnode An mcnode object containing NA or infinite values
#' @param na_value Numeric value to replace NA and infinite values (default = 0)
#'
#' @return An mcnode object with NA and infinite values replaced by na_value
#'
#' @examples
#' sample_mcnode <- mcstoc(runif,
#'                min = mcdata(c(NA, 0.2, -Inf), type = "0", nvariates = 3),
#'                max = mcdata(c(NA, 0.3, Inf), type = "0", nvariates = 3),
#'                nvariates = 3
#')
#' # Replace NA and Inf with 0
#' clean_mcnode <- mcnode_na_rm(sample_mcnode)
#'
#' @export
mcnode_na_rm <- function(mcnode, na_value = 0) {
  replace(mcnode, is.na(mcnode) | is.infinite(mcnode), na_value)
}

#' Replace NULL mcnode object
#'
#' Replaces an mcnode that is not found in the data or in the previous module with a specified value.
#'
#' @param mcnode An mcnode object containing NA or infinite values
#' @param null_value Numeric value to replace NA and infinite values (default = 0)
#'
#' @return The mcnode if is found, otherwise the null_value
#'
#' @examples
#' mcnode_null_rm(unexisting_mcnode)
#'
#' @export
mcnode_null_rm <- function(mcnode, null_value = 0) {
  if (missing(mcnode)) {
    return(null_value)
  } else if (!exists(deparse(substitute(mcnode)), envir = parent.frame())) {
    return(null_value)
  } else if (is.null(mcnode)) {
    return(null_value)
  } else {
    return(mcnode)
  }
}
