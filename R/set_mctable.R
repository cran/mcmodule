.pkgglobalenv <- new.env(parent = emptyenv())

#' Set or Get Monte Carlo Inputs Table
#'
#' @description
#' Manages a Monte Carlo inputs table in the global package environment by either setting new data
#' or retrieving the current table. The table stores information about Monte Carlo nodes including
#' their descriptions, functions, dependencies, and sensitivity analysis settings.
#'
#' @param data Optional data frame containing MC table information. Must contain an 'mcnode' column.
#'            Other columns will be auto-filled if missing. If NULL, returns the current MC table.
#'
#' @return
#'   \itemize{
#'     \item If data = NULL: Returns the current MC table
#'     \item If data provided: Sets the new MC table and returns invisibly
#'   }
#'   The table contains the following columns:
#'   \itemize{
#'     \item mcnode - Character. Name of the Monte Carlo node (required)
#'     \item description - Character. Description of the parameter
#'     \item mc_func - Character. Probability distribution
#'     \item from_variable - Character. Variable name in the data table, if it is in a column with a name different from the mcnode
#'     \item transformation - Character. Transformation to be applied to the original column values
#'     \item sensi_analysis - Logical. Whether to include in sensitivity analysis
#'   }
#'
#' @examples
#' # Get current MC table
#' current_table <- set_mctable()
#'
#' # Set new MC table
#' mct <- data.frame(
#'   mcnode = c("h_prev", "w_prev"),
#'   description = c("Herd prevalence", "Within herd prevalence"),
#'   mc_func = c("runif", "runif"),
#'   sensi_analysis = c(TRUE, TRUE)
#' )
#' set_mctable(mct)
#'
#' @export
set_mctable <- function(data = NULL) {
  # Check if mctable exists, if not create with default values
  if (!exists("mctable", envir = .pkgglobalenv)) {
    assign(
      "mctable",
      data.frame(
        mcnode = character(),
        description = character(),
        mc_func = character(),
        from_variable = character(),
        transformation = character(),
        sensi_analysis = logical()
      ),
      envir = .pkgglobalenv
    )
  }

  # Get current mctable
  mct <- get("mctable", envir = .pkgglobalenv)

  # If data provided, perform checks and auto-fill
  if (!is.null(data)) {
    data_name<-deparse(substitute(data))
    data <- check_mctable(data)
    assign("mctable", data, envir = .pkgglobalenv)
    message("mctable set to ", data_name)
  } else{
    # Return current mctable
    return(get("mctable", envir = .pkgglobalenv))
  }
}

#' Resets the Monte Carlo inputs table
#'
#' @return An empty data frame with the standard mctable structure
#'
#' @export

reset_mctable <- function() {
  empty_mctable <- data.frame(
    mcnode = character(),
    description = character(),
    mc_func = character(),
    from_variable = character(),
    transformation = character(),
    sensi_analysis = logical()
  )
  assign("mctable", empty_mctable, envir = .pkgglobalenv)
  return(get("mctable", envir = .pkgglobalenv))
}


#' Checks mctable data
#'
#' @param data A data frame containing MC table information. Must contain an 'mcnode' column.
#'
#' @return A data frame with the standard mctable structure

check_mctable <- function(data) {
  # If data provided, perform checks and auto-fill
  if (is.data.frame(data)) {
    # Check if mcnode column exists
    if (!"mcnode" %in% colnames(data)) {
      stop("mcnode column not found in the mctable")
    }
    # Check required columns and auto-fill if missing
    required_cols <- c("description",
                       "mc_func",
                       "from_variable",
                       "transformation",
                       "sensi_analysis")
    missing_cols <- required_cols[!required_cols %in% colnames(data)]
    if (length(missing_cols) > 0) {
      warning(
        paste(missing_cols, collapse = ", "), " not specified"
      )
      # Add missing columns with default values
      for (col in missing_cols) {
        if (col == "sensi_analysis") {
          data[[col]] <- FALSE
        } else {
          data[[col]] <- NA
        }
      }
    }
    return(data)
  } else {
    stop("Data must be a data frame")
  }
}
