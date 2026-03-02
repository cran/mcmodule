.pkgglobalenv <- new.env(parent = emptyenv())

#' Set or Get Monte Carlo Inputs Table
#'
#' Manages the global mctable (Monte Carlo nodes reference table) by setting
#' or retrieving its value. The table stores metadata about mcnodes including
#' descriptions, functions, and sensitivity analysis parameters.
#'
#' @param data (data frame, optional). mctable with at minimum an `mcnode` column.
#'   Other columns auto-filled if absent. If NULL, returns the current table.
#'   Default: NULL.
#'
#' @return Current or newly set mctable. Columns include: mcnode (required),
#'   description, mc_func, from_variable, transformation, sensi_baseline,
#'   sensi_variation.
#'
#' @examples
#' # Get current MC table
#' current_table <- set_mctable()
#'
#' # Set new MC table
#' mct <- data.frame(
#'   mcnode = c("h_prev", "w_prev"),
#'   description = c("Herd prevalence", "Within herd prevalence"),
#'   mc_func = c("runif", "runif")
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
        sensi_baseline = character(),
        sensi_variation = character()
      ),
      envir = .pkgglobalenv
    )
  }

  # Get current mctable
  mct <- get("mctable", envir = .pkgglobalenv)

  # If data provided, perform checks and auto-fill
  if (!is.null(data)) {
    data_name <- deparse(substitute(data))
    data <- check_mctable(data)
    assign("mctable", data, envir = .pkgglobalenv)
    message("mctable set to ", data_name)
  } else {
    # Return current mctable
    return(get("mctable", envir = .pkgglobalenv))
  }
}

#' Reset Monte Carlo Inputs Table
#'
#' Clears and resets the global mctable to an empty state with standard columns.
#'
#' @return Empty data frame with standard mctable columns.
#'
#' @export

reset_mctable <- function() {
  empty_mctable <- data.frame(
    mcnode = character(),
    description = character(),
    mc_func = character(),
    from_variable = character(),
    transformation = character(),
    sensi_baseline = character(),
    sensi_variation = character()
  )
  assign("mctable", empty_mctable, envir = .pkgglobalenv)
  return(get("mctable", envir = .pkgglobalenv))
}


#' Validate and Prepare mctable Data Frame
#'
#' @description
#' Validates that an mctable contains required columns (`mcnode`, `mc_func`),
#' issues warnings for missing columns, and auto-fills missing optional columns
#' with `NA`.
#'
#' @param data (data frame). mctable with `mcnode` column (required) and optionally
#'   `mc_func`, `description`, `from_variable`, `transformation`, `sensi_baseline`,
#'   and `sensi_variation`. Default: required.
#'
#' @details
#' If `mc_func` is missing, all nodes are treated as deterministic (no uncertainty).
#' Optional columns are auto-filled with `NA` if absent.
#'
#' @return The validated `data` frame with all standard mctable columns present,
#'   with missing optional columns filled as `NA`.
check_mctable <- function(data) {
  # If data provided, perform checks and auto-fill
  if (is.data.frame(data)) {
    # Check if mcnode column exists
    if (!"mcnode" %in% colnames(data)) {
      stop("mcnode column not found in the mctable")
    }
    # Warn if mc_func column is missing
    if (!"mc_func" %in% colnames(data)) {
      warning(
        "No mc_func column found in the mctable. All nodes will be treated as deterministic (no uncertainty)."
      )
    }

    # Check columns and auto-fill with NA if missing
    cols <- c(
      "mc_func",
      "description",
      "from_variable",
      "transformation",
      "sensi_baseline",
      "sensi_variation"
    )

    missing_cols <- cols[!cols %in% colnames(data)]

    data[missing_cols] <- NA

    return(data)
  } else {
    stop("Data must be a data frame")
  }
}
