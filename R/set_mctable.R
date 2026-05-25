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
#'   description, mc_func, from_variable, sample_space, transformation,
#'   sensi_variation.
#'
#' @details
#' mctable columns are interpreted as follows:
#' \itemize{
#'   \item `mcnode`: Name of the Monte Carlo node (required).
#'   \item `description`: Human-readable description of the node.
#'   \item `mc_func`: Distribution function used to create stochastic nodes
#'     (for example `runif`, `rpert`). If missing/`NA`, node is deterministic.
#'   \item `from_variable`: Source column name in `data` when different from
#'     `mcnode`.
#'   \item `sample_space`: Sampling definition used by [mctable_bounds()] and [mctable_sobol_matrices()].
#'     Supported formats include `c(...)` and named bounds such as
#'     `min = X, max = Y`.
#'   \item `transformation`: R expression applied using `value` as placeholder
#'     before node creation.
#'   \item `sensi_variation`: OAT variation expression using `value` placeholder
#'     in [eval_module()].
#' }
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
        sample_space = character(),
        transformation = character(),
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
    sample_space = character(),
    transformation = character(),
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
#'   `mc_func`, `description`, `from_variable`, `sample_space`, `transformation`, and
#'   `sensi_variation`. Default: required.
#'
#' @details
#' If `mc_func` is missing, all nodes are treated as deterministic (no uncertainty).
#' Optional columns are auto-filled with `NA` if absent. When `sample_space`
#' values are provided, they must use supported formats (`c(...)` or named
#' assignments such as `min = X, max = Y`).
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
      "sample_space",
      "transformation",
      "sensi_variation"
    )

    missing_cols <- cols[!cols %in% colnames(data)]

    data[missing_cols] <- NA

    # Validate sample_space format when provided
    sample_space_chr <- as.character(data$sample_space)
    has_sample_space <- !is.na(sample_space_chr) &
      nzchar(trimws(sample_space_chr))
    if (any(has_sample_space)) {
      valid_sample_space <-
        grepl("^c\\s*\\(", trimws(sample_space_chr)) |
        grepl("=", sample_space_chr)

      invalid_rows <- which(has_sample_space & !valid_sample_space)
      if (length(invalid_rows) > 0) {
        stop(sprintf(
          "Invalid sample_space format at row(s): %s. Use 'c(...)' or named assignments like 'min = X, max = Y'.",
          paste(invalid_rows, collapse = ", ")
        ))
      }
    }

    return(data)
  } else {
    stop("Data must be a data frame")
  }
}
