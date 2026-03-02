#' Set or Get Global Data Keys
#'
#' Manages a global data model by setting or retrieving data keys.
#' The data model defines column names and their associated grouping keys for each data frame.
#'
#' @param data_keys (list, optional). List of data specifications. Each element is a named list with:
#'   \itemize{
#'     \item cols: Character vector of column names.
#'     \item keys: Character vector of key columns (subset of cols).
#'   }
#'   If NULL, returns the current global data model. Default: NULL.
#'
#' @return Current or newly set global data model (invisibly).
#'
#' @examples
#' print(imports_data_keys)
#' set_data_keys(imports_data_keys)
#'
#' @export
set_data_keys <- function(data_keys = NULL) {
  if (is.null(data_keys)) {
    if (!exists("data_keys", envir = .pkgglobalenv)) {
      empty_model <- list()
      assign("data_keys", empty_model, envir = .pkgglobalenv)
    }
    return(get("data_keys", envir = .pkgglobalenv))
  } else {
    # Validate data model structure
    if (!is.list(data_keys)) {
      stop("Data model must be a list")
    }

    # Check each element has required structure
    for (name in names(data_keys)) {
      element <- data_keys[[name]]
      if (is.null(element)) {
        next # Skip NULL elements (conditionally included datasets)
      }
      if (
        !is.list(element) ||
          !all(c("cols", "keys") %in% names(element)) ||
          !is.vector(element$cols) ||
          !is.vector(element$keys)
      ) {
        stop(
          "Each data model element must be a list with 'cols' (column names vector) and 'keys' (vector of key columns)"
        )
      }

      # Validate that all keys exist in cols
      if (!all(element$keys %in% element$cols)) {
        missing_keys <- element$keys[!element$keys %in% element$cols]
        stop(
          "Keys must be a subset of column names. Missing in cols: ",
          paste(missing_keys, collapse = ", "),
          " for dataset: ",
          name
        )
      }
    }

    # Assign validated data model
    assign("data_keys", data_keys, envir = .pkgglobalenv)
    message("data_keys set to ", deparse(substitute(data_keys)))
  }
}

#' Reset Data Keys
#'
#' @description
#' Reset Global Data Keys
#'
#' Clears and resets the global data keys to an empty state.
#'
#' @return NULL (invisibly). Clears global data_keys.
#'
#' @examples
#' reset_data_keys()
#'
#' @export
reset_data_keys <- function() {
  empty_model <- list()
  assign("data_keys", empty_model, envir = .pkgglobalenv)
  message("data_keys reset")
}
