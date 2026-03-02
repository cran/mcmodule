#' Get Comprehensive Monte Carlo Module Information
#'
#' @description
#' Extracts comprehensive metadata about a Monte Carlo module, including:
#' - Module composition (raw vs combined modules)
#' - Input data per expression
#' - Keys for each variate (data row)
#' - Global data keys
#'
#' @param mcmodule A Monte Carlo module object
#'
#' @return A list with six elements:
#'   \item{is_combined}{Logical. TRUE if module is combined, FALSE if raw}
#'   \item{n_modules}{Integer. Number of component modules (1 for raw, >1 for combined)}
#'   \item{module_names}{Character vector. Names of all component modules (recursive)}
#'   \item{module_exp_data}{Data frame with module and expression information, including data_name}
#'   \item{data_keys}{Data frame with keys for each variate, including variate number and data_name}
#'   \item{global_keys}{Character vector of global key names used across the module}
#'
#' @details
#' A raw module has a single expression in `mcmodule$exp`.
#' A combined module has multiple expressions in `mcmodule$exp`, each
#' representing a component module that was combined via `combine_modules()`.
#'
#' For combined modules, module names are recursively extracted up to one level deep.
#' This allows identifying all base modules even in deeply nested combinations.
#'
#' @examples
#' # Get comprehensive module information
#' info <- mcmodule_info(imports_mcmodule)
#' str(info)
#'
#' # Access composition information
#' info$is_combined
#' info$n_modules
#' info$module_names
#'
#' # Access index information
#' head(info$module_exp_data)
#' head(info$data_keys)
#' info$global_keys
#'
#' @export
mcmodule_info <- function(mcmodule) {
  # Validate input
  if (!inherits(mcmodule, "mcmodule")) {
    stop("Input must be an mcmodule object")
  }

  if (!("exp" %in% names(mcmodule)) || is.null(mcmodule$exp)) {
    stop("mcmodule does not contain an 'exp' element")
  }

  # Function to recursively extract module names up to one level
  extract_module_names <- function(exp_list, level = 0) {
    if (level > 1) {
      return(character(0))
    }

    names_list <- names(exp_list)

    module_names <- lapply(seq_along(exp_list), function(i) {
      elem <- exp_list[[i]]
      elem_name <- names_list[i]

      if (is.list(elem) && !is.expression(elem) && !is.language(elem)) {
        child_is_list <- vapply(
          elem,
          function(x) is.list(x) && !is.expression(x) && !is.language(x),
          logical(1)
        )

        if (any(child_is_list) && level < 1L) {
          nested_names <- extract_module_names(elem, level = level + 1L)

          if (length(nested_names) > 0) {
            return(nested_names)
          }
        }

        return(elem_name)
      }

      elem_name
    })

    unlist(module_names)
  }

  # Function to extract all expressions with their module names
  extract_module_exp <- function(exp_list, parent_module = NULL, level = 0) {
    if (level > 1) {
      return(data.frame(
        module = character(0),
        exp = character(0),
        stringsAsFactors = FALSE
      ))
    }

    names_list <- names(exp_list)

    result_list <- lapply(seq_along(exp_list), function(i) {
      elem <- exp_list[[i]]
      elem_name <- names_list[i]

      if (is.list(elem) && !is.expression(elem) && !is.language(elem)) {
        # Check if children are lists (nested modules)
        child_is_list <- vapply(
          elem,
          function(x) is.list(x) && !is.expression(x) && !is.language(x),
          logical(1)
        )

        if (any(child_is_list) && level < 1L) {
          # Recurse into nested modules
          extract_module_exp(
            elem,
            parent_module = elem_name,
            level = level + 1L
          )
        } else {
          # This is a module with expressions
          exp_names <- names(elem)
          data.frame(
            module = rep(elem_name, length(elem)),
            exp = exp_names,
            stringsAsFactors = FALSE
          )
        }
      } else if (is.expression(elem) || is.language(elem)) {
        # This is an expression
        data.frame(
          module = if (!is.null(parent_module)) parent_module else elem_name,
          exp = elem_name,
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          module = character(0),
          exp = character(0),
          stringsAsFactors = FALSE
        )
      }
    })

    dplyr::bind_rows(result_list)
  }

  # Combined modules have nested lists in exp
  has_nested <- any(vapply(
    mcmodule$exp,
    function(x) is.list(x) && !is.expression(x) && !is.language(x),
    logical(1)
  ))

  if (has_nested) {
    module_names <- extract_module_names(mcmodule$exp)
    n_modules <- length(module_names)
    is_combined <- n_modules > 1
    module_exp <- extract_module_exp(mcmodule$exp)
  } else {
    module_names <- deparse(substitute(mcmodule))
    n_modules <- 1L
    is_combined <- FALSE

    # For raw modules, extract expressions directly
    exp_names <- names(mcmodule$exp)
    module_exp <- data.frame(
      module = rep(module_names, length(exp_names)),
      exp = exp_names,
      stringsAsFactors = FALSE
    )
  }

  # Extract module expressions and metadata (index information)
  exp_name <- unlist(lapply(names(mcmodule$node_list), function(x) {
    mcmodule$node_list[[x]][["exp_name"]] %||% NA
  }))

  data_name <- unlist(lapply(
    names(mcmodule$node_list)[!is.na(exp_name)],
    function(x) {
      mcmodule$node_list[[x]][["data_name"]] %||% NA
    }
  ))

  names(data_name) <- exp_name[!is.na(exp_name)]

  #data_name <- data_name[!is.na(data_name) & !duplicated(data_name)]

  # Create a data frame to store module, expression and data_name
  module_exp$data_name <- data_name[module_exp$exp]

  # Process global keys
  global_keys <- unique(unlist(lapply(names(mcmodule$node_list), function(x) {
    mcmodule$node_list[[x]][["keys"]]
  })))

  # Process data keys
  data_keys <- data.frame()
  for (i in unique(data_name)[unique(data_name) %in% names(mcmodule$data)]) {
    data_i <- mcmodule$data[[i]][names(mcmodule$data[[i]]) %in% global_keys]
    data_i$variate <- seq_len(nrow(data_i))
    data_i$data_name <- i
    data_keys <- dplyr::bind_rows(data_keys, data_i)
  }

  list(
    is_combined = is_combined,
    n_modules = n_modules,
    module_names = module_names,
    module_exp_data = module_exp,
    data_keys = data_keys,
    global_keys = global_keys
  )
}
