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
#' representing a component module that was combined via [combine_modules()].
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

  # Fully recursive extractor for module names
  extract_module_names <- function(exp_list) {
    out <- character(0)

    if (!is.list(exp_list) || length(exp_list) == 0) {
      return(out)
    }

    for (i in seq_along(exp_list)) {
      elem <- exp_list[[i]]
      elem_name <- names(exp_list)[i]

      if (is.list(elem) && !is.expression(elem) && !is.language(elem)) {
        # If this child contains expressions (leaf module), record its name
        child_is_expr <- any(vapply(
          elem,
          function(x) is.expression(x) || is.language(x),
          logical(1)
        ))
        if (child_is_expr) {
          out <- c(out, elem_name)
        }

        # Recurse to find deeper modules
        out <- c(out, extract_module_names(elem))
      }
    }

    unique(out)
  }

  # Fully recursive extractor for expressions and their module names
  extract_module_exp <- function(exp_list, parent_module = NULL) {
    out <- list()

    if (!is.list(exp_list) || length(exp_list) == 0) {
      return(dplyr::bind_rows(out))
    }

    for (i in seq_along(exp_list)) {
      elem <- exp_list[[i]]
      elem_name <- names(exp_list)[i]

      if (is.list(elem) && !is.expression(elem) && !is.language(elem)) {
        # Recurse into nested lists to find expressions; parent_module is the module name
        nested <- extract_module_exp(elem, parent_module = elem_name)
        if (nrow(nested) > 0) {
          out[[length(out) + 1]] <- nested
        }
      } else if (is.expression(elem) || is.language(elem)) {
        out[[length(out) + 1]] <- data.frame(
          module = if (!is.null(parent_module)) parent_module else elem_name,
          exp = elem_name,
          stringsAsFactors = FALSE
        )
      }
    }

    dplyr::bind_rows(out)
  }

  # Combined modules have nested lists in exp
  has_nested <- any(vapply(
    mcmodule$exp,
    function(x) is.list(x) && !is.expression(x) && !is.language(x),
    logical(1)
  ))

  # Handle raw modules (top-level expressions) separately to preserve module name
  top_level_is_expr <- all(vapply(
    mcmodule$exp,
    function(x) is.expression(x) || is.language(x),
    logical(1)
  ))

  if (top_level_is_expr) {
    module_names <- deparse(substitute(mcmodule))
    n_modules <- 1L
    is_combined <- FALSE
    exp_names <- names(mcmodule$exp)
    module_exp <- data.frame(
      module = rep(module_names, length(exp_names)),
      exp = exp_names,
      stringsAsFactors = FALSE
    )
  } else {
    # Extract module/expressions recursively for combined modules
    module_exp <- extract_module_exp(mcmodule$exp)
    module_names <- unique(module_exp$module)
    n_modules <- length(module_names)
    is_combined <- n_modules > 1
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

  # Compute node counts per module and prev mcmodule traceability
  if (!is.null(mcmodule$node_list) && length(mcmodule$node_list) > 0) {
    node_df <- data.frame(
      name = names(mcmodule$node_list),
      exp = vapply(
        mcmodule$node_list,
        function(x) x[["exp_name"]] %||% NA_character_,
        character(1)
      ),
      type = vapply(
        mcmodule$node_list,
        function(x) x[["type"]] %||% NA_character_,
        character(1)
      ),
      stringsAsFactors = FALSE
    )

    node_df$module <- module_exp$module[match(node_df$exp, module_exp$exp)]

    # Replace NA modules with a fallback (raw module name)
    node_df$module[is.na(node_df$module)] <- deparse(substitute(mcmodule))

    # Pivot counts by module and type
    agg <- stats::aggregate(name ~ module, data = node_df, FUN = length)
    names(agg)[names(agg) == "name"] <- "n_nodes"

    types <- unique(node_df$type)

    # Build node_counts data.frame
    node_counts <- agg
    for (t in types) {
      counts <- as.integer(vapply(
        node_counts$module,
        function(m) sum(node_df$type[node_df$module == m] == t, na.rm = TRUE),
        integer(1)
      ))
      col_name <- paste0("n_", gsub("[^[:alnum:]]+", "_", t))
      node_counts[[col_name]] <- counts
    }
  } else {
    node_counts <- NULL
  }

  # Process global keys
  global_keys <- unique(unlist(lapply(names(mcmodule$node_list), function(x) {
    mcmodule$node_list[[x]][["keys"]]
  })))

  # Process data keys
  data_keys <- data.frame()

  # Only warn about missing data if nodes are NOT from the sample design.
  has_sample_design_nodes <- any(vapply(
    mcmodule$node_list,
    function(x) isTRUE(x[["from_sample_design"]]),
    logical(1)
  ))

  empty_data <- length(mcmodule$data) == 0 ||
    all(vapply(
      mcmodule$data,
      function(df) {
        if (is.data.frame(df)) {
          nrow(df) == 0L
        } else {
          TRUE
        }
      },
      logical(1)
    ))

  if (empty_data) {
    if (!has_sample_design_nodes) {
      warning(
        "No data frames found in mcmodule$data for any expressions in mcmodule$node_list"
      )
    }
    data_keys <- NULL
  } else {
    for (i in unique(data_name)[unique(data_name) %in% names(mcmodule$data)]) {
      data_i <- mcmodule$data[[i]][names(mcmodule$data[[i]]) %in% global_keys]
      data_i$variate <- seq_len(nrow(data_i))
      data_i$data_name <- i
      data_keys <- dplyr::bind_rows(data_keys, data_i)
    }
  }

  list(
    is_combined = is_combined,
    n_modules = n_modules,
    module_names = module_names,
    module_exp_data = module_exp,
    data_keys = data_keys,
    global_keys = global_keys,
    node_counts = node_counts
  ) -> out_list

  out_list
}
