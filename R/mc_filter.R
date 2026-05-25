#' Filter mcnode Variates by Condition
#'
#' Filters variates (data rows) from an mcnode based on logical conditions,
#' similar to [dplyr::filter()]. Can return a new node in the mcmodule or
#' return a filtered mcnode directly.
#'
#' @param mcmodule (mcmodule object, optional). Module containing the node.
#'   Default: NULL.
#' @param mc_name (character, optional). Name of the mcnode in the module.
#' @param ... (expression). Logical conditions to filter by; evaluated in context
#'   of the data associated with the mcnode.
#' @param data (data frame, optional). Input data frame. Default: NULL.
#' @param mcnode (mcnode object, optional). mcnode to filter directly. Default: NULL.
#' @param name (character, optional). Name for the new filtered node when adding to
#'   mcmodule. If NULL, auto-generated from `mc_name` and `suffix`.
#'   Default: NULL.
#' @param prefix (character, optional). Prefix for the auto-generated node name.
#'   Default: NULL.
#' @param suffix (character). Suffix appended to auto-generated name.
#'   Default: "filtered".
#' @param summary (logical). If TRUE, compute summary statistics for the new node.
#'   Default: TRUE.
#'
#' @details
#' Call signatures:
#' - To add filtered node to mcmodule: `mc_filter(mcmodule, "node", conditions, name = "new_name")`
#' - To return filtered mcnode only: `mc_filter(conditions, data = data, mcnode = mcnode)`
#'
#' Filter conditions work on variates (data rows); only rows meeting all conditions
#' are retained in the resulting mcnode.
#'
#' For derived nodes with pre-computed summaries (types `"filter"`, `"compare"`,
#' or `"agg_total"`), filtering uses the node's `summary` by default as the data
#' source (instead of `mcmodule$data[[data_name]]`) so variate alignment is
#' preserved.
#'
#' @return Either:
#'   - Updated mcmodule with new filtered node (when mcmodule and `name` provided).
#'   - Filtered mcnode object (when only `data` and `mcnode` provided).
#'
#' @return Either:
#'   - An updated mcmodule with a new filtered node (when mcmodule and name are provided)
#'   - A raw filtered mcnode object (when only data and mcnode are provided)
#'
#' @examples
#' # Filter within an mcmodule and create new node
#' imports_mcmodule <- mc_filter(
#'   imports_mcmodule,
#'   "w_prev",
#'   origin == "nord",
#'   name = "w_prev_countryA"
#' )
#'
#' # Filter and return raw mcnode (note: conditions before named args)
#' w_prev <- imports_mcmodule$node_list$w_prev$mcnode
#' w_prev_filtered <- mc_filter(
#'   origin == "nord",
#'   data = imports_data,
#'   mcnode = w_prev
#' )
#'
#' # Multiple filter conditions
#' imports_mcmodule <- mc_filter(
#'   imports_mcmodule,
#'   "w_prev",
#'   origin == "nord",
#'   pathogen == "virus",
#'   name = "w_prev_countryA_virus"
#' )
#' @export
mc_filter <- function(
  mcmodule = NULL,
  mc_name = NULL,
  ...,
  data = NULL,
  mcnode = NULL,
  name = NULL,
  prefix = NULL,
  suffix = "filtered",
  summary = TRUE
) {
  eval_env <- parent.frame()
  mcmodule_expr <- substitute(mcmodule)
  filter_expr <- as.list(substitute(list(...)))[-1]

  # Capture module name before evaluation to avoid deparsing entire object
  module_name <- if (is.symbol(mcmodule_expr)) {
    as.character(mcmodule_expr)
  } else {
    deparse(mcmodule_expr)
  }

  if (length(filter_expr) == 0 && is.call(mcmodule_expr) && is.null(mc_name)) {
    filter_expr <- list(mcmodule_expr)
    mcmodule <- NULL
  } else {
    mcmodule <- eval(mcmodule_expr, eval_env)
  }

  if (length(filter_expr) == 0) {
    stop("At least one filter condition must be provided")
  }

  # Input validation
  if (!is.null(mcnode) && is.null(mc_name)) {
    mc_name <- deparse(substitute(mcnode))
  }

  return_mcmodule <- FALSE

  if (!is.null(mcmodule)) {
    return_mcmodule <- TRUE

    if (is.null(mcnode)) {
      mcnode <- mcmodule$node_list[[mc_name]]$mcnode
    }

    if (!is.mcnode(mcnode)) {
      stop(sprintf("%s must be a mcnode present in %s", mc_name, module_name))
    }

    node <- mcmodule$node_list[[mc_name]]
    data_name <- node$data_name
    node_type <- node$type
    # For derived nodes (filter, compare, agg_total), use pre-computed summary as the data source
    # to preserve variate-to-row correspondence
    uses_summary_data <-
      !is.null(node_type) &&
      node_type %in% c("filter", "compare", "agg_total") &&
      !is.null(node$summary)

    # If data not provided, attempt to use node's data_name to get data from mcmodule$data
    # If node_type is "total" and multiple data_names exist use summary
    if (is.null(data)) {
      data <- if (uses_summary_data) {
        node$summary
      } else if (length(data_name) > 1 && !uses_summary_data) {
        if (node_type == "total") {
          node$summary
        } else {
          message(
            "Multiple data names detected. Using first data_name for filtering."
          )
          data <- mcmodule$data[[data_name[1]]]
        }
      } else {
        mcmodule$data[[data_name]]
      }
    }
  } else {
    if (is.null(data)) {
      stop("mcmodule or data must be provided")
    }
    if (is.null(mcnode)) {
      stop("mcnode must be provided when mcmodule is NULL")
    }
  }

  # Validate mcnode dimensions match data
  n_variates <- dim(mcnode)[3]
  if (n_variates != nrow(data)) {
    stop(sprintf(
      "Number of variates in mcnode (%d) does not match number of rows in data (%d)",
      n_variates,
      nrow(data)
    ))
  }

  # Apply filter to data
  data_filtered <- eval(
    as.call(c(quote(dplyr::filter), list(data), filter_expr)),
    eval_env
  )

  if (nrow(data_filtered) == 0) {
    warning("Filter conditions resulted in zero rows")
  }

  # Get indices of filtered rows without creating temporary columns
  filter_expr_combined <- if (length(filter_expr) == 1) {
    filter_expr[[1]]
  } else {
    Reduce(function(x, y) call("&", x, y), filter_expr)
  }

  filter_indices <- which(eval(filter_expr_combined, data, eval_env))

  if (length(filter_indices) == 0) {
    data_filtered <- data[0, , drop = FALSE]
  } else {
    data_filtered <- data[filter_indices, , drop = FALSE]
  }

  # Extract filtered variates from mcnode
  if (length(filter_indices) == 0) {
    mcnode_filtered <- mc2d::extractvar(mcnode, 1) -
      mc2d::extractvar(mcnode, 1)
  } else if (length(filter_indices) == 1) {
    mcnode_filtered <- mc2d::extractvar(mcnode, filter_indices)
  } else {
    mcnode_filtered <- mc2d::extractvar(mcnode, filter_indices[1])
    for (i in filter_indices[-1]) {
      mcnode_filtered <- mc2d::addvar(
        mcnode_filtered,
        mc2d::extractvar(mcnode, i)
      )
    }
  }

  # If not returning to mcmodule, just return the filtered mcnode
  if (!return_mcmodule) {
    return(mcnode_filtered)
  }

  # Generate name for filtered node
  filtered_mc_name <- if (!is.null(name)) {
    if (!is.null(suffix) && suffix != "") {
      paste0(name, "_", suffix)
    } else {
      name
    }
  } else {
    if (!is.null(suffix) && suffix != "") {
      paste0(mc_name, "_", suffix)
    } else {
      paste0(mc_name, "_flt")
    }
  }

  # Add prefix if provided
  if (!is.null(prefix) && prefix != "") {
    prefix <- paste0(sub("_$", "", prefix), "_")
    filtered_mc_name <- paste0(
      prefix,
      sub(paste0("^", prefix), "", filtered_mc_name)
    )
    prefix <- sub("_$", "", prefix)
  }

  # Get keys from original node
  keys_names <- if (!is.null(mcmodule$node_list[[mc_name]]$keys)) {
    mcmodule$node_list[[mc_name]]$keys
  } else {
    character(0)
  }

  # Create filter expression string for documentation
  filter_expr_str <- vapply(
    filter_expr,
    function(x) {
      paste(deparse(x), collapse = " ")
    },
    character(1)
  )
  filter_description <- paste(filter_expr_str, collapse = " & ")

  # Add new filtered node to module
  mcmodule$node_list[[filtered_mc_name]] <- list(
    mcnode = mcnode_filtered,
    type = "filter",
    param = mc_name,
    inputs = mc_name,
    description = sprintf("Filtered %s: %s", mc_name, filter_description),
    module = module_name,
    keys = keys_names,
    node_expression = sprintf("filter(%s, %s)", mc_name, filter_description),
    scenario = if ("scenario_id" %in% names(data_filtered)) {
      data_filtered$scenario_id
    } else {
      NULL
    },
    data_name = if (!is.null(mcmodule)) {
      mcmodule$node_list[[mc_name]][["data_name"]]
    } else {
      NULL
    },
    prefix = if (!is.null(prefix)) prefix else NULL,
    filter = filter_description
  )

  # Add summary if requested
  if (summary) {
    if (nrow(data_filtered) == 0) {
      mcmodule$node_list[[filtered_mc_name]][["summary"]] <- NULL
    } else {
      # Include scenario_id in summary if it exists in the data
      summary_keys <- keys_names
      if (
        "scenario_id" %in%
          names(data_filtered) &&
          !("scenario_id" %in% keys_names)
      ) {
        summary_keys <- c("scenario_id", keys_names)
      }

      mcmodule$node_list[[filtered_mc_name]][["summary"]] <-
        mc_summary(
          mcmodule = mcmodule,
          data = data_filtered,
          mc_name = filtered_mc_name,
          keys_names = summary_keys
        )
    }
  }

  return(mcmodule)
}
