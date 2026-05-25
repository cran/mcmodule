#' Combine Probabilities Assuming Independence
#'
#' Combines probabilities of multiple independent events using the formula:
#' P(at least one) = 1 - (1-P(A)) * (1-P(B)) * ... Automatically matches
#' dimensions and keys.
#'
#' @param mcmodule (mcmodule object). Module containing node list and data frames.
#' @param mc_names (character vector). Node names to combine.
#' @param name (character, optional). Custom name for combined node.
#'   If NULL, auto-generated. Default: NULL.
#' @param all_suffix (character). Suffix for auto-generated node name.
#'   Default: "all".
#' @param prefix (character, optional). Prefix for output node name. Default: NULL.
#' @param summary (logical). If TRUE, calculate summary statistics. Default: TRUE.
#'
#' @return Updated mcmodule with new combined probability node.
#'
#' @examples
#' module <- list(
#'   node_list = list(
#'     p1 = list(
#'       mcnode = mcstoc(runif,
#'         min = mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3),
#'         max = mcdata(c(0.2, 0.3, 0.4), type = "0", nvariates = 3),
#'         nvariates = 3
#'       ),
#'       data_name = "data_x",
#'       keys = c("category")
#'     ),
#'     p2 = list(
#'       mcnode = mcstoc(runif,
#'         min = mcdata(c(0.5, 0.6, 0.7), type = "0", nvariates = 3),
#'         max = mcdata(c(0.6, 0.7, 0.8), type = "0", nvariates = 3),
#'         nvariates = 3
#'       ),
#'       data_name = "data_y",
#'       keys = c("category")
#'     )
#'   ),
#'   data = list(
#'     data_x = data.frame(
#'       category = c("A", "B", "C"),
#'       scenario_id = c("0", "0", "0")
#'     ),
#'     data_y = data.frame(
#'       category = c("B", "B", "B"),
#'       scenario_id = c("0", "1", "2")
#'     )
#'   )
#' )
#'
#' module <- at_least_one(module, c("p1", "p2"), name = "p_combined")
#' print(module$node_list$p_combined$summary)
#' @export
at_least_one <- function(
  mcmodule,
  mc_names,
  name = NULL,
  all_suffix = NULL,
  prefix = NULL,
  summary = TRUE
) {
  module_name <- deparse(substitute(mcmodule))

  # Check if mcnodes are in mcmodule
  missing_nodes <- mc_names[!mc_names %in% names(mcmodule$node_list)]

  if (length(missing_nodes) > 0) {
    stop(sprintf(
      "Nodes %s not found in %s",
      paste(missing_nodes, collapse = ", "),
      module_name
    ))
  }

  # Extract data_name for each node in mc_names
  nodes_data_name <- lapply(mc_names, function(x) {
    mcmodule$node_list[[x]][["data_name"]]
  })
  # Get unique, non-empty data_names
  data_name <- unique(unlist(nodes_data_name))
  data_name <- data_name[!is.na(data_name) & nzchar(data_name)]

  # Get the third dimension size (number of variates) for each node
  nodes_dim <- sapply(mc_names, function(x) {
    dim(mcmodule$node_list[[x]][["mcnode"]])[3]
  })

  # Check if each node is aggregated (has agg_keys)
  nodes_agg <- sapply(mc_names, function(x) {
    !is.null(mcmodule$node_list[[x]][["agg_keys"]])
  })

  # Key names that are common to all nodes
  nodes_common_keys_names <- Reduce(
    intersect,
    lapply(mc_names, function(x) {
      names(mc_keys(mcmodule, x))
    })
  )

  # List of key values for each node, using only the common keys
  nodes_common_keys <- lapply(mc_names, function(x) {
    mc_keys(mcmodule, x)[nodes_common_keys_names]
  })
  names(nodes_common_keys) <- mc_names

  # List of key values for each node
  nodes_keys <- lapply(mc_names, function(x) {
    mc_keys(mcmodule, x)
  })
  names(nodes_keys) <- mc_names

  # Initialize combined probability and keys_names vector
  p_all <- 0
  keys_names <- c()

  # Check that data_name, dimensions and keys are identical for all nodes
  if (
    length(data_name) == 1 &&
      length(unique(nodes_dim)) == 1 &&
      all(!nodes_agg) &&
      length(unique(nodes_common_keys)) == 1
  ) {
    data <- nodes_common_keys[[1]]

    # Loop to get the combined probability of all mcnodes
    for (i in seq_along(mc_names)) {
      mc_name <- mc_names[i]
      p_i <- mcmodule$node_list[[mc_name]][["mcnode"]]
      keys_names <- nodes_common_keys_names

      # Update combined probability
      p_all <- 1 - ((1 - p_all) * (1 - p_i))
    }
  } else {
    if (!length(mc_names) == 2) {
      stop(sprintf(
        "To aggregate mc_names with different data_name or keys, provide exactly two mc_nodes"
      ))
    }

    # Get keys for both nodes
    mc_name_x <- mc_names[1]
    mc_name_y <- mc_names[2]

    keys_names_x <- unique(c(keys_names, names(nodes_keys[[mc_name_x]])))
    keys_names_y <- unique(c(keys_names, names(nodes_keys[[mc_name_y]])))

    keys_names <- unique(intersect(keys_names_x, keys_names_y))

    # Match and combine probabilities
    p_xy <- mc_match(mcmodule, mc_name_x, mc_name_y, keys_names)
    p_all <- 1 - ((1 - p_xy[[1]]) * (1 - p_xy[[2]]))
    data <- p_xy$keys_xy
  }

  # Generate name for combined node
  p_all_mc_name <- if (is.null(name)) {
    generate_all_name(mc_names, all_suffix = all_suffix)
  } else if (is.null(all_suffix)) {
    name
  } else {
    paste0(name, "_", all_suffix)
  }

  # Add prefix if provided
  if (!is.null(prefix) && prefix != "") {
    prefix <- paste0(sub("_$", "", prefix), "_")
    p_all_mc_name <- paste0(prefix, sub(paste0("^", prefix), "", p_all_mc_name))
    prefix <- sub("_$", "", prefix)
  }

  if (!is.null(prefix) && prefix == "") {
    prefix <- NULL
  }

  # Add new node to module
  mcmodule$node_list[[p_all_mc_name]] <- list(
    mcnode = p_all,
    type = "total",
    param = mc_names,
    inputs = mc_names,
    description = paste(
      "Probability at least one of",
      mc_names,
      "(assuming independence)"
    ),
    module = module_name,
    keys = keys_names,
    node_expression = paste0(
      "1-(",
      paste(
        paste("(1-", mc_names, ")", sep = ""),
        collapse = "*"
      ),
      ")"
    ),
    scenario = data$scenario_id,
    data_name = data_name,
    prefix = prefix
  )

  # Get agg keys (if nodes are aggregated)
  if (any(nodes_agg)) {
    mcmodule$node_list[[p_all_mc_name]][["agg_keys"]] <- unique(unlist(lapply(
      mc_names,
      function(x) {
        mcmodule$node_list[[x]][["agg_keys"]]
      }
    )))
    mcmodule$node_list[[p_all_mc_name]][["keep_variates"]] <- all(unlist(lapply(
      mc_names,
      function(x) {
        mcmodule$node_list[[x]][["keep_variates"]]
      }
    )))
  }

  # Mark as from_sample_design if all input nodes are from_sample_design
  if (
    all(mc_names %in% names(mcmodule$node_list)) &&
      all(sapply(mc_names, function(x) {
        isTRUE(mcmodule$node_list[[x]][["from_sample_design"]])
      }))
  ) {
    mcmodule$node_list[[p_all_mc_name]][["from_sample_design"]] <- TRUE
  }

  # Add summary if requested
  if (summary) {
    mcmodule$node_list[[p_all_mc_name]][["summary"]] <-
      mc_summary(
        mcmodule = mcmodule,
        data = data,
        mc_name = p_all_mc_name,
        keys_names = keys_names
      )
  }

  return(mcmodule)
}

# Function to generate a consistent name with all_suffix, adds "all" by default
generate_all_name <- function(mc_names, all_suffix = NULL) {
  if (is.null(all_suffix)) {
    all_suffix <- "all"
  }
  # Check if "all" is already in any input
  if (any(grepl(paste0("_", all_suffix, "$"), mc_names))) {
    stop(sprintf(
      "One of the mc_names already contains '%s' suffix",
      paste0("_", all_suffix)
    ))
  }

  # Remove common suffixes by finding the common prefix
  # Split strings into parts
  parts_list <- strsplit(mc_names, "_")

  # Find the minimum length to compare
  min_length <- min(sapply(parts_list, length))

  # Compare parts until they differ
  common_parts <- c()
  for (i in 1:min_length) {
    current_parts <- sapply(parts_list, `[`, i)
    if (length(unique(current_parts)) == 1) {
      common_parts <- c(common_parts, current_parts[1])
    } else {
      break
    }
  }

  # If no common parts found, throw error
  if (length(common_parts) == 0) {
    stop("Input strings do not share a common prefix - please provide a name")
  }

  # Generate final name
  paste0(c(common_parts, all_suffix), collapse = "_")
}

#' Aggregate mcnode Values Across Groups
#'
#' Aggregates node values across grouping variables using various methods
#' (combined probability, sum, mean, or automatic selection). Returns an
#' updated mcmodule with new aggregated node.
#'
#' If sample-design nodes are aggregated, the resulting node will be equal
#' to the original node, but with the "agg_total" type and summary statistics added.
#'
#' @param mcmodule (mcmodule object). Module containing node list and data.
#' @param mc_name (character). Name of node to aggregate.
#' @param agg_keys (character vector, optional). Column names for grouping.
#'   If NULL, defaults to "scenario_id". Default: NULL.
#' @param agg_suffix (character, optional). Suffix for aggregated node name.
#'   Default: "agg".
#' @param prefix (character, optional). Prefix for output node name. Default: NULL.
#' @param name (character, optional). Custom name for output node. Default: NULL.
#' @param summary (logical). If TRUE, include summary statistics. Default: TRUE.
#' @param keep_variates (logical). If TRUE, preserve individual variate values.
#'   Default: FALSE.
#' @param agg_func (character, optional). Aggregation method: "prob" (combined
#'   probability), "sum", "avg", or NULL (automatic). Default: NULL.
#'
#' @return mcmodule with new aggregated node added
#'
#' @examples
#' imports_mcmodule <- agg_totals(
#'   imports_mcmodule, "no_detect",
#'   agg_keys = c("scenario_id", "pathogen")
#' )
#' print(imports_mcmodule$node_list$no_detect_agg$summary)
#' @export
agg_totals <- function(
  mcmodule,
  mc_name,
  agg_keys = NULL,
  agg_suffix = NULL,
  prefix = NULL,
  name = NULL,
  summary = TRUE,
  keep_variates = FALSE,
  agg_func = NULL
) {
  module_name <- deparse(substitute(mcmodule))

  # Check if mcnode is in mcmodule
  if (!mc_name %in% names(mcmodule$node_list)) {
    stop(sprintf("%s not found in %s", mc_name, module_name))
  }

  if (
    !(is.null(agg_func) ||
      agg_func %in% c("prob", "avg", "sum"))
  ) {
    stop("Aggregation function must be prob, avg, sum or NULL")
  }
  if (is.null(agg_keys)) {
    agg_keys <- "scenario_id"
    message(sprintf(
      "Keys to aggregate by not provided, using 'scenario_id' by default"
    ))
  }

  # Extract module name and node data
  mcnode <- mcmodule$node_list[[mc_name]][["mcnode"]]
  key_col <- mc_keys(mcmodule, mc_name, agg_keys)
  data_name <- mcmodule$node_list[[mc_name]][["data_name"]]

  # Generate name for aggregated node
  agg_mc_name <- if (is.null(name)) {
    if (is.null(agg_suffix)) {
      agg_suffix <- "agg"
    }
    paste0(mc_name, "_", agg_suffix)
  } else if (is.null(agg_suffix)) {
    name
  } else {
    paste0(name, "_", agg_suffix)
  }

  # Add prefix if provided
  if (!is.null(prefix) && prefix != "") {
    prefix <- paste0(sub("_$", "", prefix), "_")
    agg_mc_name <- paste0(
      prefix,
      "_",
      sub(paste0("^", prefix), "", agg_mc_name)
    )
    prefix <- sub("_$", "", prefix)
  }

  if (!is.null(prefix) && prefix == "") {
    prefix <- NULL
  }

  # Create grouping index
  key_col$key <- do.call(paste, c(key_col, sep = ", "))
  key_levels <- unique(key_col$key)

  #If sample-design nodes are aggregated
  if (isTRUE(mcmodule$node_list[[mc_name]][["from_sample_design"]])) {
    # Return original node with new type and summary
    total_agg <- mcnode
    mcmodule$node_list[[agg_mc_name]][["from_sample_design"]] <- TRUE
  } else {
    # Extract variates
    variates_list <- list()
    inv_variates_list <- list()
    for (i in seq_len(dim(mcnode)[3])) {
      variates_list[[i]] <- mc2d::extractvar(mcnode, i)
      inv_variates_list[[i]] <- 1 - mc2d::extractvar(mcnode, i)
    }

    # Process each group
    for (i in seq_along(key_levels)) {
      index <- key_col$key %in% key_levels[i]

      if (!is.null(agg_func) && agg_func == "avg") {
        # Calculate average value
        total_lev <- Reduce("+", variates_list[index]) / sum(index)
      } else if (
        (is.null(agg_func) &&
          grepl("_n$", mc_name)) ||
          (!is.null(agg_func) && agg_func == "sum")
      ) {
        # Sum for counts
        total_lev <- Reduce("+", variates_list[index])
      } else {
        # Combine probabilities
        total_lev <- 1 - Reduce("*", inv_variates_list[index])
      }

      # Aggregate results
      if (keep_variates) {
        # One row per original variate
        agg_index <- mc2d::mcdata(index, type = "0", nvariates = length(index))

        if (i != 1) {
          total_agg <- total_agg + agg_index * total_lev
        } else {
          total_agg <- agg_index * total_lev
        }
      } else {
        # One row per result
        if (i != 1) {
          total_agg <- mc2d::addvar(total_agg, total_lev)
        } else {
          total_agg <- total_lev
        }
      }
    }
  }

  # Generate new data
  if (keep_variates) {
    # One row per original variate
    new_agg_keys <- mcmodule$node_list[[mc_name]][["keys"]]
    key_data <- mc_keys(mcmodule, mc_name)[new_agg_keys]
  } else {
    # One row per result
    new_agg_keys <- agg_keys
    key_data <- unique(key_col)
  }

  # Add description and node_expression
  if (!is.null(agg_func) && agg_func == "avg") {
    # Calculate average value
    mcmodule$node_list[[agg_mc_name]][["description"]] <-
      paste0("Average value by: ", paste0(agg_keys, collapse = ", "))
    mcmodule$node_list[[agg_mc_name]][["node_expression"]] <-
      paste0("Average ", mc_name, " by: ", paste0(agg_keys, collapse = ", "))
  } else if (
    (is.null(agg_func) &&
      grepl("_n$", mc_name)) ||
      (!is.null(agg_func) && agg_func == "sum")
  ) {
    # Sum for counts
    mcmodule$node_list[[agg_mc_name]][["description"]] <-
      paste0("Sum by: ", paste0(agg_keys, collapse = ", "))
    mcmodule$node_list[[agg_mc_name]][["node_expression"]] <-
      paste0(
        mc_name,
        "_1+",
        mc_name,
        "_2+... by: ",
        paste0(agg_keys, collapse = ", ")
      )
  } else {
    # Combine probabilities
    mcmodule$node_list[[agg_mc_name]][["description"]] <-
      paste0(
        "Combined probability assuming independence by: ",
        paste0(agg_keys, collapse = ", ")
      )
    mcmodule$node_list[[agg_mc_name]][["node_expression"]] <-
      paste0(
        "1-((1-",
        mc_name,
        "_1)*(1-",
        mc_name,
        "_2)...) by: ",
        paste0(agg_keys, collapse = ", ")
      )
  }

  # Add aggregated node to module
  mcmodule$node_list[[agg_mc_name]][["mcnode"]] <- total_agg
  mcmodule$node_list[[agg_mc_name]][["type"]] <- "agg_total"
  mcmodule$node_list[[agg_mc_name]][["module"]] <- module_name
  mcmodule$node_list[[agg_mc_name]][["agg_data"]] <- key_levels
  mcmodule$node_list[[agg_mc_name]][["agg_keys"]] <- new_agg_keys
  mcmodule$node_list[[agg_mc_name]][["keep_variates"]] <- keep_variates
  mcmodule$node_list[[agg_mc_name]][["keys"]] <-
    mcmodule$node_list[[mc_name]][["keys"]]
  mcmodule$node_list[[agg_mc_name]][["inputs"]] <- mc_name
  mcmodule$node_list[[agg_mc_name]][["data_name"]] <- data_name

  if (!is.null(prefix)) {
    mcmodule$node_list[[agg_mc_name]][["prefix"]] <- prefix
  }

  if (summary) {
    if (isTRUE(mcmodule$node_list[[mc_name]][["from_sample_design"]])) {
      summary_keys <- names(key_data)
    } else {
      summary_keys <- new_agg_keys
    }
    mcmodule$node_list[[agg_mc_name]][["summary"]] <-
      mc_summary(
        mcmodule = mcmodule,
        data = key_data,
        mc_name = agg_mc_name,
        keys_names = summary_keys
      )
  }
  return(mcmodule)
}


#' Trial Probability and Expected Counts
#'
#' Calculates probabilities and expected counts across hierarchical levels
#' (trial, subset, set) in a structured population. Uses trial probabilities and
#' handles nested sampling with conditional probabilities.
#'
#' @param mcmodule (mcmodule object). Module containing input data and node structure.
#' @param mc_names (character vector). Node names to process.
#' @param trials_n (character). Trial count column name.
#' @param subsets_n (character, optional). Subset count column name. Default: NULL.
#' @param subsets_p (character, optional). Subset prevalence column name. Default: NULL.
#' @param name (character, optional). Custom name for output nodes. Default: NULL.
#' @param prefix (character, optional). Prefix for output node names. Default: NULL.
#' @param combine_prob (logical). If TRUE, combine probability of all nodes assuming
#'   independence. Default: TRUE.
#' @param all_suffix (character). Suffix for combined node name. Default: "all".
#' @param level_suffix (list, optional). Suffixes for each hierarchical level.
#'   Default: c(trial="trial", subset="subset", set="set").
#' @param mctable (data frame, optional). Monte Carlo nodes definitions.
#'   Default: set_mctable().
#' @param sample_design (matrix, data frame, or list, optional). Sampling
#'   design used to create missing input nodes via [matrix_to_mcnodes()].
#'   Accepts a matrix/data frame (for example from [sensobol::sobol_matrices()]) or a list with element `X` (typically output
#'   of [sensitivity::sensitivity] functions such as [sensitivity::morris()]). Defaults to [set_sample_design()].
#' @param agg_keys (character vector, optional). Column names for aggregation.
#'   Default: NULL.
#' @param agg_suffix (character). Suffix for aggregated node names. Default: "hag".
#' @param keep_variates (logical). If TRUE, preserve individual variate values.
#'   Default: FALSE.
#' @param summary (logical). If TRUE, include summary statistics. Default: TRUE.
#' @param data_name (character, optional). Data name used to create trials_n,
#'   subsets_n and subsets_p nodes if they don't exist in mcmodule. Default: NULL.
#'
#' @return Updated mcmodule object containing combined node probabilities and
#'   probabilities/counts at trial, subset, and set levels.
#'
#' @examples
#' imports_mcmodule <- trial_totals(
#'   mcmodule = imports_mcmodule,
#'   mc_names = "no_detect",
#'   trials_n = "animals_n",
#'   subsets_n = "farms_n",
#'   subsets_p = "h_prev",
#'   mctable = imports_mctable
#' )
#' print(imports_mcmodule$node_list$no_detect_set$summary)
#' @export
trial_totals <- function(
  mcmodule,
  mc_names,
  trials_n,
  subsets_n = NULL,
  subsets_p = NULL,
  name = NULL,
  prefix = NULL,
  combine_prob = TRUE,
  all_suffix = NULL,
  level_suffix = c(trial = "trial", subset = "subset", set = "set"),
  mctable = set_mctable(),
  sample_design = set_sample_design(),
  agg_keys = NULL,
  agg_suffix = NULL,
  keep_variates = FALSE,
  summary = TRUE,
  data_name = NULL
) {
  module_name <- deparse(substitute(mcmodule))

  # Check if mcnodes are in mcmodule
  missing_nodes <- mc_names[!mc_names %in% names(mcmodule$node_list)]

  if (length(missing_nodes) > 0) {
    stop(sprintf(
      "Nodes %s not found in %s",
      paste(missing_nodes, collapse = ", "),
      module_name
    ))
  }

  # Get data_name for all mc_nodes
  names(mc_names) <- c(paste0("mc_name_", 1:length(mc_names)))
  mc_trial_names <- c(
    trials_n = trials_n,
    subsets_n = subsets_n,
    subsets_p = subsets_p
  )
  mc_inputs_names <- c(mc_names, mc_trial_names)

  mc_inputs_names <- mc_inputs_names[
    !is.null(mc_inputs_names) & mc_inputs_names != "1"
  ]
  nodes_data_name <- lapply(mc_inputs_names, function(x) {
    mcmodule$node_list[[x]][["data_name"]]
  })
  names(nodes_data_name) <- mc_inputs_names

  # For each node, get its data_names (for error/message context)
  node_data_names <- lapply(mc_inputs_names, function(x) {
    mcmodule$node_list[[x]][["data_name"]]
  })
  names(node_data_names) <- mc_inputs_names

  # Filter out NULL, NA, and "" data_names for each node
  filtered_node_data_names <- lapply(nodes_data_name, function(x) {
    x[!is.null(x) & !is.na(x) & nzchar(x)]
  })

  # Only keep nodes that have at least one valid data_name
  filtered_node_data_names <- Filter(
    function(x) length(x) > 0,
    filtered_node_data_names
  )

  # Check if all remaining nodes have the same set of data_names (or null)
  all_equal <- length(unique(lapply(filtered_node_data_names, function(x) {
    paste(sort(x), collapse = ",")
  }))) <=
    1

  # All unique data_names across all nodes
  all_data_names <- unique(unlist(filtered_node_data_names))

  sample_design_data <- NULL
  if (!is.null(sample_design)) {
    sample_design_input <- sample_design
    if (
      is.list(sample_design_input) &&
        !is.data.frame(sample_design_input) &&
        !is.matrix(sample_design_input)
    ) {
      if (!"X" %in% names(sample_design_input)) {
        stop("sample_design list must contain element 'X'")
      }
      sample_design_input <- sample_design_input$X
    }

    if (
      !(is.matrix(sample_design_input) || is.data.frame(sample_design_input))
    ) {
      stop(
        "sample_design must be a matrix, data frame, or list with element 'X'"
      )
    }

    sample_design_data <- as.data.frame(
      sample_design_input,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    if (nrow(sample_design_data) < 1) {
      stop("sample_design has 0 rows")
    }

    if (ncol(sample_design_data) < 1) {
      stop("sample_design has 0 columns")
    }
  }

  # Calculate combined probability for all nodes if requested
  # (if more than one node is provided)
  if (combine_prob && length(mc_names) > 1) {
    mcmodule <- at_least_one(
      mcmodule = mcmodule,
      mc_names = mc_names,
      name = name,
      all_suffix = all_suffix,
      prefix = prefix,
      summary
    )

    # Generate name for combined node
    p_all_mc_name <- if (is.null(name)) {
      generate_all_name(mc_names, all_suffix = all_suffix)
    } else if (is.null(all_suffix)) {
      name
    } else {
      paste0(name, "_", all_suffix)
    }

    # Update module name metadata (defaults to mcmodule)
    mcmodule$node_list[[p_all_mc_name]][["module"]] <- module_name
    # mc_match if several data names are provided
    if (!all_equal) {
      for (i in seq_along(mc_names)) {
        mc_match_i <- mc_match(mcmodule, p_all_mc_name, mc_names[i])[[2]]
        mc_name_i <- paste0(names(mc_names)[i], "_mc")
        assign(mc_name_i, mc_match_i)
      }
    }
    ref_mc_name <- p_all_mc_name
    mc_names <- c(mc_names, mc_name_all = p_all_mc_name)
  } else {
    ref_mc_name <- mc_names[1]
    if (!combine_prob && length(mc_names) > 1) {
      message(sprintf(
        "Using '%s' as reference node for mc_match",
        ref_mc_name
      ))
    }
  }

  # Determine which data_name to use
  if (!all_equal) {
    #If a combined probability node was created, use its data_name
    if (!is.null(mcmodule$node_list[[ref_mc_name]])) {
      ref_data_names <- mcmodule$node_list[[ref_mc_name]][["data_name"]]
      if (length(ref_data_names) > 1) {
        if (!is.null(data_name)) {
          # User provided data_name: check it exists in any node
          if (!data_name %in% all_data_names) {
            stop(sprintf(
              "Provided data_name '%s' not found in available data_names for nodes: %s",
              data_name,
              paste(all_data_names, collapse = ", ")
            ))
          }
          ref_data_name <- data_name
          message(sprintf(
            "Using data_name '%s' for node creation.",
            ref_data_name
          ))
        } else {
          # Default to the last available data_name
          ref_data_name <- ref_data_names[length(ref_data_names)]
          # Indicate which nodes have which data_names
          node_names_with_multiple <- names(node_data_names)[sapply(
            node_data_names,
            function(x) length(x) > 1
          )]
          msg <- sprintf(
            "data_name is not equal for all nodes, using data_name '%s' for node creation (can be manually set with data_name argument).",
            ref_data_name
          )
          if (length(node_names_with_multiple) > 0) {
            msg <- paste0(
              msg,
              " Nodes with multiple data_names: ",
              paste(node_names_with_multiple, collapse = ", "),
              "."
            )
          }
          message(sprintf("%s", msg))
        }
      } else {
        ref_data_name <- ref_data_names
      }
      # If no combined probability node, handle data_name selection
    } else {
      if (!is.null(data_name)) {
        # User provided data_name: check it exists in any node
        if (!data_name %in% all_data_names) {
          stop(sprintf(
            "Provided data_name '%s' not found in available data_names for nodes: %s",
            data_name,
            paste(all_data_names, collapse = ", ")
          ))
        }
        ref_data_name <- data_name
        message(sprintf(
          "Using data_name '%s' for node creation.",
          ref_data_name
        ))
      } else {
        # Default to the last available data_name
        ref_data_name <- all_data_names[length(all_data_names)]
        # Indicate which nodes have which data_names
        node_names_with_multiple <- names(node_data_names)[sapply(
          node_data_names,
          function(x) length(x) > 1
        )]
        msg <- sprintf(
          "data_name is not equal for all nodes, using data_name '%s' for node creation (can be manually set with data_name argument).",
          ref_data_name
        )
        if (length(node_names_with_multiple) > 0) {
          msg <- paste0(
            msg,
            " Nodes with multiple data_names: ",
            paste(node_names_with_multiple, collapse = ", "),
            "."
          )
        }
        message(sprintf("%s", msg))
      }
    }
  } else {
    # All nodes have the same data_name(s)
    # If user provided a valid data_name, use it; otherwise fall back to current logic
    if (!is.null(data_name)) {
      if (data_name %in% all_data_names) {
        ref_data_name <- data_name
        message(sprintf(
          "Using provided data_name '%s' for node creation.",
          ref_data_name
        ))
      } else {
        stop(sprintf(
          "Provided data_name '%s' not found in available data_names: '%s'.",
          data_name,
          paste(all_data_names, collapse = "', '")
        ))
      }
    }

    if (!exists("ref_data_name")) {
      if (length(all_data_names) > 1) {
        node_names_with_multiple <- names(node_data_names)[sapply(
          node_data_names,
          function(x) length(x) > 1
        )]
        ref_data_name <- all_data_names[length(all_data_names)]
        message(sprintf(
          "mcnodes have multiple data_name ('%s') for node(s) '%s', using '%s' for node creation (can be manually set with data_name argument)",
          paste(all_data_names, collapse = "', '"),
          paste(node_names_with_multiple, collapse = ", "),
          ref_data_name
        ))
      } else {
        ref_data_name <- all_data_names
      }
    }
  }

  if (
    !is.null(name) &&
      length(mc_names) > 1 &&
      !combine_prob
  ) {
    stop(sprintf(
      "name argument can only be used when mc_names length is 1 or when combine_prob is TRUE"
    ))
  }

  if (!all(names(level_suffix) %in% c("trial", "subset", "set"))) {
    stop(sprintf(
      "Suffixes for each hierarchical level must be defined as a named vector with the following structure: c(trial = '...', subset = '...', set = '...')"
    ))
  }

  if (!is.null(prefix) && prefix == "") {
    prefix <- NULL
  }

  # Fix missing level suffixes
  missing_suffixes <- setdiff(c("trial", "subset", "set"), names(level_suffix))
  for (suffix in missing_suffixes) {
    level_suffix[suffix] <- suffix
  }

  hag_suffix <- if (is.null(agg_suffix) || agg_suffix == "") {
    "hag"
  } else {
    agg_suffix
  }

  data <- NULL
  if (!is.null(ref_data_name) && ref_data_name %in% names(mcmodule$data)) {
    data <- mcmodule$data[[ref_data_name]]
  }

  if (is.null(data) && is.null(sample_design_data)) {
    stop(
      "No input data found for trial_totals and sample_design is NULL"
    )
  }

  # Function for individual mcnode creation and processing
  process_trial_mcnode <- function(
    mc_name,
    mcmodule,
    data,
    module_name,
    agg_keys,
    hag_suffix,
    mctable,
    keep_variates,
    ref_data_name,
    sample_design_data = NULL,
    agg_func = NULL
  ) {
    if (mc_name %in% names(mcmodule$node_list)) {
      mc_node <- mcmodule$node_list[[mc_name]][["mcnode"]]
    } else {
      if (
        !is.null(sample_design_data) &&
          mc_name %in% colnames(sample_design_data)
      ) {
        matrix_to_mcnodes(
          X = sample_design_data[, mc_name, drop = FALSE],
          envir = environment()
        )
      } else {
        if (!mc_name %in% mctable$mcnode) {
          stop(sprintf("%s not found in mctable", mc_name))
        }

        if (is.null(data)) {
          stop(sprintf(
            "data is NULL and '%s' is not present in sample_design",
            mc_name
          ))
        }

        mc_row <- mctable[mctable$mcnode %in% mc_name, ]
        create_mcnodes(data, mctable = mc_row)
      }

      mc_node <- get(mc_name)

      if (mc_name %in% mctable$mcnode) {
        mc_row <- mctable[mctable$mcnode %in% mc_name, ]
      } else {
        mc_row <- NULL
      }

      # Add metadata
      pattern <- paste0("\\<", mc_name, "(\\>|[^>]*\\>)")
      inputs_col <- if (!is.null(data)) {
        names(data[grepl(pattern, names(data))])
      } else {
        character(0)
      }
      mcmodule$node_list[[mc_name]][["inputs_col"]] <- inputs_col

      if (!is.null(mc_row) && !is.na(mc_row$mc_func)) {
        mcmodule$node_list[[mc_name]][["mc_func"]] <- as.character(
          mc_row$mc_func
        )
      }

      mcmodule$node_list[[mc_name]][["description"]] <- if (!is.null(mc_row)) {
        as.character(mc_row$description)
      } else {
        NA_character_
      }
      mcmodule$node_list[[mc_name]][["type"]] <- "in_node"
      mcmodule$node_list[[mc_name]][["module"]] <- module_name
      mcmodule$node_list[[mc_name]][["data_name"]] <- ref_data_name
      mcmodule$node_list[[mc_name]][["mcnode"]] <- mc_node
      mcmodule$node_list[[mc_name]][["mc_func"]] <- if (!is.null(mc_row)) {
        mc_row$mc_func
      } else {
        NA
      }

      if (!is.null(data) && "scenario_id" %in% names(data)) {
        mcmodule$node_list[[mc_name]][["scenario"]] <- data$scenario_id
      }

      if (
        !is.null(sample_design_data) &&
          mc_name %in% colnames(sample_design_data)
      ) {
        mcmodule$node_list[[mc_name]][["from_sample_design"]] <- TRUE
      }
    }

    if (!is.null(agg_keys)) {
      # Aggregate node if agg_keys provided
      messages <- character(0)
      withCallingHandlers(
        expr = {
          mcmodule <- agg_totals(
            mcmodule = mcmodule,
            mc_name = mc_name,
            agg_keys = agg_keys,
            agg_suffix = hag_suffix,
            agg_func = agg_func,
            keep_variates = keep_variates
          )
        },
        message = function(m) {
          messages <<- c(messages, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      )
      if (!all(grepl("variates per group for", messages))) {
        message(sprintf("%s", paste(messages, collapse = "; ")))
      }
      # Change mcnode name to agg version name
      mc_name_name <- deparse(substitute(mc_name))
      agg_mc_name <- paste0(mc_name, "_", hag_suffix)
      assign(mc_name_name, agg_mc_name, envir = parent.frame())
      # Add agg_keys to metadata
      mcmodule$node_list[[agg_mc_name]][["agg_keys"]] <- agg_keys
      # Reassign mcmodule name (defaults to "mcmodule")
      mcmodule$node_list[[agg_mc_name]][["module"]] <- module_name
      mcmodule$node_list[[agg_mc_name]][["keep_variates"]] <- keep_variates
    }

    return(mcmodule)
  }
  # Iniciate keys_names vector to keep track of all keys used in nodes
  keys_names <- c()

  # Process all nodes
  mcmodule <- process_trial_mcnode(
    trials_n,
    mcmodule,
    data,
    module_name,
    agg_keys,
    hag_suffix,
    mctable,
    keep_variates,
    ref_data_name,
    sample_design_data
  )

  # mc_match if several data names are provided
  trials_n_mc <- if (!all_equal) {
    mc_match(mcmodule, ref_mc_name, trials_n)[[2]]
  } else {
    mcmodule$node_list[[trials_n]][["mcnode"]]
  }

  keys_names <- unique(c(
    keys_names,
    mcmodule$node_list[[trials_n]][["keys"]]
  ))

  # If subsets_n is NULL, defaults to 1
  if (is.null(subsets_n)) {
    subsets_n_mc <- mcnode_na_rm(trials_n_mc / trials_n_mc, 1)
    subsets_n <- "1"
    hierarchical_n <- FALSE
  } else {
    mcmodule <- process_trial_mcnode(
      subsets_n,
      mcmodule,
      data,
      module_name,
      agg_keys,
      hag_suffix,
      mctable,
      keep_variates,
      ref_data_name,
      sample_design_data,
      agg_func = "avg"
    )

    # mc_match if several data names are provided
    subsets_n_mc <- if (!all_equal) {
      mc_match(mcmodule, ref_mc_name, subsets_n)[[2]]
    } else {
      mcmodule$node_list[[subsets_n]][["mcnode"]]
    }

    keys_names <- unique(c(
      keys_names,
      mcmodule$node_list[[subsets_n]][["keys"]]
    ))

    hierarchical_n <- TRUE
  }

  # If subsets_p is NULL, no multilevel probability, defaults to 1
  if (is.null(subsets_p)) {
    multilevel <- FALSE
    subsets_p_mc <- mcnode_na_rm(trials_n_mc / trials_n_mc, 1)
    subsets_p <- "1"
    hierarchical_p <- FALSE
  } else {
    multilevel <- TRUE
    mcmodule <- process_trial_mcnode(
      subsets_p,
      mcmodule,
      data,
      module_name,
      agg_keys,
      hag_suffix,
      mctable,
      keep_variates,
      ref_data_name,
      sample_design_data,
      agg_func = "avg"
    )

    # mc_match if several data names are provided
    subsets_p_mc <- if (!all_equal) {
      mc_match(mcmodule, ref_mc_name, subsets_p)[[2]]
    } else {
      mcmodule$node_list[[subsets_p]][["mcnode"]]
    }

    keys_names <- unique(c(
      keys_names,
      mcmodule$node_list[[subsets_p]][["keys"]]
    ))

    hierarchical_p <- TRUE
  }

  # Helper function to add metadata to nodes
  add_mc_metadata <- function(
    node_list,
    name,
    value,
    params,
    description,
    expression,
    type = "total",
    keys_names,
    agg_keys,
    total_type,
    keep_variates,
    prefix
  ) {
    node_list[[name]] <- list(
      mcnode = value,
      param = params,
      inputs = params,
      description = description,
      node_expression = expression,
      type = type,
      module = module_name,
      keys = keys_names,
      scenario = if (!is.null(data) && "scenario_id" %in% names(data)) {
        data$scenario_id
      } else {
        NULL
      },
      data_name = all_data_names,
      prefix = prefix,
      total_type = total_type
    )

    if (
      all(params %in% names(node_list)) &&
        !is.null(sample_design) &&
        all(sapply(params, function(x) {
          isTRUE(node_list[[x]][["from_sample_design"]]) ||
            isTRUE(node_list[[x]][["type"]] == "scalar") ||
            isTRUE(node_list[[x]][["created_in_exp"]])
        }))
    ) {
      node_list[[name]][["from_sample_design"]] <- TRUE
    }

    if (!is.null(agg_keys)) {
      node_list[[name]]$agg_keys <- agg_keys
      node_list[[name]]$keep_variates <- keep_variates
    }

    node_list
  }

  # Configuration for calculations
  calculations <- list(
    trial = list(
      prob = list(
        formula = function(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc) {
          p_a * subsets_p_mc
        },
        description = paste0(
          "Probability of one %s trial (",
          level_suffix[["trial"]],
          ")"
        ),
        suffix = paste0("_", level_suffix[["trial"]]),
        expression = function(mc_name, subsets_p) {
          paste0(subsets_p, "*", mc_name)
        }
      ),
      num = list(
        formula = function(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc) {
          mcnode_na_rm(p_a / p_a, 1)
        },
        description = paste0("One %s trials (", level_suffix[["trial"]], ")"),
        suffix = paste0("_", level_suffix[["trial"]], "_n"),
        expression = function(mc_name) {
          paste0("mcnode_na_rm(", mc_name, "/", mc_name, ", 1)")
        }
      )
    ),
    subset = list(
      prob = list(
        formula = function(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc) {
          1 - (1 - subsets_p_mc * (1 - (1 - p_a)^trials_n_mc))
        },
        description = paste0(
          "Probability of at least one %s in a subset (",
          level_suffix[["subset"]],
          ")"
        ),
        suffix = paste0("_", level_suffix[["subset"]]),
        expression = function(mc_name, trials_n, subsets_p) {
          paste0("1-(1-", subsets_p, "*(1-(1-", mc_name, ")^", trials_n, "))")
        }
      ),
      num = list(
        formula = function(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc) {
          p_a * trials_n_mc * subsets_p_mc
        },
        description = paste0(
          "Expected number of %s in a subset (",
          level_suffix[["subset"]],
          ")"
        ),
        suffix = paste0("_", level_suffix[["subset"]], "_n"),
        expression = function(mc_name, trials_n, subsets_p) {
          paste0(mc_name, "*", trials_n, "*", subsets_p)
        }
      )
    ),
    set = list(
      prob = list(
        formula = function(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc) {
          1 - (1 - subsets_p_mc * (1 - (1 - p_a)^trials_n_mc))^subsets_n_mc
        },
        description = paste0(
          "Probability of at least one %s in a set (",
          level_suffix[["set"]],
          ")"
        ),
        suffix = paste0("_", level_suffix[["set"]]),
        expression = function(mc_name, trials_n, subsets_n, subsets_p) {
          paste0(
            "1-(1-",
            subsets_p,
            "*(1-(1-",
            mc_name,
            ")^",
            trials_n,
            "))^",
            subsets_n
          )
        }
      ),
      num = list(
        formula = function(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc) {
          p_a * trials_n_mc * subsets_p_mc * subsets_n_mc
        },
        description = paste0(
          "Expected number of %s in a set (",
          level_suffix[["set"]],
          ")"
        ),
        suffix = paste0("_", level_suffix[["set"]], "_n"),
        expression = function(mc_name, trials_n, subsets_n, subsets_p) {
          paste0(mc_name, "*", trials_n, "*", subsets_p, "*", subsets_n)
        }
      )
    )
  )

  for (mc_name in mc_names) {
    if (!is.null(agg_keys)) {
      # Original node keys before aggregation
      original_keys <- mcmodule$node_list[[mc_name]][["keys"]]
      # Aggregate node if agg_keys provided
      messages <- character(0)
      withCallingHandlers(
        expr = {
          mcmodule <- agg_totals(
            mcmodule = mcmodule,
            mc_name = mc_name,
            agg_keys = agg_keys,
            agg_suffix = hag_suffix,
            keep_variates = keep_variates
          )
        },
        message = function(m) {
          messages <<- c(messages, conditionMessage(m))
          invokeRestart("muffleMessage")
        }
      )
      if (!all(grepl("variates per group for", messages))) {
        message(sprintf("%s", paste(messages, collapse = "; ")))
      }

      # Generate name for aggregated node
      if (!is.null(name) && length(mc_names) == 1) {
        agg_mc_name <- paste0(name, "_", hag_suffix)
        names(mcmodule$node_list)[
          names(mcmodule$node_list) %in% paste0(mc_name, "_", hag_suffix)
        ] <-
          agg_mc_name
      } else {
        agg_mc_name <- paste0(mc_name, "_", hag_suffix)
      }

      # Change mcnode name to agg version name
      mc_name <- agg_mc_name

      # Add metadata
      mcmodule$node_list[[mc_name]][["module"]] <- module_name
      mcmodule$node_list[[mc_name]][["agg_keys"]] <- agg_keys
      mcmodule$node_list[[mc_name]][["keys"]] <- original_keys
      mcmodule$node_list[[mc_name]][["keep_variates"]] <- keep_variates

      # Update keys_names if it does not keep all variates
      if (!keep_variates) {
        keys_names <- agg_keys
      } else {
        keys_names <- unique(c(
          original_keys,
          mcmodule$node_list[[mc_name]][["keys"]]
        ))
      }
    } else {
      keys_names <- unique(c(
        keys_names,
        mcmodule$node_list[[mc_name]][["keys"]]
      ))
    }

    if (!all_equal && mc_name %in% mc_inputs_names) {
      mc_name_matched <- paste0(
        names(mc_inputs_names)[mc_inputs_names %in% mc_name],
        "_mc"
      )
      p_a <- get(mc_name_matched)
    } else {
      p_a <- mcmodule$node_list[[mc_name]][["mcnode"]]
    }

    # If no combined (all) probabilities use new name,
    # else, it was already generated in at_least_one
    # Remove prefix (to avoid prefix duplication)
    prefix <- paste0(sub("_$", "", prefix), "_")
    mc_name_no_prefix <- if (
      length(mc_names) == 1 &&
        !is.null(name)
    ) {
      if (!is.null(agg_keys)) {
        sub(paste0("^", prefix), "", agg_mc_name)
      } else {
        sub(paste0("^", prefix), "", name)
      }
    } else {
      sub(paste0("^", prefix), "", mc_name)
    }
    prefix <- sub("_$", "", prefix)

    # Remove hag_suffix if agg_suffix==""
    if (!is.null(agg_suffix) && agg_suffix == "") {
      mc_name_no_prefix <- sub(
        paste0("_", hag_suffix, "$"),
        "",
        mc_name_no_prefix
      )
    }

    # Process levels
    all_levels <- if (hierarchical_n) {
      if (hierarchical_p) {
        c("trial", "subset", "set")
      } else {
        c("subset", "set")
      }
    } else {
      if (hierarchical_p) {
        c("trial", "set")
      } else {
        c("set")
      }
    }

    for (level in all_levels) {
      # Process probability and number calculations
      for (calc_type in c("prob", "num")) {
        if (level == "trial" && calc_type == "num") {
          next
        }
        calc <- calculations[[level]][[calc_type]]

        new_mc_name <- if (!is.null(prefix) && prefix != "") {
          paste0(prefix, "_", mc_name_no_prefix, calc$suffix)
        } else {
          paste0(mc_name_no_prefix, calc$suffix)
        }

        # Calculate value based on level
        value <- calc$formula(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc)
        total_type <- paste(
          ifelse(multilevel, "multilevel", "single level"),
          level,
          calc_type
        )

        # Create node and add metadata
        mcmodule$node_list <- add_mc_metadata(
          node_list = mcmodule$node_list,
          name = new_mc_name,
          value = value,
          params = if (level == "trial") {
            if (calc_type == "prob") {
              c(mc_name, subsets_p)
            } else {
              c()
            }
          } else if (level == "subset") {
            c(mc_name, trials_n, subsets_p)
          } else {
            c(mc_name, trials_n, subsets_n, subsets_p)
          },
          description = sprintf(calc$description, mc_name),
          expression = if (level == "trial") {
            if (calc_type == "prob") {
              calc$expression(mc_name, subsets_p)
            } else {
              calc$expression(mc_name)
            }
          } else if (level == "subset") {
            calc$expression(mc_name, trials_n, subsets_p)
          } else {
            calc$expression(mc_name, trials_n, subsets_n, subsets_p)
          },
          keys_names = keys_names,
          agg_keys = agg_keys,
          total_type = total_type,
          keep_variates = keep_variates,
          prefix = prefix
        )

        # Add summary if requested
        if (summary && !is.null(data) && nrow(data) > 0) {
          if (!is.null(agg_keys) && !keep_variates) {
            mcmodule$node_list[[new_mc_name]][["summary"]] <- mc_summary(
              mcmodule = mcmodule,
              data = mcmodule$node_list[[mc_name]][["summary"]],
              mc_name = new_mc_name,
              keys_names = agg_keys
            )
          } else {
            mcmodule$node_list[[new_mc_name]][["summary"]] <- mc_summary(
              mcmodule = mcmodule,
              data = data,
              mc_name = new_mc_name,
              keys_names = keys_names[keys_names %in% names(data)]
            )
          }
        }
      }
    }
  }
  return(mcmodule)
}
