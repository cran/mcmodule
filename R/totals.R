#' Combined Probability of Events (At least one)
#'
#' Combines probabilities of multiple events assuming independence,
#' using the formula P(A or B) = 1 - (1-P(A))*(1-P(B)).
#' It matches dimensions automatically.
#'
#' @param mcmodule Module containing node list and input data frames
#' @param mc_names Vector of node names to combine
#' @param name Optional custom name for combined node (default: NULL)
#' @param all_suffix Suffix for combined node name (default: "all")
#' @param prefix Optional prefix for output node name (default: NULL)
#' @param summary Whether to calculate summary statistics (default: TRUE)
#'
#' @return Updated mcmodule with new combined probability node
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
at_least_one <- function(mcmodule,
                         mc_names,
                         name = NULL,
                         all_suffix = NULL,
                         prefix = NULL,
                         summary = TRUE) {
  module_name <- deparse(substitute(mcmodule))

  # Check if mcnodes are in mcmodule
  missing_nodes <- mc_names[!mc_names %in% names(mcmodule$node_list)]

  if (length(missing_nodes) > 0) {
    stop(paste(
      "Nodes",
      paste(missing_nodes, collapse = ", "),
      "not found in",
      module_name
    ))
  }


  nodes_data_name <- sapply(mc_names, function(x)
    mcmodule$node_list[[x]][["data_name"]])
  data_name <- unique(nodes_data_name)
  nodes_dim <- sapply(mc_names, function(x)
    dim(mcmodule$node_list[[x]][["mcnode"]])[3])
  nodes_agg <- sapply(mc_names, function(x)
    ! is.null(mcmodule$node_list[[x]][["agg_keys"]]))
  nodes_keys <- lapply(mc_names, function(x)
    mc_keys(mcmodule, x))
  names(nodes_keys) <- mc_names


  p_all <- 0
  keys_names <- c()

  # Check that data_name, dimensions and keys are identical for all nodes
  if (length(data_name) == 1 &&
      length(unique(nodes_dim)) == 1 &&
      all(!nodes_agg) &&
      length(unique(nodes_keys)) == 1) {

    data <- nodes_keys[[1]]

    # Loop to get the combined probability of all mcnodes
    for (i in seq_along(mc_names)) {
      mc_name <- mc_names[i]
      p_i <- mcmodule$node_list[[mc_name]][["mcnode"]]
      keys_names <- unique(c(keys_names, names(nodes_keys[[i]])))

      # Update combined probability
      p_all <- 1 - ((1 - p_all) * (1 - p_i))
    }

  } else {
    if (!length(mc_names) == 2) {
      stop("To aggregate mc_names with different data_name or keys, provide exactly two mc_nodes")
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
    prefix<-paste0(sub("_$","",prefix),"_")
    p_all_mc_name <- paste0(prefix, sub(paste0("^", prefix), "", p_all_mc_name))
    prefix<-sub("_$","",prefix)
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
    node_expression = paste0("1-(", paste(
      paste("(1-", mc_names, ")", sep = ""), collapse = "*"
    ), ")"),
    scenario = data$scenario_id,
    data_name = data_name,
    prefix = prefix
  )

  # Get agg keys (if nodes are aggregated)
  if (any(nodes_agg)) {
    mcmodule$node_list[[p_all_mc_name]][["agg_keys"]] <- unique(unlist(lapply(mc_names, function(x)
      mcmodule$node_list[[x]][["agg_keys"]])))
    mcmodule$node_list[[p_all_mc_name]][["keep_variates"]] <- all(unlist(lapply(mc_names, function(x)
      mcmodule$node_list[[x]][["keep_variates"]])))
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
  if (is.null(all_suffix))
    all_suffix <- "all"
  # Check if "all" is already in any input
  if (any(grepl(paste0("_", all_suffix, "$"), mc_names))) {
    stop("One of the mc_names already contains '",
         paste0("_", all_suffix),
         "' suffix")
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

#' Aggregate Across Groups
#'
#' Combines node values across specified grouping variables using different aggregation methods.
#' The aggregation method can be specified via agg_func parameter:
#' - "prob": Combined probability assuming independence
#' - "sum": Sum of values
#' - "avg": Average of values
#' - NULL: defaults to "sum" if mc_name ends in "_n", else defaults to "prob"
#' @param mcmodule mcmodule object containing nodes and data
#' @param mc_name name of node to aggregate
#' @param agg_keys grouping variables for aggregation
#' @param agg_suffix Suffix for aggregated node name (default: "agg")
#' @param prefix Optional prefix for output node name - includes metadata as add_prefix() (default: NULL)
#' @param name Custom name for output node (optional)
#' @param summary whether to include summary statistics (default: TRUE)
#' @param keep_variates whether to preserve individual values (default: FALSE)
#' @param agg_func aggregation method ("prob", "sum", "avg", or NULL)
#'
#'
#' @return mcmodule with new aggregated node added
#'
#' @examples
#' imports_mcmodule <- agg_totals(
#'   imports_mcmodule, "no_detect_a",
#'   agg_keys = c("scenario_id", "pathogen")
#' )
#' print(imports_mcmodule$node_list$no_detect_a_agg$summary)
#' @export
agg_totals <- function(mcmodule,
                       mc_name,
                       agg_keys = NULL,
                       agg_suffix = NULL,
                       prefix = NULL,
                       name = NULL,
                       summary = TRUE,
                       keep_variates = FALSE,
                       agg_func = NULL) {
  module_name <- deparse(substitute(mcmodule))

  # Check if mcnode is in mcmodule
  if (!mc_name %in% names(mcmodule$node_list)) {
    stop(mc_name, " not found in ", module_name)
  }


  if (!(is.null(agg_func) ||
        agg_func %in% c("prob", "avg", "sum")))
    stop("Aggregation function must be prob, avg, sum or NULL")
  if (is.null(agg_keys)) {
    agg_keys <- "scenario_id"
    message("Keys to aggregate by not provided, using 'scenario_id' by default")
  }
  # Extract module name and node data
  mcnode <- mcmodule$node_list[[mc_name]][["mcnode"]]
  key_col <- mc_keys(mcmodule, mc_name, agg_keys)
  data_name <- mcmodule$node_list[[mc_name]][["data_name"]]

  # Generate name for aggregated node
  agg_mc_name <- if (is.null(name)) {
    if (is.null(agg_suffix))
      agg_suffix <- "agg"
    paste0(mc_name, "_", agg_suffix)
  } else if (is.null(agg_suffix)) {
    name
  } else {
    paste0(name, "_", agg_suffix)
  }

  # Add prefix if provided
  if (!is.null(prefix) && prefix != "") {
    prefix<-paste0(sub("_$","",prefix),"_")
    agg_mc_name <- paste0(prefix, "_" , sub(paste0("^", prefix), "", agg_mc_name))
    prefix<-sub("_$","",prefix)
  }

  # Extract variates
  variates_list <- list()
  inv_variates_list <- list()
  for (i in seq_len(dim(mcnode)[3])) {
    variates_list[[i]] <- mc2d::extractvar(mcnode, i)
    inv_variates_list[[i]] <- 1 - mc2d::extractvar(mcnode, i)
  }

  # Create grouping index
  key_col$key <- do.call(paste, c(key_col, sep = ", "))
  key_levels <- unique(key_col$key)

  # Process each group
  for (i in seq_along(key_levels)) {
    index <- key_col$key %in% key_levels[i]

    if (!is.null(agg_func) && agg_func == "avg") {
      # Calculate average value
      total_lev <- Reduce("+", variates_list[index]) / sum(index)
    } else if ((is.null(agg_func) &&
                grepl("_n$", mc_name)) ||
               (!is.null(agg_func) && agg_func == "sum")) {
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
  } else if ((is.null(agg_func) &&
              grepl("_n$", mc_name)) ||
             (!is.null(agg_func) && agg_func == "sum")) {
    # Sum for counts
    mcmodule$node_list[[agg_mc_name]][["description"]] <-
      paste0("Sum by: ", paste0(agg_keys, collapse = ", "))
    mcmodule$node_list[[agg_mc_name]][["node_expression"]] <-
      paste0(mc_name,
             "_1+",
             mc_name,
             "_2+... by: ",
             paste0(agg_keys, collapse = ", "))
  } else {
    # Combine probabilities
    mcmodule$node_list[[agg_mc_name]][["description"]] <-
      paste0("Combined probability assuming independence by: ",
             paste0(agg_keys, collapse = ", "))
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

  if (!is.null(prefix))
    mcmodule$node_list[[agg_mc_name]][["prefix"]] <- prefix

  if (summary) {
    mcmodule$node_list[[agg_mc_name]][["summary"]] <-
      mc_summary(
        mcmodule = mcmodule,
        data = key_data,
        mc_name = agg_mc_name,
        keys_names = new_agg_keys
      )
  }

  mcmodule$modules <- unique(c(mcmodule$modules, module_name))
  return(mcmodule)
}


#' Trial Probability and Expected Counts
#'
#' Calculates probabilities and expected counts across hierarchical levels
#' (trial, subset, set) in a structured population. Uses trial probabilities and
#' handles nested sampling with conditional probabilities.
#'
#' @param mcmodule mcmodule object containing input data and node structure
#' @param mc_names Vector of node names to process
#' @param trials_n Trial count column name
#' @param subsets_n Subset count column name (optional)
#' @param subsets_p Subset prevalence column name (optional)
#' @param name Custom name for output nodes (optional)
#' @param prefix Prefix for output node names (optional)
#' @param combine_prob Combine probability of all nodes assuming independence (default: TRUE)
#' @param all_suffix Suffix for combined node name (default: "all")
#' @param level_suffix A list of suffixes for each hierarchical level (default: c(trial="trial",subset="subset",set="set"))
#' @param mctable Data frame containing Monte Carlo nodes definitions (default: set_mctable())
#' @param agg_keys Column names for aggregation (optional)
#' @param agg_suffix Suffix for aggregated node names (default: "hag")
#' @param keep_variates whether to preserve individual values (default: FALSE)
#' @param summary Include summary statistics if TRUE (default: TRUE)
#'
#' @return
#' Updated mcmodule object containing:
#' - Combined node probabilities
#' - Probabilities and counts at trial level
#' - Probabilities and counts at subset level
#' - Probabilities and counts at set level
#'
#' @examples
#' imports_mcmodule <- trial_totals(
#'   mcmodule = imports_mcmodule,
#'   mc_names = "no_detect_a",
#'   trials_n = "animals_n",
#'   subsets_n = "farms_n",
#'   subsets_p = "h_prev",
#'   mctable = imports_mctable
#' )
#' print(imports_mcmodule$node_list$no_detect_a_set$summary)
#' @export
trial_totals <- function(mcmodule,
                         mc_names,
                         trials_n,
                         subsets_n = NULL,
                         subsets_p = NULL,
                         name = NULL,
                         prefix = NULL,
                         combine_prob = TRUE,
                         all_suffix = NULL,
                         level_suffix = c(trial = "trial",
                                          subset = "subset",
                                          set = "set"),
                         mctable = set_mctable(),
                         agg_keys = NULL,
                         agg_suffix = NULL,
                         keep_variates = FALSE,
                         summary = TRUE) {

  module_name <- deparse(substitute(mcmodule))

  # Check if mcnodes are in mcmodule
  missing_nodes <- mc_names[!mc_names %in% names(mcmodule$node_list)]

  if (length(missing_nodes) > 0) {
    stop(paste(
      "Nodes",
      paste(missing_nodes, collapse = ", "),
      "not found in",
      module_name
    ))
  }

  nodes_data_name <- lapply(mc_names, function(x) {
    mcmodule$node_list[[x]][["data_name"]]
  })

  # Get the first node's data_name as reference
  reference_data_name <- sort(nodes_data_name[[1]])

  # Check if all nodes have the same set of data_names
  all_equal <- all(sapply(nodes_data_name, function(x) {
    identical(sort(x), reference_data_name)
  }))

  if (!all_equal) {
    stop("data_name is not equal for all nodes")
  }

  data_name <- reference_data_name

  if (!is.null(name) &&
      length(mc_names) > 1 &&
      !combine_prob)
    stop("name argument can only be used when mc_names length is 1 or when combine_prob is TRUE")

  if (!all(names(level_suffix) %in% c("trial", "subset", "set")))
    stop(
      "Suffixes for each hierarchical level must be defined as a named vector with the following structure: c(trial = '...', subset = '...', set = '...')"
    )

  # Fix missing level suffixes
  missing_suffixes <- setdiff(c("trial", "subset", "set"), names(level_suffix))
  for (suffix in missing_suffixes) {
    level_suffix[suffix] <- suffix
  }

  hag_suffix <- if(is.null(agg_suffix)||agg_suffix=="") "hag" else agg_suffix

  data <- mcmodule$data[[data_name]]

  # Function for individual mcnode creation and processing
  process_trial_mcnode <- function(mc_name,
                             node_type,
                             mcmodule,
                             data,
                             module_name,
                             agg_keys,
                             hag_suffix,
                             mctable,
                             keep_variates,
                             agg_func = NULL) {
    if (mc_name %in% names(mcmodule$node_list)) {
      mc_node <- mcmodule$node_list[[mc_name]][["mcnode"]]
    } else {
      if (!mc_name %in% mctable$mcnode)
        stop(mc_name, " not found in mctable")

      mc_row <- mctable[mctable$mcnode %in% mc_name, ]

      create_mcnodes(data, mctable = mc_row)

      mc_node <- get(mc_name)

      # Add metadata
      pattern <- paste0("\\<", mc_name, "(\\>|[^>]*\\>)")
      inputs_col <- names(data[grepl(pattern, names(data))])
      mcmodule$node_list[[mc_name]][["inputs_col"]] <- inputs_col

      if (!is.na(mc_row$mc_func)) {
        mcmodule$node_list[[mc_name]][["mc_func"]] <- as.character(mc_row$mc_func)
      }

      mcmodule$node_list[[mc_name]][["description"]] <- as.character(mc_row$description)
      mcmodule$node_list[[mc_name]][["type"]] <- node_type
      mcmodule$node_list[[mc_name]][["module"]] <- module_name
      mcmodule$node_list[[mc_name]][["data_name"]] <- data_name
      mcmodule$node_list[[mc_name]][["mcnode"]] <- mc_node
      mcmodule$node_list[[mc_name]][["mc_func"]] <- mc_row$mc_func

      if ("scenario_id" %in% names(data)) {
        mcmodule$node_list[[mc_name]][["scenario"]] <- data$scenario_id
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
      if (!all(grepl("variates per group for", messages)))
        message(messages)
      # Change mcnode name to agg version name
      mc_name_name <- deparse(substitute(mc_name))
      agg_mc_name <-  paste0(mc_name, "_", hag_suffix)
      assign(mc_name_name, agg_mc_name, envir = parent.frame())
      # Add agg_keys to metadata
      mcmodule$node_list[[agg_mc_name]][["agg_keys"]] <- agg_keys
      # Reassign mcmodule name (defaults to "mcmodule")
      mcmodule$node_list[[agg_mc_name]][["module"]] <- module_name
      mcmodule$node_list[[agg_mc_name]][["keep_variates"]] <- keep_variates
    }
    return(mcmodule)
  }

  # Process all nodes

  mcmodule <- process_trial_mcnode(
    trials_n,
    "trials_n",
    mcmodule,
    data,
    module_name,
    agg_keys,
    hag_suffix,
    mctable,
    keep_variates
  )
  trials_n_mc <- mcmodule$node_list[[trials_n]][["mcnode"]]


  # If subsets_n is NULL, defaults to 1
  if (is.null(subsets_n)) {
    subsets_n_mc <- mcnode_na_rm(trials_n_mc / trials_n_mc, 1)
    subsets_n <- "1"
    hierarchical_n <- FALSE
  } else {
    mcmodule <- process_trial_mcnode(
      subsets_n,
      "subsets_n",
      mcmodule,
      data,
      module_name,
      agg_keys,
      hag_suffix,
      mctable,
      keep_variates,
      agg_func = "avg"
    )
    subsets_n_mc <- mcmodule$node_list[[subsets_n]][["mcnode"]]
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
      "subsets_p",
      mcmodule,
      data,
      module_name,
      agg_keys,
      hag_suffix,
      mctable,
      keep_variates,
      agg_func = "avg"
    )
    subsets_p_mc <- mcmodule$node_list[[subsets_p]][["mcnode"]]
    hierarchical_p <- TRUE
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

    mc_names <- c(mc_names, p_all_mc_name)
  }

  # Helper function to add metadata to nodes
  add_mc_metadata <- function(node_list,
                              name,
                              value,
                              params,
                              description,
                              expression,
                              type = "total",
                              keys_names,
                              agg_keys,
                              total_type,
                              keep_variates) {
    node_list[[name]] <- list(
      mcnode = value,
      param = params,
      inputs = params,
      description = description,
      node_expression = expression,
      type = type,
      module = module_name,
      keys = keys_names,
      scenario = data$scenario_id,
      data_name = data_name,
      prefix = prefix,
      total_type = total_type
    )

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
        formula = function(p_a,
                           trials_n_mc,
                           subsets_n_mc,
                           subsets_p_mc)
          p_a * subsets_p_mc,
        description = paste0("Probability of one %s trial (", level_suffix[["trial"]], ")"),
        suffix = paste0("_", level_suffix[["trial"]]),
        expression = function(mc_name, subsets_p)
          paste0(subsets_p, "*", mc_name)
      ),
      num = list(
        formula = function(p_a,
                           trials_n_mc,
                           subsets_n_mc,
                           subsets_p_mc)
          mcnode_na_rm(p_a / p_a, 1),
        description = paste0("One %s trials (", level_suffix[["trial"]], ")"),
        suffix = paste0("_", level_suffix[["trial"]], "_n"),
        expression = function(mc_name)
          paste0("mcnode_na_rm(", mc_name, "/", mc_name, ", 1)")
      )
    ),
    subset = list(
      prob = list(
        formula = function(p_a,
                           trials_n_mc,
                           subsets_n_mc,
                           subsets_p_mc)
          1 - (1 - subsets_p_mc * (1 - (1 - p_a) ^ trials_n_mc)),
        description = paste0("Probability of at least one %s in a subset (", level_suffix[["subset"]], ")"),
        suffix = paste0("_", level_suffix[["subset"]]),
        expression = function(mc_name, trials_n, subsets_p)
          paste0("1-(1-", subsets_p, "*(1-(1-", mc_name, ")^", trials_n, "))")
      ),
      num = list(
        formula = function(p_a,
                           trials_n_mc,
                           subsets_n_mc,
                           subsets_p_mc) {
          p_a * trials_n_mc * subsets_p_mc
        },
        description = paste0("Expected number of %s in a subset (", level_suffix[["subset"]], ")"),
        suffix = paste0("_", level_suffix[["subset"]], "_n"),
        expression = function(mc_name, trials_n, subsets_p) {
          paste0(mc_name, "*", trials_n, "*", subsets_p)
        }
      )
    ),
    set = list(
      prob = list(
        formula = function(p_a,
                           trials_n_mc,
                           subsets_n_mc,
                           subsets_p_mc) {
          1 - (1 - subsets_p_mc * (1 - (1 - p_a) ^ trials_n_mc)) ^ subsets_n_mc
        },
        description = paste0("Probability of at least one %s in a set (", level_suffix[["set"]], ")"),
        suffix = paste0("_", level_suffix[["set"]]),
        expression = function(mc_name, trials_n, subsets_n, subsets_p) {
          paste0("1-(1-",
                 subsets_p,
                 "*(1-(1-",
                 mc_name,
                 ")^",
                 trials_n,
                 "))^",
                 subsets_n)
        }
      ),
      num = list(
        formula = function(p_a,
                           trials_n_mc,
                           subsets_n_mc,
                           subsets_p_mc) {
          p_a * trials_n_mc * subsets_p_mc * subsets_n_mc
        },
        description = paste0("Expected number of %s in a set (", level_suffix[["set"]], ")"),
        suffix = paste0("_", level_suffix[["set"]], "_n"),
        expression = function(mc_name, trials_n, subsets_n, subsets_p) {
          paste0(mc_name, "*", trials_n, "*", subsets_p, "*", subsets_n)
        }
      )
    )
  )

  # Process each node
  for (mc_name in mc_names) {
    if (!is.null(agg_keys)) {
      keys_names <- mcmodule$node_list[[mc_name]][["keys"]]
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
      if (!all(grepl("variates per group for", messages)))
        message(messages)

      # Generate name for aggregated node
      if (!is.null(name) && length(mc_names) == 1) {
        agg_mc_name <- paste0(name, "_", hag_suffix)
        names(mcmodule$node_list)[names(mcmodule$node_list) %in% paste0(mc_name, "_", hag_suffix)] <-
          agg_mc_name
      } else {
        agg_mc_name <- paste0(mc_name, "_", hag_suffix)
      }

      # Change mcnode name to agg version name
      mc_name <- agg_mc_name

      # Add metadata
      mcmodule$node_list[[mc_name]][["module"]] <- module_name
      mcmodule$node_list[[mc_name]][["agg_keys"]] <- agg_keys
      mcmodule$node_list[[mc_name]][["keys"]] <- keys_names
      mcmodule$node_list[[mc_name]][["keep_variates"]] <- keep_variates

      # Update keys_names if it does not keep all variates
      if (!keep_variates)
        keys_names <- agg_keys
    } else {
      keys_names <- mcmodule$node_list[[mc_name]][["keys"]]
    }

    p_a <- mcmodule$node_list[[mc_name]][["mcnode"]]

    # If no combined (all) probabilities use new name,
    # else, it was already generated in at_least_one
    # Remove prefix (to avoid prefix duplication)
    prefix<-paste0(sub("_$","",prefix),"_")
    mc_name_no_prefix  <- if (length(mc_names) == 1 &&
                              !is.null(name)) {
      if(!is.null(agg_keys)){
        sub(paste0("^", prefix), "", agg_mc_name)
      }else{
        sub(paste0("^", prefix), "", name)
      }
    } else{
       sub(paste0("^", prefix), "", mc_name)
    }
    prefix<-sub("_$","",prefix)

    # Remove hag_suffix if agg_suffix==""
    if(!is.null(agg_suffix)&&agg_suffix==""){
      mc_name_no_prefix<- sub(paste0("_",hag_suffix,"$"), "", mc_name_no_prefix)
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
        if (level == "trial" && calc_type == "num")
          next
        calc <- calculations[[level]][[calc_type]]

        new_mc_name <- if (!is.null(prefix) && prefix != "") {
          paste0(prefix, "_", mc_name_no_prefix, calc$suffix)
        } else {
          paste0(mc_name_no_prefix, calc$suffix)

        }

        # Calculate value based on level
        value <- calc$formula(p_a, trials_n_mc, subsets_n_mc, subsets_p_mc)
        total_type <- paste(ifelse(multilevel, "multilevel", "single level"),
                            level,
                            calc_type)

        # Create node and add metadata
        mcmodule$node_list <- add_mc_metadata(
          node_list = mcmodule$node_list,
          name = new_mc_name,
          value = value,
          params = if (level == "trial") {
            if (calc_type == "prob")
              c(mc_name, subsets_p)
            else
              c()
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
          keep_variates = keep_variates
        )

        # Add summary if requested
        if (summary) {
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
              keys_names = keys_names
            )
          }
        }
      }
    }
  }

  mcmodule$modules <- unique(c(mcmodule$modules, module_name))
  return(mcmodule)
}
