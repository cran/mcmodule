#' Evaluate a Monte Carlo Model Expression and create an mcmodule
#'
#' Takes a set of Monte Carlo model expressions and evaluates them and creates an mcmodule
#' containing results and metadata.
#'
#' @param exp Model expression or list of expressions to evaluate
#' @param data Input data frame containing model parameters
#' @param param_names Named vector for parameter renaming (optional)
#' @param prev_mcmodule Previous module(s) for dependent calculations
#' @param summary Logical; whether to calculate summary statistics
#' @param mctable Reference table for mcnodes, defaults to set_mctable()
#' @param data_keys Data structure and keys, defaults to set_data_keys()
#' @param match_keys Keys to match prev_mcmodule mcnodes and data by
#' @param keys Optional explicit keys for the input data (character vector)
#' @param overwrite_keys Logical or NULL. If NULL (default) it becomes TRUE when
#'   data_keys is NULL or an empty list; otherwise FALSE.
#'
#' @return An mcmodule object containing data, expressions, and nodes
#' @export
#'
#' @examples
#' # Basic usage with single expression
#' eval_module(
#'   exp = imports_exp,
#'   data = imports_data,
#'   mctable = imports_mctable,
#'   data_keys = imports_data_keys
#' )
eval_module <- function(
  exp,
  data,
  param_names = NULL,
  prev_mcmodule = NULL,
  summary = FALSE,
  mctable = set_mctable(),
  data_keys = set_data_keys(),
  match_keys = NULL,
  keys = NULL,
  overwrite_keys = NULL
) {
  data_name <- deparse(substitute(data))

  mctable <- check_mctable(mctable)

  # Validate that data is not empty
  if (nrow(data) < 1) {
    stop(sprintf("data '%s' has 0 rows", data_name))
  }

  # Determine default for overwrite_keys when not explicitly provided:
  # - If overwrite_keys is NULL and data_keys is NULL or an empty list -> default TRUE
  # - Otherwise default FALSE
  if (is.null(overwrite_keys)) {
    overwrite_keys <- is.null(data_keys) ||
      (is.list(data_keys) && length(data_keys) == 0)
  }

  # Validate keys argument (must be character vector or NULL)
  if (!is.null(keys) && !is.character(keys)) {
    stop("keys must be a character vector or NULL")
  }

  # If overwrite_keys is TRUE create a local data_keys entry for this data
  # (do not modify global data_keys)
  if (isTRUE(overwrite_keys)) {
    data_keys_local <- list(cols = names(data), keys = keys)
    data_keys <- list()
    data_keys[[data_name]] <- data_keys_local
    if (!is.null(keys)) {
      message(sprintf(
        "data_keys overwritten for %s with keys: %s",
        data_name,
        paste(keys, collapse = ", ")
      ))
    } else {
      message(sprintf("data_keys overwritten for %s", data_name))
    }
    # When overwritten, we do not need to forward keys separately
    keys_arg <- NULL
  } else {
    # Do not merge into existing data_keys; forward explicit keys to get_node_list
    keys_arg <- keys
  }

  # Convert single expression to list format
  if (is.list(exp)) {
    exp_list <- exp
  } else {
    exp_name <- gsub("_exp", "", deparse(substitute(exp)))
    exp_list <- list(exp)
    names(exp_list) <- exp_name
  }

  node_list <- list()
  modules <- c()

  # Process each expression in the list
  for (i in 1:length(exp_list)) {
    exp_i <- exp_list[[i]]
    module <- names(exp_list)[[i]]

    # Get initial node list (forward keys_arg as character vector or NULL)
    node_list_i <- get_node_list(
      exp = exp_i,
      param_names = param_names,
      mctable = mctable,
      data_keys = data_keys,
      keys = keys_arg
    )

    # Identify nodes requiring previous module inputs
    prev_nodes <- names(node_list_i)[grepl("prev_node", node_list_i)]
    prev_nodes <- prev_nodes[!prev_nodes %in% names(node_list)]

    # Process nodes requiring previous module inputs
    if (length(prev_nodes) > 0) {
      if (is.null(prev_mcmodule)) {
        stop(sprintf(
          "prev_mcmodule for %s needed but not provided",
          paste(prev_nodes, collapse = ", ")
        ))
      } else {
        prev_mcmodule_list <- if (inherits(prev_mcmodule, "mcmodule")) {
          list(prev_mcmodule)
        } else {
          prev_mcmodule
        }

        # Previous modules
        for (j in 1:length(prev_mcmodule_list)) {
          prev_mcmodule_i <- prev_mcmodule_list[[j]]

          # Prefix matching for node names
          if (any(!prev_nodes %in% names(prev_mcmodule_i$node_list))) {
            prefixes <- unlist(sapply(
              prev_mcmodule_i$node_list,
              "[[",
              "prefix"
            ))
            if (!is.null(prefixes)) {
              new_names <- sapply(names(prefixes), function(x) {
                gsub(paste0("^", prefixes[x], "_"), "", x)
              })

              original_names <- names(prefixes)
              names(prefixes) <- new_names

              prev_nodes_names <- prev_nodes
              prev_nodes <- ifelse(
                prev_nodes %in% original_names,
                prev_nodes,
                ifelse(
                  is.na(prefixes[prev_nodes]),
                  prev_nodes,
                  paste0(prefixes[prev_nodes], "_", prev_nodes)
                )
              )
              names(prev_nodes) <- prev_nodes_names
              prev_param_names <- prev_nodes
            }
          }
          # Get nodes from previous module
          prev_node_list_i <- get_mcmodule_nodes(
            prev_mcmodule_i,
            mc_names = prev_nodes
          )

          #Check if all prev_nodes are found in prev_mcmodule
          missing_prev_nodes <- prev_nodes[
            !prev_nodes %in% names(prev_node_list_i)
          ]
          if (length(missing_prev_nodes) > 0) {
            stop(sprintf(
              "%s not found in prev_mcmodule",
              paste0(missing_prev_nodes, collapse = ", ")
            ))
          }

          # Process each previous node
          for (k in 1:length(prev_nodes)) {
            mc_name <- prev_nodes[k]
            node_list_i[[mc_name]] <- prev_node_list_i[[mc_name]]
            # Check if previous node is an aggregated node,
            # - if it is NOT an aggregated node it will be matched by its reference data_name
            # - if it is an aggregated node it will be matched by its summary
            if (
              is.null(prev_node_list_i[[mc_name]][["agg_keys"]]) ||
                prev_node_list_i[[mc_name]][["keep_variates"]]
            ) {
              data_name_k <- prev_node_list_i[[mc_name]]$data_name

              if (length(data_name_k) > 1) {
                message(sprintf(
                  "Multiple data_name found for %s: %s. Using summary to match dimensions.",
                  mc_name,
                  paste(data_name_k, collapse = ", ")
                ))
                prev_data <- NULL
              } else {
                prev_data <- prev_mcmodule_i$data[[data_name_k]]
              }

              # Match if previous node data is not equal to new data or if node has multiple data_names
              # IF IT PASSES ALL THE CHECKS IT ALREADY MATCHES AND NO MATCH IS NEEDED
              if (
                is.null(prev_data) ||
                  !(nrow(prev_data) == nrow(data) &&
                    ncol(prev_data) == ncol(data) &&
                    nrow(prev_data) ==
                      dim(prev_mcmodule$node_list[[mc_name]]$mcnode)[[3]] &&
                    all(names(prev_data) == names(data)) &&
                    all(prev_data == data, na.rm = TRUE))
              ) {
                match_prev <- mc_match_data(
                  prev_mcmodule,
                  mc_name,
                  data,
                  keys_names = match_keys
                )
                match_prev_mcnode <- match_prev[[1]]
                data <- match_prev[["data_match"]]

                assign(mc_name, match_prev_mcnode)
              }
            } else {
              # Match previous aggregated node with current data and update data
              agg_keys <- prev_node_list_i[[mc_name]][["agg_keys"]]

              if (!is.null(match_keys)) {
                if (!all(agg_keys %in% match_keys)) {
                  message(sprintf(
                    "Using match_keys (%s) instead of: %s",
                    paste(match_keys, collapse = ", "),
                    paste(agg_keys, collapse = ", ")
                  ))
                  agg_keys <- match_keys
                }
              }

              message(sprintf(
                "Matching agg prev_nodes dimensions by: %s",
                paste(agg_keys, collapse = ", ")
              ))

              match_agg_prev <- mc_match_data(
                mcmodule = prev_mcmodule,
                mc_name = mc_name,
                data = data,
                keys_names = agg_keys
              )

              match_prev_mcnode <- match_agg_prev[[1]]
              data <- match_agg_prev[["data_match"]]

              assign(mc_name, match_prev_mcnode)
            }
          }
        }
      }
    }

    # Combine node lists
    node_list <- c(node_list, node_list_i)

    # Update parameter names
    new_param_names <- if (exists("prev_param_names")) {
      c(param_names, prev_param_names)
    } else {
      param_names
    }

    # Handle parameter renaming
    if (!is.null(new_param_names)) {
      for (j in 1:length(new_param_names)) {
        exp_name <- names(new_param_names)[j]
        param_name <- new_param_names[j]

        if (exists(param_name)) {
          assign(exp_name, get(param_name))
        } else if (!is.null(prev_mcmodule$node_list[[param_name]])) {
          assign(
            exp_name,
            prev_mcmodule$node_list[[param_name]][["mcnode"]]
          )
        }
      }
    }

    # Create mcnodes for the current expression
    mctable_i = mctable[
      mctable$mcnode %in% names(node_list_i)[grepl("in_node", node_list_i)],
    ]
    if (nrow(mctable_i) > 0) {
      create_mcnodes(data = data, mctable = mctable_i)
    }

    # Evaluate current expression
    eval(exp_i)
    message(sprintf("%s evaluated", module))

    # Update node metadata
    for (j in 1:length(node_list)) {
      mc_name <- names(node_list)[j]

      if (mc_name %in% prev_nodes) {
        next
      }

      # Update input references
      inputs <- node_list[[mc_name]][["inputs"]]
      node_list[[mc_name]][["param"]] <- inputs

      inputs[inputs %in% names(new_param_names)] <-
        new_param_names[inputs[inputs %in% names(new_param_names)]]
      node_list[[mc_name]][["inputs"]] <- inputs

      # Update keys for output nodes
      if (
        ((!is.null(prev_mcmodule)) | (length(exp) > 1)) &
          node_list[[mc_name]][["type"]] == "out_node"
      ) {
        keys_names <- unique(unlist(lapply(inputs, function(x) {
          if (is.null(node_list[[x]][["agg_keys"]])) {
            node_list[[x]][["keys"]]
          } else {
            node_list[[x]][["agg_keys"]]
          }
        })))
        node_list[[mc_name]][["keys"]] <- keys_names
      }

      # Scalar to mcnode conversion
      mcnode <- get(mc_name)
      if (!is.mcnode(mcnode) & is.numeric(mcnode)) {
        mcnode <- mcdata(mcnode, type = "0", nvariates = length(mcnode))
      }

      # Update node metadata
      node_list[[mc_name]][["mcnode"]] <- mcnode
      node_list[[mc_name]][["data_name"]] <- data_name
      node_list[[mc_name]][["mc_name"]] <- mc_name

      # Set module name
      if (
        length(node_list[[mc_name]][["module"]]) == 0 ||
          node_list[[mc_name]][["module"]] %in% "exp_i"
      ) {
        node_list[[mc_name]][["module"]] <- module
      }

      modules <- unique(c(modules, node_list[[mc_name]][["module"]]))

      # Calculate summary statistics if requested
      if (summary & is.mcnode(mcnode)) {
        inputs_names <- node_list[[mc_name]][["inputs"]]

        keys_names <- if (is.null(node_list[[mc_name]][["agg_keys"]])) {
          node_list[[mc_name]][["keys"]]
        } else {
          node_list[[mc_name]][["agg_keys"]]
        }

        node_summary <- mc_summary(
          data = data,
          mcnode = mcnode,
          mc_name = mc_name,
          keys_names = keys_names
        )

        node_list[[mc_name]][["summary"]] <- node_summary
      }

      # Add scenario information if available
      if ("scenario_id" %in% names(data)) {
        node_list[[mc_name]][["scenario"]] <- data$scenario_id
        if ("hg" %in% names(data)) {
          node_list[[mc_name]][["hg"]] <- data$hg
        }
      }
    }
  }

  # Remove temporary previous nodes
  node_list <- node_list[!(sapply(node_list, "[[", "type") == "prev_node")]

  # Return results
  mcmodule <- list(
    data = list(data),
    exp = exp,
    node_list = node_list,
    modules = modules
  )

  names(mcmodule$data) <- data_name
  class(mcmodule) <- "mcmodule"

  message(sprintf(
    "mcmodule created (expressions: %s)",
    paste(names(exp), collapse = ", ")
  ))

  return(mcmodule)
}


#' Get Nodes from Monte Carlo Module
#'
#' Retrieves nodes from a Monte Carlo module and assigns them to the parent environment
#'
#' @param mcmodule An mcmodule or mcnode_list object
#' @param mc_names Optional vector of node names to retrieve
#' @param envir Environment where MC nodes will be created (default: parent.frame())
#'
#' @return A subset of the node list containing requested nodes
get_mcmodule_nodes <- function(
  mcmodule,
  mc_names = NULL,
  envir = parent.frame()
) {
  if (inherits(mcmodule, "mcmodule")) {
    node_list <- mcmodule$node_list
  } else if (inherits(mcmodule, "mcnode_list")) {
    node_list <- mcmodule
  } else {
    stop("mcmodule or mcnode_list object must be provided")
  }

  all_mc_names <- names(node_list)
  mc_names <- all_mc_names[all_mc_names %in% mc_names]

  if (length(mc_names) > 0) {
    for (i in 1:length(mc_names)) {
      mc_name <- mc_names[i]
      assign(mc_name, node_list[[mc_name]][["mcnode"]], envir = envir)
    }
  }

  return(node_list[mc_names])
}
