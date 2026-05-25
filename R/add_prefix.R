#' Add Prefix to mcnode Names
#'
#' Adds a prefix to node names and their input references. Existing prefixes are
#' preserved to avoid breaking references.
#'
#' @param mcmodule (mcmodule or list). mcmodule object or node_list to prefix.
#' @param prefix (character, optional). Prefix to add to node names; defaults to
#'   mcmodule name. Default: NULL.
#' @param rewrite_module (character, optional). Module name to rewrite prefixes for.
#'   Default: NULL.
#'
#' @return The mcmodule with prefixed node names.
#'
#' @examples
#' print(names(imports_mcmodule$node_list))
#' imports_mcmodule_prefix<-purchase <- add_prefix(imports_mcmodule)
#' print(names(imports_mcmodule_prefix$node_list))
#' @export
add_prefix <- function(mcmodule, prefix = NULL, rewrite_module = NULL) {
  # Extract node list
  node_list <- mcmodule$node_list

  # Assign current mcmodule to nodes without one or when pipe was used and auto named to "."
  for (i in 1:length(node_list)) {
    if (
      is.null(node_list[[i]][["module"]]) ||
        (length(node_list[[i]][["module"]]) == 1 &&
          node_list[[i]][["module"]] == ".")
    ) {
      node_list[[i]][["module"]] <- deparse(substitute(mcmodule))
    }
  }

  # Get mcmodule structure
  info <- mcmodule_info(mcmodule)
  modules <- info$module_names
  exps <- unique(info$module_exp_data$exp)
  exp_and_modules <- unique(c(modules, exps, deparse(substitute(mcmodule))))

  # Get node names and modules
  node_names <- names(node_list)
  node_module <- unlist(sapply(node_list, "[[", "module"))
  # Get node expression names, NA if not from an expression
  node_exp <- unlist(sapply(node_list, function(x) {
    if (!is.null(x[["exp_name"]])) {
      return(x[["exp_name"]])
    } else {
      return(NA)
    }
  }))

  # Get inputs and their modules
  inputs <- unique(unlist(sapply(node_list, "[[", "inputs")))
  inputs_module <- unlist(sapply(node_list[inputs], "[[", "module"))
  inputs_exp <- unlist(sapply(node_list[inputs], "[[", "exp_name"))

  # Set default prefix if none provided
  if (is.null(prefix)) {
    prefix <- deparse(substitute(mcmodule))
  }

  # Handle module rewriting if specified
  nodes_to_reprefix <- NULL
  if (!is.null(rewrite_module)) {
    # Identify nodes that start with rewrite_module prefix
    old_prefix_pattern <- paste0("^", rewrite_module, "_")
    nodes_to_rewrite <- grep(old_prefix_pattern, node_names)
    inputs_to_rewrite <- grep(old_prefix_pattern, inputs)

    # Rename module
    node_module[node_module %in% rewrite_module] <- prefix
    node_exp[node_exp %in% rewrite_module] <- prefix
    inputs_module[inputs_module %in% rewrite_module] <- prefix
    inputs_exp[inputs_exp %in% rewrite_module] <- prefix

    # Remove prefix from identified nodes
    node_names[nodes_to_rewrite] <- gsub(
      paste0(rewrite_module, "_"),
      "",
      node_names[nodes_to_rewrite]
    )
    names(node_module) <- gsub(
      paste0(rewrite_module, "_"),
      "",
      names(node_module)
    )
    names(node_exp) <- gsub(
      paste0(rewrite_module, "_"),
      "",
      names(node_exp)
    )
    inputs[inputs_to_rewrite] <- gsub(
      paste0(rewrite_module, "_"),
      "",
      inputs[inputs_to_rewrite]
    )
    names(inputs_module) <- gsub(
      paste0(rewrite_module, "_"),
      "",
      names(inputs_module)
    )
    names(inputs_exp) <- gsub(
      paste0(rewrite_module, "_"),
      "",
      names(inputs_exp)
    )

    # Store which nodes need re-prefixing after rewrite
    nodes_to_reprefix <- nodes_to_rewrite
  }

  # Add prefix to node names
  node_prefix_index <- which(
    node_module[node_names] %in%
      exp_and_modules |
      node_exp[node_names] %in% exp_and_modules
  )

  # Include nodes that were rewritten
  if (!is.null(nodes_to_reprefix)) {
    node_prefix_index <- unique(c(node_prefix_index, nodes_to_reprefix))
  }

  node_names[node_prefix_index] <- paste0(
    prefix,
    "_",
    node_names[node_prefix_index]
  )

  # Add prefix to inputs
  inputs_prefix_index <- which(
    inputs_module[inputs] %in%
      exp_and_modules |
      inputs_exp[inputs] %in% exp_and_modules
  )

  new_inputs <- inputs

  new_inputs[inputs_prefix_index] <- paste0(
    prefix,
    "_",
    inputs[inputs_prefix_index]
  )

  # Remove duplicated prefixes
  node_names <- gsub(paste0(prefix, "_", prefix), prefix, node_names)
  new_inputs <- gsub(paste0(prefix, "_", prefix), prefix, new_inputs)

  names(new_inputs) <- inputs

  # Update node list inputs and prefix
  for (i in 1:length(node_list)) {
    # Update nodes that belong to this module or its expressions
    node_module_i <- node_list[[i]][["module"]]
    node_exp_i <- node_list[[i]][["exp_name"]]
    node_type_i <- node_list[[i]][["type"]]

    is_current_node <-
      any(node_module_i %in% exp_and_modules) ||
      (!is.null(node_exp_i) && any(node_exp_i %in% exp_and_modules))

    if (is_current_node) {
      old_inputs <- node_list[[i]][["inputs"]]
      node_list[[i]][["inputs"]] <- new_inputs[old_inputs]
      node_list[[i]][["prefix"]] <- prefix
    }
  }

  names(node_list) <- node_names

  # Return appropriate object type
  if (inherits(mcmodule, "mcmodule")) {
    mcmodule$node_list <- node_list
    return(mcmodule)
  } else {
    return(node_list)
  }
}
