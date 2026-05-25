#' Create Node List from Model Expression
#'
#' Creates a list of nodes based on a given model expression, handling input,
#' output, and previous nodes with their properties and relationships.
#'
#' @param exp An R expression containing model calculations
#' @param param_names Optional named vector for parameter renaming
#' @param mctable Reference table for mcnodes, defaults to set_mctable()
#' @param data_keys Data structure and keys, defaults to set_data_keys()
#' @param keys Optional explicit keys for the input data (character vector)
#'
#' @return A list of class "mcnode_list" containing node information
get_node_list <- function(
  exp,
  param_names = NULL,
  mctable = set_mctable(),
  data_keys = set_data_keys(),
  keys = NULL
) {
  # Validate that exp is a quoted expression (use quote({ ... }))
  if (!(is.call(exp) || is.expression(exp) || is.language(exp))) {
    stop("exp must be a quoted expression, use quote({ ... })")
  }

  exp_name <- gsub("_exp", "", deparse(substitute(exp)))

  # Initialize lists and vectors
  out_node_list <- list()
  all_nodes <- c()

  # Process output nodes from model exp
  for (i in 2:length(exp)) {
    node_name <- deparse(exp[[i]][[2]])
    node_exp <- paste0(deparse(exp[[i]][[3]]), collapse = "")

    # Use AST parser
    parse_res <- ast_traverse(node_exp)
    inputs <- parse_res$inputs

    if (length(parse_res$unsupported_types) > 0) {
      warning(
        sprintf(
          "mcdata/mcstoc calls with type(s) %s are not fully supported by this mcmodule version, downstream compatibility is not guaranteed",
          paste(unique(parse_res$unsupported_types), collapse = ", ")
        )
      )
    }

    if (parse_res$created_in_exp) {
      out_node_list[[node_name]][["created_in_exp"]] <- TRUE

      if (parse_res$nvariates) {
        stop(
          "Remove 'nvariates' argument from:\n   ",
          node_exp,
          "\nNumber of variates is determined automatically based on input data rows"
        )
      }

      if (!is.null(parse_res$mc_func)) {
        out_node_list[[node_name]][["mc_func"]] <- parse_res$mc_func
      }
    }

    if (parse_res$na_rm) {
      out_node_list[[node_name]][["na_rm"]] <- TRUE
    }
    if (parse_res$function_call) {
      out_node_list[[node_name]][["function_call"]] <- TRUE
    }
    if (length(parse_res$null_rm_inputs) > 0) {
      out_node_list[[node_name]][["null_rm_inputs"]] <- parse_res$null_rm_inputs
    }

    # Collect node names and inputs
    all_nodes <- unique(c(all_nodes, inputs, node_name))

    # Set node type: numeric literal -> scalar; otherwise an output node that may depend on inputs
    out_node_list[[node_name]][["type"]] <-
      if (!grepl("[[:alpha:]]", node_exp) && !is.na(as.numeric(node_exp))) {
        "scalar"
      } else {
        "out_node"
      }

    out_node_list[[node_name]][["node_exp"]] <- node_exp
    out_node_list[[node_name]][["inputs"]] <- inputs
    out_node_list[[node_name]][["exp_name"]] <- exp_name
    out_node_list[[node_name]][["mc_name"]] <- node_name
  }

  # Rename parameters
  for (i in 1:length(all_nodes)) {
    all_nodes[i] <- if (all_nodes[i] %in% names(param_names)) {
      param_names[all_nodes[i]]
    } else {
      all_nodes[i]
    }
  }

  # Process input nodes
  in_node_list <- list()
  input_nodes <- all_nodes[all_nodes %in% as.character(mctable$mcnode)]

  # Build list of column names per dataset (safe when data_keys NULL or empty)
  if (is.null(data_keys) || length(data_keys) == 0) {
    all_inputs <- list()
  } else {
    all_inputs <- lapply(data_keys, function(x) {
      if (!is.null(x$cols)) {
        return(x$cols)
      }
      return(NULL)
    })
  }

  null_rm_inputs <- unique(unlist(lapply(out_node_list, function(x) {
    if (!is.null(x[["null_rm_inputs"]])) {
      return(x[["null_rm_inputs"]])
    }
    return(NULL)
  })))

  if (length(input_nodes) > 0) {
    for (i in 1:length(input_nodes)) {
      node_name <- input_nodes[[i]]
      mc_row <- mctable[mctable$mcnode == node_name, ]

      in_node_list[[node_name]][["type"]] <- "in_node"

      if (!is.na(mc_row$mc_func)) {
        in_node_list[[node_name]][["mc_func"]] <- as.character(mc_row$mc_func)
      }

      in_node_list[[node_name]][["description"]] <- as.character(
        mc_row$description
      )

      matched_dataset <- NULL

      # Process input columns and datasets
      for (dataset_name in names(all_inputs)) {
        if (is.null(mc_row$from_variable) || is.na(mc_row$from_variable)) {
          # Get matching input columns for current node
          pattern <- paste0("\\<", node_name, "(\\>|[^>]*\\>)")
          inputs_col <- all_inputs[[dataset_name]][grepl(
            pattern,
            all_inputs[[dataset_name]]
          )]
        } else {
          # Get matching input columns from transformed variable
          pattern <- paste0("\\<", mc_row$from_variable, "(\\>|[^>]*\\>)")
          inputs_col <- all_inputs[[dataset_name]][grepl(
            pattern,
            all_inputs[[dataset_name]]
          )]
          # Add transformation info to node list
          if (!is.null(mc_row$from_variable) && !is.na(mc_row$transformation)) {
            in_node_list[[node_name]][[
              "transformation"
            ]] <- mc_row$transformation
          }
        }

        # Update node list if matching inputs found
        if (length(inputs_col) > 0) {
          matched_dataset <- dataset_name
          in_node_list[[node_name]][["inputs_col"]] <- inputs_col
          in_node_list[[node_name]][["input_dataset"]] <- dataset_name

          # Determine dataset base keys from data_keys (if available)
          base_keys <- NULL
          if (
            !is.null(data_keys) &&
              dataset_name %in% names(data_keys) &&
              !is.null(data_keys[[dataset_name]][["keys"]])
          ) {
            base_keys <- data_keys[[dataset_name]][["keys"]]
          }

          # If explicit keys provided: merge with base_keys if base_keys exist; otherwise use explicit keys
          if (!is.null(keys)) {
            if (!is.character(keys)) {
              stop("keys must be a character vector")
            }
            final_keys <- if (!is.null(base_keys)) {
              unique(c(base_keys, keys))
            } else {
              keys
            }
          } else {
            final_keys <- base_keys
          }

          in_node_list[[node_name]][["keys"]] <- final_keys
        }
      }

      in_node_list[[node_name]][["exp_name"]] <- exp_name
      in_node_list[[node_name]][["mc_name"]] <- node_name

      if (node_name %in% null_rm_inputs) {
        in_node_list[[node_name]][["null_rm"]] <- TRUE
      }

      # If no inputs_col matched (no dataset matched) but explicit keys were provided, assign them
      if (is.null(in_node_list[[node_name]][["keys"]]) && !is.null(keys)) {
        if (!is.character(keys)) {
          stop("keys must be a character vector")
        }
        in_node_list[[node_name]][["keys"]] <- keys
      }

      # Parameter renaming
      if (node_name %in% param_names) {
        node_name_exp <- names(param_names)[param_names %in% node_name]
        names(in_node_list)[
          names(in_node_list) %in% param_names
        ] <- node_name_exp
        all_nodes[all_nodes %in% param_names] <- node_name_exp
      }
    }
  }

  # Process previous nodes
  prev_node_list <- list()
  all_nodes_prev <- all_nodes[!all_nodes %in% names(out_node_list)]
  prev_nodes <- all_nodes_prev[
    !all_nodes_prev %in% c(names(in_node_list), names(out_node_list))
  ]

  if (length(prev_nodes) > 0) {
    for (i in 1:length(prev_nodes)) {
      node_name <- prev_nodes[i]
      is_fun <- if (exists(node_name)) is.function(get(node_name)) else FALSE
      if (!is_fun) {
        prev_node_list[[node_name]][["type"]] <- "prev_node"

        # Set null_rm flag if this prev_node is wrapped in mcnode_null_rm()
        if (node_name %in% null_rm_inputs) {
          prev_node_list[[node_name]][["null_rm"]] <- TRUE
        }
      }
    }
  }

  # Combine all node lists (provisional list for key matching)
  node_list <- c(in_node_list, prev_node_list, out_node_list)

  # Order list in node appearance order in expression
  node_list <- node_list[all_nodes]

  # Process output node keys
  for (i in names(out_node_list)) {
    inputs <- node_list[[i]][["inputs"]]
    if (length(inputs) > 0) {
      keys_names <- unique(unlist(lapply(inputs, function(x) {
        node_list[[x]][["keys"]]
      })))
      if (length(keys_names) > 0) {
        node_list[[i]][["keys"]] <- keys_names
      }
    }
  }

  class(node_list) <- "mcnode_list"

  return(node_list)
}

#' AST parser for node expressions
#'
#' Traverse a parsed R expression (AST) and extract symbol names and flags
#' used by get_node_list.
#'
#' @param expr_text Character scalar with the expression to parse (R code as text).
#' @return A list with elements:
#'   - inputs: character vector of symbol names considered inputs
#'   - created_in_exp: logical; TRUE if mcstoc or mcdata was used in the expression
#'   - mc_func: character or NULL; sampling function name detected for mcstoc/mcdata
#'   - nvariates: logical; TRUE if a `nvariates` argument was present
#'   - na_rm_inputs: character vector of symbol names passed to `mcnode_na_rm`
#'   - null_rm: logical; TRUE if `mcnode_null_rm` was used
#'   - function_call: logical; TRUE if any function calls were present
#' @keywords internal
#' @noRd
ast_traverse <- function(expr_text) {
  parsed <- tryCatch(parse(text = expr_text), error = function(e) NULL)
  if (is.null(parsed)) {
    return(list(
      inputs = character(),
      created_in_exp = FALSE,
      mc_func = NULL,
      nvariates = FALSE,
      na_rm = FALSE,
      null_rm_inputs = character(),
      function_call = FALSE,
      unsupported_types = character()
    ))
  }
  node <- parsed[[1]]

  symbols <- character()
  function_names <- character()
  mc_func <- NULL
  created_in_exp <- FALSE
  na_rm_flag <- FALSE
  null_rm_inputs <- character()
  nvariates_flag <- FALSE
  function_call_flag <- FALSE
  unsupported_types <- character()

  traverse <- function(e) {
    if (is.symbol(e)) {
      symbols <<- c(symbols, as.character(e))
      return()
    }
    if (is.atomic(e)) {
      return()
    }
    if (is.call(e)) {
      function_call_flag <<- TRUE
      call_head <- e[[1]]
      fname <- if (is.symbol(call_head)) {
        as.character(call_head)
      } else {
        paste0(deparse(call_head), collapse = "")
      }

      if (is.call(call_head)) {
        ns_op <- as.character(call_head[[1]])
        if (ns_op %in% c("::", ":::")) {
          pkg_name <- as.character(call_head[[2]])
          fname_ns <- as.character(call_head[[3]])
          function_names <<- c(function_names, pkg_name)
          fname <- fname_ns
        }
      }

      function_names <<- c(function_names, fname)

      if (fname %in% c("mcstoc", "mcdata")) {
        created_in_exp <<- TRUE
        nm <- names(e)
        if (!is.null(nm) && "nvariates" %in% nm) {
          nvariates_flag <<- TRUE
        }

        # detect mc sampling function name
        if (!is.null(nm) && "func" %in% nm) {
          argval <- e[["func"]]
          mc_func <<- if (is.symbol(argval)) {
            as.character(argval)
          } else {
            paste0(deparse(argval), collapse = "")
          }
        } else if (length(e) >= 2) {
          second <- e[[2]]
          mc_func <<- if (is.symbol(second)) {
            as.character(second)
          } else {
            paste0(deparse(second), collapse = "")
          }
        }

        # detect unsupported type argument values ("U", "VU")
        if (!is.null(nm) && "type" %in% nm) {
          type_val <- e[["type"]]
          type_char <- if (is.character(type_val)) {
            type_val
          } else if (is.symbol(type_val)) {
            as.character(type_val)
          } else {
            paste0(deparse(type_val), collapse = "")
          }
          if (type_char %in% c("U", "VU")) {
            unsupported_types <<- unique(c(unsupported_types, type_char))
          }
        }
      }

      if (fname == "mcnode_na_rm") {
        na_rm_flag <<- TRUE
      }

      if (fname == "mcnode_null_rm" && length(e) >= 2) {
        first_arg <- e[[2]]
        if (is.symbol(first_arg)) {
          null_rm_inputs <<- c(null_rm_inputs, as.character(first_arg))
        }
      }

      for (i in seq_along(e)[-1]) {
        traverse(e[[i]])
      }
    }
  }

  traverse(node)

  inputs <- setdiff(unique(symbols), unique(function_names))
  if (!is.null(mc_func)) {
    inputs <- setdiff(inputs, mc_func)
  }

  list(
    inputs = inputs,
    created_in_exp = created_in_exp,
    mc_func = mc_func,
    nvariates = nvariates_flag,
    na_rm = na_rm_flag,
    null_rm_inputs = unique(null_rm_inputs),
    function_call = function_call_flag,
    unsupported_types = unsupported_types
  )
}
