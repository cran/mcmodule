#' Create Node List from Model Expression
#'
#' Creates a list of nodes based on a given model expression, handling input,
#' output, and previous nodes with their properties and relationships.
#'
#' @param exp An R expression containing model calculations
#' @param param_names Optional named vector for parameter renaming
#' @param mctable Reference table for  mcnodes, defaults to set_mctable()
#' @param data_keys Data structure and keys, defaults to set_data_keys()
#'
#' @return A list of class "mcnode_list" containing node information
get_node_list <- function(exp, param_names = NULL,
                          mctable = set_mctable(), data_keys = set_data_keys()) {
  module <- gsub("_exp", "", deparse(substitute(exp)))

  # Initialize lists and vectors
  out_node_list <- list()
  all_nodes <- c()

  # Process output nodes from model exp
  for (i in 2:length(exp)) {
    node_name <- deparse(exp[[i]][[2]])
    node_exp <- paste0(deparse(exp[[i]][[3]]), collapse = "")
    out_node_list[[node_name]][["node_exp"]] <- node_exp

    # Extract input node names
    exp1 <- gsub("_", "975UNDERSCORE2023", node_exp)
    exp1 <- gsub("::", "975DOUBLEDOT2025", exp1)

    exp2 <- gsub("[^[:alnum:]]", ",", exp1)

    exp3 <- gsub("975UNDERSCORE2023", "_", exp2)
    exp3 <- gsub("975DOUBLEDOT2025", "::", exp3)

    exp4 <- c(strsplit(exp3, split = ",")[[1]])
    exp4 <- exp4[!exp4 %in% ""]

    if(any(suppressWarnings(as.numeric(exp4))%in%c(Inf,-Inf))) warning("Inputs called 'inf' or '-inf' are assumed to be infinite (numeric) and are not parsed as mcnodes")
    exp5 <- suppressWarnings(exp4[is.na(as.numeric(exp4))])
    inputs <- unique(exp5)

    # Check NA removal
    na_rm <- any(inputs %in% "mcnode_na_rm")
    if (na_rm) {
      out_node_list[[node_name]][["na_rm"]] <- na_rm
    }

    # Filter function inputs
    capture_fun <- gregexpr('(([A-Za-z.][A-Za-z0-9._]*::)?[A-Za-z.][A-Za-z0-9._]*)\\(', node_exp, perl = TRUE)
    starts <- attr(capture_fun[[1]], "capture.start")[, 1]
    lens   <- attr(capture_fun[[1]], "capture.length")[, 1]
    fun_input <- if (length(starts)) substring(node_exp, starts, starts + lens - 1) else character(0)

    inputs <- inputs[!inputs %in% fun_input]
    all_nodes <- unique(c(all_nodes, node_name, inputs))

    # Set node type
    out_node_list[[node_name]][["type"]] <-
      if (!grepl("[[:alpha:]]", node_exp)) "scalar" else "out_node"

    out_node_list[[node_name]][["inputs"]] <- inputs
    out_node_list[[node_name]][["module"]] <- module
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

  # Get all column names from each data frame
  all_inputs <- lapply(data_keys, function(x) {
    if (!is.null(x$cols)) {
      return(x$cols)
    }
    return(NULL)
  })

  if (length(input_nodes) > 0) {
    for (i in 1:length(input_nodes)) {
      node_name <- input_nodes[[i]]
      mc_row <- mctable[mctable$mcnode == node_name, ]

      in_node_list[[node_name]][["type"]] <- "in_node"

      if (!is.na(mc_row$mc_func)) {
        in_node_list[[node_name]][["mc_func"]] <- as.character(mc_row$mc_func)
      }

      in_node_list[[node_name]][["description"]] <- as.character(mc_row$description)

      # Process input columns and datasets
      for (dataset_name in names(all_inputs)) {

        if (is.null(mc_row$from_variable)||is.na(mc_row$from_variable)) {
          # Get matching input columns for current node
          pattern <- paste0("\\<", node_name, "(\\>|[^>]*\\>)")
          inputs_col <- all_inputs[[dataset_name]][grepl(pattern, all_inputs[[dataset_name]])]
        }else{
          # Get matching input columns from transformed variable
          pattern <- paste0("\\<", mc_row$from_variable, "(\\>|[^>]*\\>)")
          inputs_col <- all_inputs[[dataset_name]][grepl(pattern, all_inputs[[dataset_name]])]
          # Add transformation info to node list
          if(!is.null(mc_row$from_variable)&&!is.na(mc_row$transformation)) in_node_list[[node_name]][["transformation"]] <- mc_row$transformation

        }

        # Update node list if matching inputs found
        if (length(inputs_col) > 0) {
          in_node_list[[node_name]][["inputs_col"]] <- inputs_col
          in_node_list[[node_name]][["input_dataset"]] <- dataset_name
          in_node_list[[node_name]][["keys"]] <- data_keys[[dataset_name]][["keys"]]
        }
      }

      in_node_list[[node_name]][["module"]] <- module
      in_node_list[[node_name]][["mc_name"]] <- node_name

      # Parameter renaming
      if (node_name %in% param_names) {
        node_name_exp <- names(param_names)[param_names %in% node_name]
        names(in_node_list)[names(in_node_list) %in% param_names] <- node_name_exp
        all_nodes[all_nodes %in% param_names] <- node_name_exp
      }
    }
  }


  # Process previous nodes
  prev_node_list <- list()
  prev_nodes <- all_nodes[!all_nodes %in% c(names(in_node_list), names(out_node_list))]

  if (length(prev_nodes) > 0) {
    for (i in 1:length(prev_nodes)) {
      node_name <- prev_nodes[i]
      is_fun <- if (exists(node_name)) is.function(get(node_name)) else FALSE
      if (!is_fun) {
        prev_node_list[[node_name]][["type"]] <- "prev_node"
      }
    }
  }

  # Combine all node lists (provisional list for key matching)
  node_list <- c(in_node_list, prev_node_list, out_node_list)

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
