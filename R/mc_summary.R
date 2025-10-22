#' Compute summary statistics for an mcnode object
#'
#' @param mcmodule An mcmodule object containing the node to summarize
#' @param mc_name Character string specifying the name of the mcnode in the module
#' @param keys_names Vector of column names to use as keys for grouping (default: NULL)
#' @param data Optional data frame containing the input data (default: NULL)
#' @param mcnode Optional mcnode object to summarize directly (default: NULL)
#' @param sep_keys Logical; if TRUE, keeps keys in separate columns (default: TRUE)
#' @param digits Integer indicating number of significant digits for rounding (default: NULL)
#'
#' @details
#' This function can be called in two ways:
#' 1. By providing an mcmodule and mc_name
#' 2. By providing data and mcnode directly
#'
#' @return A data frame containing summary statistics with columns:
#'   - mc_name: Name of the mcnode
#'   - keys: Grouping variables (if sep_keys=FALSE) or individual key columns (if sep_keys=TRUE)
#'   - Summary statistics including:
#'     * mean: Average value
#'     * sd: Standard deviation
#'     * Various quantiles (2.5%, 25%, 50%, 75%, 97.5%)
#'
#' @examples
#' # Use with mcmodule
#' summary_basic <- mc_summary(imports_mcmodule, "w_prev")
#'
#' # Using custom keys and rounding
#' summary_custom <- mc_summary(imports_mcmodule, "w_prev",
#'   keys_names = c("origin"),
#'   digits = 3
#' )
#'
#' # Use with data and mcnode
#' w_prev <- imports_mcmodule$node_list$w_prev$mcnode
#' summary_direct <- mc_summary(
#'   data = imports_data,
#'   mcnode = w_prev,
#'   sep_keys = FALSE
#' )
#' @export
mc_summary <- function(mcmodule = NULL, mc_name = NULL,
                       keys_names = NULL,
                       data = NULL,
                       mcnode = NULL,
                       sep_keys = TRUE, digits = NULL) {
  # Input validation
  if (!is.null(mcnode) & is.null(mc_name)) {
    mc_name <- deparse(substitute(mcnode))
  }

  if (!is.null(mcmodule)) {
    module_name <- deparse(substitute(mcmodule))

    if (is.null(mcnode)) {
      mcnode <- mcmodule$node_list[[mc_name]]$mcnode
    }

    if (!is.mcnode(mcnode)) {
      stop(sprintf("%s must be a mcnode present in %s", mc_name, module_name))
    }

    if (is.null(mcnode)) {
      mcnode <- mcmodule$node_list[[mc_name]]$mcnode
    }

    data_name <- mcmodule$node_list[[mc_name]]$data_name

    if (is.null(data)) {
      data <- mcmodule$data[[data_name]]
    }

    if(length(data_name)>1&!is.null(mcmodule$node_list[[mc_name]]$summary)){
      message("Too many data names. Using existing summary.")
      return(mcmodule$node_list[[mc_name]]$summary)
    }

  } else {
    if (is.null(data)) stop("mcmodule or data must be provided")
  }



  # Validate provided keys
  if (!is.null(keys_names)) {
    missing_keys <- keys_names[!keys_names %in% names(data)]
    if (length(missing_keys) > 0) {
      stop(sprintf(
        "keys_names (%s) must appear in %s data column names",
        paste(missing_keys, collapse = ", "), mc_name
      ))
    }
  }

  # Process keys
  keys_names <- if (is.null(keys_names) & !is.null(mcmodule)) names(mc_keys(mcmodule, mc_name)) else keys_names

  keys <- if (length(keys_names) > 0 && any(keys_names %in% names(data))) {
    data[names(data) %in% keys_names]
  } else {
    data.frame(variate = seq_len(nrow(data)))
  }

  if (!sep_keys) {
    keys$keys <- do.call(paste, c(keys, list(sep = ", ")))
    keys <- keys["keys"]
    keys_groups <- c("mc_name", "keys")
  } else {
    keys_groups <- c("mc_name", names(keys))
  }

  # Calculate summary statistics
  summary_l <- summary(mcnode)[[1]]
  if (!is.list(summary_l)) summary_l <- list(summary_l)

  # Create summary dataframe
  summary_names <- colnames(summary_l[[1]])
  summary_df <- data.frame(matrix(unlist(summary_l),
    nrow = length(summary_l),
    byrow = TRUE
  ))
  names(summary_df) <- summary_names
  summary_df <- cbind(mc_name, keys, summary_df)

  # Round if digits specified
  if (!is.null(digits)) {
    numeric_cols <- sapply(summary_df, is.numeric)
    summary_df[numeric_cols] <- lapply(
      summary_df[numeric_cols],
      function(x) signif_round(x, digits = digits)
    )
  }

  return(summary_df)
}

signif_round <- function(x, digits = 2) ifelse(x < (10^-(digits)), signif(x, digits = digits), round(x, digits = digits))

#' Get mcnode summary keys
#' @param mcsummary data frame from mc_summary()
#' @return vector of key names
mc_summary_keys <- function(mcsummary) {
  if ("mean" %in% names(mcsummary)) {
    names(mcsummary)[2:(match("mean", names(mcsummary)) - 1)]
  } else {
    names(mcsummary)[2:(match("NoUnc", names(mcsummary)) - 1)]
  }
}

#' Include summary and keys in node_list
#' @param mcmodule mc module object
#' @param data data frame with mc inputs
#' @param node_list list of nodes
#' @return updated node_list
node_list_summary <- function(mcmodule = NULL, data = NULL, node_list = NULL) {
  if (!is.null(mcmodule)) {
    data <- mcmodule$data
    node_list <- mcmodule$node_list
  } else if (is.null(data) || is.null(node_list)) {
    stop("data containing mc_inputs and node_list must be provided")
  }

  for (i in seq_along(node_list)) {
    node_name <- names(node_list)[i]
    inputs_names <- node_list[[i]][["inputs"]]
    mcnode <- node_list[[i]][["mcnode"]]

    if (is.null(inputs_names)) {
      node_summary <- mc_summary(
        data = data, mcnode = mcnode,
        mc_name = node_name
      )
    } else {
      keys_names <- unique(unlist(lapply(inputs_names, function(x) {
        node_list[[x]][["keys"]]
      })))

      node_summary <- mc_summary(
        data = data, mcnode = mcnode,
        mc_name = node_name,
        keys_names = keys_names
      )
    }

    node_list[[i]][["summary"]] <- node_summary
    node_list[[i]][["keys"]] <- mc_summary_keys(node_summary)
  }

  if (!is.null(mcmodule)) {
    mcmodule$node_list <- node_list
    return(mcmodule)
  }

  return(node_list)
}
