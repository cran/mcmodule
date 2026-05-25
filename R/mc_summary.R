#' Summarise Monte Carlo Node Values
#'
#' Computes summary statistics for an mcnode object, including mean,
#' standard deviation, and quantiles. Can be called with an mcmodule and
#' node name, or directly with an mcnode and data frame.
#' @param mcmodule (mcmodule object, optional). Module containing the node.
#'   Default: NULL.
#' @param mc_name (character, optional). Name of the mcnode in the module.
#' @param keys_names (character vector, optional). Column names for grouping.
#'   Default: NULL.
#' @param data (data frame, optional). Input data frame. Default: NULL.
#' @param mcnode (mcnode object, optional). mcnode to summarise directly. Default: NULL.
#' @param sep_keys (logical). If TRUE, keep keys in separate columns; if FALSE,
#'   combine into single column. Default: TRUE.
#' @param digits (integer, optional). Number of significant digits for rounding.
#'   Default: NULL.
#'
#' @details
#' This function can be called in two ways:
#' 1. By providing an mcmodule and mc_name
#' 2. By providing data and mcnode directly
#'
#' For filtered nodes (type = "filter"), compared nodes (type = "compare"), and
#' aggregated nodes (type = "agg_total"), this function returns the pre-calculated
#' summary statistics that were computed when the node was created, rather than
#' recalculating from the original data.
#'
#' @return A data frame with summary statistics for each mcnode variate.
#'   Columns include:
#'   \itemize{
#'     \item mc_name: Node name.
#'     \item Key columns (if sep_keys = TRUE) or single keys column (if FALSE).
#'     \item mean: Average value.
#'     \item sd: Standard deviation.
#'     \item Quantile columns (2.5%, 25%, 50%, 75%, 97.5%).
#'   }
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
mc_summary <- function(
  mcmodule = NULL,
  mc_name = NULL,
  keys_names = NULL,
  data = NULL,
  mcnode = NULL,
  sep_keys = TRUE,
  digits = NULL
) {
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

    # Check if node has a pre-calculated summary (for filtered, compared, or aggregated nodes)
    node_type <- mcmodule$node_list[[mc_name]]$type
    if (
      !is.null(node_type) &&
        node_type %in% c("filter", "compare", "agg_total") &&
        !is.null(mcmodule$node_list[[mc_name]]$summary)
    ) {
      return(mcmodule$node_list[[mc_name]]$summary)
    }

    data_name <- mcmodule$node_list[[mc_name]]$data_name

    if (is.null(data)) {
      data <- mcmodule$data[[data_name]]
    }

    if (
      length(data_name) > 1 & !is.null(mcmodule$node_list[[mc_name]]$summary)
    ) {
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
        paste(missing_keys, collapse = ", "),
        mc_name
      ))
    }
  }

  # Process keys
  keys_names <- if (is.null(keys_names) & !is.null(mcmodule)) {
    names(mc_keys(mcmodule, mc_name))
  } else {
    keys_names
  }

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
  if (!is.list(summary_l)) {
    summary_l <- list(summary_l)
  }

  # Create summary dataframe
  summary_names <- colnames(summary_l[[1]])
  summary_df <- data.frame(matrix(
    unlist(summary_l),
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

signif_round <- function(x, digits = 2) {
  ifelse(
    x < (10^-(digits)),
    signif(x, digits = digits),
    round(x, digits = digits)
  )
}
