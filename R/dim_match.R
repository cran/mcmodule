#' Get Monte Carlo Node Keys
#'
#' Extracts key columns from Monte Carlo node's associated data.
#'
#' @param mcmodule Monte Carlo module containing nodes and data
#' @param mc_name Name of the node to extract keys from
#' @param keys_names Vector of column names to extract (optional)
#'
#' @return Dataframe with scenario_id and requested key columns
#'
#' @examples
#' keys_df <- mc_keys(imports_mcmodule, "w_prev")
#'
#' @export
mc_keys <- function(mcmodule, mc_name, keys_names = NULL) {
  # Input validation
  if (!is.list(mcmodule) || is.null(mcmodule$node_list)) {
    stop(sprintf("%s", "Invalid mcmodule structure"))
  }
  if (!mc_name %in% names(mcmodule$node_list)) {
    stop(
      sprintf(
        "%s not found in %s",
        mc_name,
        deparse(substitute(mcmodule))
      )
    )
  }

  # Get the node from module
  node <- mcmodule$node_list[[mc_name]]

  # Determine keys to extract:
  # 1. Use provided keys_names if not NULL
  # 2. Otherwise use agg_keys if available
  # 3. Fall back to regular keys
  keys_names <- if (!is.null(keys_names)) {
    keys_names
  } else if (!is.null(node[["agg_keys"]]) && !node[["keep_variates"]]) {
    node[["agg_keys"]]
  } else {
    node[["keys"]]
  }

  # Get data based on node aggregation and data_names:
  # 1. Aggregated mcnodes and mcnodes use "summary"
  # 2. Nodes with multiple data_names:
  #    - Use "summary" if all agg_keys are found in "summary"
  #    - Use "data" corresponding to last data_name if not found in "summary"
  # 3. Otherwise, use "data" corresponding to data_name

  data <- if (!is.null(node[["agg_keys"]]) && !node[["keep_variates"]]) {
    if (is.null(node[["summary"]])) {
      stop(
        sprintf(
          "%s summary is needed for aggregated mcnodes",
          mc_name
        ),
        call. = FALSE
      )
    }
    # Case 1: Aggregated nodes
    node[["summary"]]
  } else if (length(node[["data_name"]]) > 1) {
    # Case 2: Multiple data_names
    if (is.null(node[["summary"]])) {
      stop(
        sprintf(
          "%s summary is needed for mcnodes with multiple data_names",
          mc_name
        ),
        call. = FALSE
      )
    }

    if (all(keys_names %in% names(node[["summary"]]))) {
      # All keys found in summary
      node[["summary"]]
    } else {
      # Check last data_name
      ref_data_name <- node[["data_name"]][length(node[["data_name"]])]
      common_keys <- intersect(
        names(mcmodule$data[[ref_data_name]]),
        names(node[["summary"]])
      )

      # Verify data consistency
      if (
        all(
          mcmodule$data[[ref_data_name]][common_keys] ==
            node[["summary"]][common_keys]
        )
      ) {
        message(sprintf(
          "%s has multiple data_name (%s), using '%s' for mc_keys",
          mc_name,
          paste0(unique(unlist(node[["data_name"]])), collapse = ", "),
          ref_data_name
        ))
        mcmodule$data[[ref_data_name]]
      } else {
        # Identify missing agg_keys
        missing_agg_keys <- setdiff(keys_names, names(node[["summary"]]))

        stop(
          sprintf(
            "Data mismatch detected for node '%s': Common keys between '%s' and 'summary' have different values. Missing agg_keys in summary: %s",
            mc_name,
            ref_data_name,
            paste0(missing_agg_keys, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }
  } else {
    # Case 3: Single data_name
    mcmodule$data[[node[["data_name"]]]]
  }

  # Add scenario_id column if missing
  if (!"scenario_id" %in% names(data)) {
    data$scenario_id <- "0"
  }

  # Validate all requested keys exist
  missing_keys <- setdiff(keys_names, names(data))
  if (length(missing_keys) > 0) {
    stop(
      sprintf(
        "Columns %s not found in %s data",
        paste(missing_keys, collapse = ", "),
        mc_name
      )
    )
  }

  # Check for duplicates in baseline scenario
  scenario_0 <- if (any(data$scenario_id == "0")) "0" else data$scenario_id[1]
  if (any(duplicated(data[data$scenario_id == scenario_0, keys_names]))) {
    data_0 <- data[data$scenario_id == scenario_0, keys_names]
    # Remove key columns that are all NA
    na_cols <- sapply(data_0, function(x) all(is.na(x)))
    if (any(na_cols)) {
      data_0 <- data_0[, !na_cols]
    }
    if (length(data_0) == 1 && is.data.frame(data_0)) {
      var_groups <- max(
        table(apply(data_0, 1, paste, collapse = "-")),
        na.rm = TRUE
      )
    } else {
      if (is.data.frame(data_0) && ncol(data_0) < 1) {
        var_groups <- "Unknown"
      } else {
        var_groups <- max(table(data_0), na.rm = TRUE)
      }
    }
    message(sprintf(
      "%s variates per group for %s",
      var_groups,
      mc_name
    ))
  }

  # Return only requested columns
  data[unique(c("scenario_id", keys_names))]
}

#' Match Monte Carlo Nodes
#'
#' Matches two mcnodes by:
#' 1. Group matching - Align nodes with same scenarios but different group order
#' 2. Scenario matching - Align nodes with same groups but different scenarios
#' 3. Null matching - Add missing groups across different scenarios
#'
#' @param mcmodule Monte Carlo module
#' @param mc_name_x First node name
#' @param mc_name_y Second node name
#' @param keys_names Names of key columns
#' @return List containing matched nodes and combined keys (keys_xy)
#' @examples
#' test_module <- list(
#'   node_list = list(
#'     node_x = list(
#'       mcnode = mcstoc(runif,
#'         min = mcdata(c(1, 2, 3), type = "0", nvariates = 3),
#'         max = mcdata(c(2, 3, 4), type = "0", nvariates = 3),
#'         nvariates = 3
#'       ),
#'       data_name = "data_x",
#'       keys = c("category")
#'     ),
#'     node_y = list(
#'       mcnode = mcstoc(runif,
#'         min = mcdata(c(5, 6, 7), type = "0", nvariates = 3),
#'         max = mcdata(c(6, 7, 8), type = "0", nvariates = 3),
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
#' result <- mc_match(test_module, "node_x", "node_y")
#' @export
mc_match <- function(mcmodule, mc_name_x, mc_name_y, keys_names = NULL) {
  # Check if mcnodes are in mcmodule
  missing_nodes <- c(mc_name_x, mc_name_y)[
    !c(mc_name_x, mc_name_y) %in% names(mcmodule$node_list)
  ]

  if (length(missing_nodes) > 0) {
    stop(
      sprintf(
        "Nodes %s not found in %s",
        paste(missing_nodes, collapse = ", "),
        deparse(substitute(mcmodule))
      )
    )
  }

  # Get nodes
  mcnode_x <- mcmodule$node_list[[mc_name_x]][["mcnode"]]
  mcnode_y <- mcmodule$node_list[[mc_name_y]][["mcnode"]]

  # Get nodes data name
  data_name_x <- mcmodule$node_list[[mc_name_x]][["data_name"]]
  data_name_y <- mcmodule$node_list[[mc_name_y]][["data_name"]]

  # Remove scenario_id from keys
  keys_names <- keys_names[!keys_names == "scenario_id"]

  # Get keys dataframes for x and y
  keys_x <- mc_keys(mcmodule, mc_name_x, keys_names)
  keys_y <- mc_keys(mcmodule, mc_name_y, keys_names)

  # Validate baseline scenario contains all key combinations for both nodes
  check_baseline_keys(keys_x, keys_names, mc_name_x)
  check_baseline_keys(keys_y, keys_names, mc_name_y)

  # If nodes do not have the same keys but both nodes come from the same data, keys are inferred from data
  if (
    nrow(keys_x) == nrow(keys_y) &&
      all(
        keys_x[intersect(names(keys_x), names(keys_y))] ==
          keys_y[intersect(names(keys_x), names(keys_y))],
        na.rm = TRUE
      )
  ) {
    # Find keys that are only pressent in one of the mcnodes
    keys_x_only <- setdiff(names(keys_x), names(keys_y))
    keys_y_only <- setdiff(names(keys_y), names(keys_x))

    if (length(keys_x_only) > 0) {
      message(sprintf(
        "Keys inferred from %s for %s: %s",
        paste(c(data_name_x), collapse = ", "),
        mc_name_x,
        paste0(keys_x_only, collapse = ", ")
      ))
    }

    if (length(keys_y_only) > 0) {
      message(sprintf(
        "Keys inferred from %s for %s: %s",
        paste(c(data_name_y), collapse = ", "),
        mc_name_y,
        paste0(keys_y_only, collapse = ", ")
      ))
    }

    # Add the same keys to both mcnodes
    keys_x <- cbind(
      keys_x[intersect(names(keys_x), names(keys_y))],
      keys_x[keys_x_only],
      keys_y[keys_y_only]
    )

    keys_y <- cbind(
      keys_y[intersect(names(keys_x), names(keys_y))],
      keys_x[keys_x_only],
      keys_y[keys_y_only]
    )

    # Return nodes as they are if they already match
    message(sprintf(
      "%s and %s already match, dim: [%s]",
      mc_name_x,
      mc_name_y,
      paste(dim(mcnode_x), collapse = ", ")
    ))

    return(list(
      mcnode_x_match = mcnode_x,
      mcnode_y_match = mcnode_y,
      keys_xy = keys_match(keys_x, keys_y, keys_names)$xy
    ))
  }

  # Match keys
  keys_list <- keys_match(keys_x, keys_y, keys_names)
  keys_x_match <- keys_list$x
  keys_y_match <- keys_list$y
  keys_xy_match <- keys_list$xy

  # Update keys (add x and y unique keys to all matched outputs)
  new_keys <- unique(names(keys_x), names(keys_y))
  new_keys <- new_keys[!new_keys %in% c("g_id", "g_row", "scenario_id")]

  keys_x <- cbind(
    keys_x_match[!names(keys_x_match) %in% new_keys],
    keys_x[names(keys_x) %in% new_keys]
  )
  keys_y <- cbind(
    keys_y_match[!names(keys_y_match) %in% new_keys],
    keys_y[names(keys_y) %in% new_keys]
  )
  keys_xy_match <- left_join(
    keys_xy_match,
    keys_x[!names(keys_x) %in% names(keys_xy_match)],
    by = c("g_row.x" = "g_row")
  )
  keys_xy_match <- left_join(
    keys_xy_match,
    keys_y[!names(keys_y) %in% names(keys_xy_match)],
    by = c("g_row.y" = "g_row")
  )
  keys_xy_match
  keys_xy <- relocate(
    keys_xy_match,
    c("g_id", "g_row.x", "g_row.y", "scenario_id")
  )

  # Match nodes
  null_x <- 0
  null_y <- 0

  # Process X node
  for (i in 1:nrow(keys_xy)) {
    g_row_x_i <- keys_xy$g_row.x[i]

    if (keys_xy$g_id[i] %in% keys_x$g_id) {
      mc_i <- extractvar(mcnode_x, g_row_x_i)
    } else {
      mc_i <- extractvar(mcnode_x, 1) - extractvar(mcnode_x, 1)
      null_x <- null_x + 1
    }

    if (i == 1) {
      mcnode_x_match <- mc_i
    } else {
      mcnode_x_match <- addvar(mcnode_x_match, mc_i)
    }
  }

  # Process Y node
  for (i in 1:nrow(keys_xy)) {
    g_row_y_i <- keys_xy$g_row.y[i]

    if (keys_xy$g_id[i] %in% keys_y$g_id) {
      mc_i <- extractvar(mcnode_y, g_row_y_i)
    } else {
      mc_i <- extractvar(mcnode_y, 1) - extractvar(mcnode_y, 1)
      null_y <- null_y + 1
    }

    if (i == 1) {
      mcnode_y_match <- mc_i
    } else {
      mcnode_y_match <- addvar(mcnode_y_match, mc_i)
    }
  }

  # Log results
  message(sprintf(
    "%s prev dim: [%s], new dim: [%s], %s null matches",
    mc_name_x,
    paste(dim(mcnode_x), collapse = ", "),
    paste(dim(mcnode_x_match), collapse = ", "),
    null_x
  ))

  message(sprintf(
    "%s prev dim: [%s], new dim: [%s], %s null matches",
    mc_name_y,
    paste(dim(mcnode_y), collapse = ", "),
    paste(dim(mcnode_y_match), collapse = ", "),
    null_y
  ))

  # Return results
  result <- list(mcnode_x_match, mcnode_y_match, keys_xy)
  names(result) <- c(
    paste0(mc_name_x, "_match"),
    paste0(mc_name_y, "_match"),
    "keys_xy"
  )

  return(result)
}

#' Match Monte Carlo Node with other data frame
#'
#' Matches an mcnode with a data frame by:
#' 1. Group matching - Same scenarios but different group order
#' 2. Scenario matching - Same groups but different scenarios
#' 3. Null matching - Add missing groups across different scenarios
#'
#' @param mcmodule Monte Carlo module
#' @param mc_name Node name
#' @param data Data frame containing keys to match with
#' @param keys_names Names of key columns
#' @return List containing matched node, matched data and combined keys (keys_xy
#' @examples
#' test_data  <- data.frame(pathogen=c("a","b"),
#'                          inf_dc_min=c(0.05,0.3),
#'                          inf_dc_max=c(0.08,0.4))
#' result<-mc_match_data(imports_mcmodule,"no_detect_a", test_data)
#' @export
mc_match_data <- function(mcmodule, mc_name, data, keys_names = NULL) {
  # Check if mcnodes are in mcmodule
  if (!mc_name %in% names(mcmodule$node_list)) {
    stop(paste("Nodes", mc_name, "not found in", deparse(substitute(mcmodule))))
  }

  # Get node
  mcnode_x <- mcmodule$node_list[[mc_name]][["mcnode"]]

  # Get nodes data name
  data_name_x <- mcmodule$node_list[[mc_name]][["data_name"]]
  data_name_y <- deparse(substitute(data))

  # Remove scenario_id from keys
  keys_names <- keys_names[!keys_names == "scenario_id"]

  # Get keys dataframes for x and y
  keys_x <- mc_keys(mcmodule, mc_name, keys_names)
  keys_data <- intersect(names(keys_x), names(data))
  keys_y <- data[keys_data]

  # Validate baseline scenario contains all key combinations for both node and provided data
  check_baseline_keys(keys_x, keys_names, mc_name)
  check_baseline_keys(data, keys_data, data_name_y)

  # If nodes do not have the same keys but both nodes come from the same data, keys are inferred from data
  if (
    nrow(keys_x) == nrow(keys_y) &&
      all(
        keys_x[intersect(names(keys_x), names(keys_y))] ==
          keys_y[intersect(names(keys_x), names(keys_y))],
        na.rm = TRUE
      )
  ) {
    # Return nodes as they are if they already match
    message(
      mc_name,
      " and ",
      data_name_y,
      " already match, dim: [",
      paste(dim(mcnode_x), collapse = ", "),
      "]"
    )

    return(list(
      mcnode_match = mcnode_x,
      data_match = data,
      keys_xy = keys_match(keys_x, keys_y, keys_names)$xy
    ))
  }

  # Match keys
  keys_list <- keys_match(keys_x, keys_y, keys_names)
  keys_x_match <- keys_list$x
  keys_y_match <- keys_list$y
  keys_xy_match <- keys_list$xy

  # Update keys (add x and y unique keys to all matched outputs)
  new_keys <- unique(names(keys_x), names(keys_y))
  new_keys <- new_keys[!new_keys %in% c("g_id", "g_row", "scenario_id")]

  keys_x <- cbind(
    keys_x_match[!names(keys_x_match) %in% new_keys],
    keys_x[names(keys_x) %in% new_keys]
  )
  keys_y <- cbind(
    keys_y_match[!names(keys_y_match) %in% new_keys],
    keys_y[names(keys_y) %in% new_keys]
  )
  keys_xy_match <- left_join(
    keys_xy_match,
    keys_x[!names(keys_x) %in% names(keys_xy_match)],
    by = c("g_row.x" = "g_row")
  )
  keys_xy_match <- left_join(
    keys_xy_match,
    keys_y[!names(keys_y) %in% names(keys_xy_match)],
    by = c("g_row.y" = "g_row")
  )
  keys_xy_match
  keys_xy <- relocate(
    keys_xy_match,
    c("g_id", "g_row.x", "g_row.y", "scenario_id")
  )

  # Match nodes
  null_x <- 0
  null_y <- 0

  # Process X node
  for (i in 1:nrow(keys_xy)) {
    g_row_x_i <- keys_xy$g_row.x[i]

    if (keys_xy$g_id[i] %in% keys_x$g_id) {
      mc_i <- extractvar(mcnode_x, g_row_x_i)
    } else {
      mc_i <- extractvar(mcnode_x, 1) - extractvar(mcnode_x, 1)
      null_x <- null_x + 1
    }

    if (i == 1) {
      mcnode_x_match <- mc_i
    } else {
      mcnode_x_match <- addvar(mcnode_x_match, mc_i)
    }
  }
  # Process data
  for (i in 1:nrow(keys_xy)) {
    g_row_y_i <- keys_xy$g_row.y[i]

    if (keys_xy$g_id[i] %in% keys_y$g_id) {
      row_i <- data[g_row_y_i, ]
      row_i <- cbind(keys_xy[i, new_keys], row_i[!names(row_i) %in% new_keys])
    } else {
      row_i <- keys_xy[i, keys_data]
      null_y <- null_y + 1
    }

    if (i == 1) {
      data_match <- row_i
    } else {
      data_match <- dplyr::bind_rows(data_match, row_i)
    }
  }

  # Log results
  message(sprintf(
    "%s prev dim: [%s], new dim: [%s], %s null matches",
    mc_name,
    paste(dim(mcnode_x), collapse = ", "),
    paste(dim(mcnode_x_match), collapse = ", "),
    null_x
  ))

  message(sprintf(
    "%s prev dim: [%s], new dim: [%s], %s null matches",
    data_name_y,
    paste(dim(data), collapse = ", "),
    paste(dim(data_match), collapse = ", "),
    null_y
  ))

  # Return results
  result <- list(mcnode_x_match, data_match, keys_xy)
  names(result) <- c(
    paste0(mc_name, "_match"),
    paste0(data_name_y, "_match"),
    "keys_xy"
  )

  return(result)
}

#' Match Datasets With Differing Scenarios
#'
#' Matches datasets by group and preserves baseline scenarios (scenario_id=0) when scenarios differ between them.
#'
#' @param x First dataset to match
#' @param y Second dataset to match
#' @param by Grouping variable(s) to match on, defaults to NULL
#' @return List containing matched datasets with aligned scenario IDs:
#'   - First element: matched version of dataset x
#'   - Second element: matched version of dataset y
#' @examples
#' x <- data.frame(
#'   category = c("a", "b", "a", "b"),
#'   scenario_id = c(0, 0, 1, 1),
#'   value = 1:4
#' )
#'
#' y <- data.frame(
#'   category = c("a", "b", "a", "b"),
#'   scenario_id = c(0, 0, 2, 2),
#'   value = 5:8
#' )
#'
#' # Automatic matching
#' result <- wif_match(x, y)
#'
#' @export
wif_match <- function(x, y, by = NULL) {
  # Match keys between datasets
  list_xy <- keys_match(x, y, by)

  # Find any unmatched groups in both datasets

  # Define keys_names if not provided
  if (is.null(by)) {
    # Get categorical variables for each dataframe
    cat_x <- names(x)[sapply(x, function(col) {
      is.character(col) | is.factor(col)
    })]
    cat_y <- names(y)[sapply(y, function(col) {
      is.character(col) | is.factor(col)
    })]

    # Find intersection of categorical variables
    by <- unique(intersect(cat_x, cat_y))
    by <- by[!by %in% c("g_id", "g_row", "scenario_id")]
  }

  null_x <- unique(list_xy$xy[is.na(list_xy$xy$g_row.x), by])
  null_y <- unique(list_xy$xy[is.na(list_xy$xy$g_row.y), by])

  # Format error messages for unmatched groups
  w_null_x <- paste(names(null_x), null_x, sep = " ", collapse = ", ")
  w_null_y <- paste(names(null_y), null_y, sep = " ", collapse = ", ")

  # Stop if any groups couldn't be matched
  if (any(is.na(list_xy$xy$g_row.x)) || any(is.na(list_xy$xy$g_row.y))) {
    error_msg <- character()
    if (any(is.na(list_xy$xy$g_row.x))) {
      error_msg <- c(error_msg, paste("In x:", w_null_x))
    }
    if (any(is.na(list_xy$xy$g_row.y))) {
      error_msg <- c(error_msg, paste("In y:", w_null_y))
    }
    stop(
      sprintf("Groups not found: %s", paste(error_msg, collapse = "; "))
    )
  }

  # Create matched versions of both datasets
  new_x <- x[list_xy$xy$g_row.x, ] %>%
    mutate(scenario_id = list_xy$xy$scenario_id)
  rownames(new_x) <- NULL

  new_y <- y[list_xy$xy$g_row.y, ] %>%
    mutate(scenario_id = list_xy$xy$scenario_id)
  rownames(new_y) <- NULL

  # Count groups and scenarios for logging
  n_g_x <- length(unique(list_xy$x$g_id))
  n_g_y <- length(unique(list_xy$y$g_id))
  n_g_xy <- length(unique(list_xy$xy$g_id))

  n_scenario_x <- length(unique(list_xy$x$scenario_id))
  n_scenario_y <- length(unique(list_xy$y$scenario_id))
  n_scenario_xy <- length(unique(list_xy$xy$scenario_id))

  # Log matching results
  message(sprintf(
    "From %s rows (%s groups, %s scenarios) and %s rows (%s groups, %s scenarios), to %s rows (%s groups, %s scenarios)",
    nrow(x),
    n_g_x,
    n_scenario_x,
    nrow(y),
    n_g_y,
    n_scenario_y,
    nrow(new_x),
    n_g_xy,
    n_scenario_xy
  ))

  # Return list with matched datasets
  list_new_xy <- list(new_x, new_y)
  names(list_new_xy) <- c(deparse(substitute(x)), deparse(substitute(y)))

  return(list_new_xy)
}

# Helper: check baseline scenario contains all key combinations required for matching
check_baseline_keys <- function(
  data,
  keys_names = NULL,
  dataset_name = "<data>"
) {
  # Exclude reserved columns from keys (scenario_id must never be a key)
  reserved <- c("scenario_id", "g_id", "g_row")

  # Infer keys when not explicitly provided
  if (is.null(keys_names) || length(keys_names) == 0) {
    keys_names <- setdiff(names(data), reserved)
  } else {
    # Remove reserved columns if user accidentally passed them
    keys_names <- setdiff(keys_names, reserved)
  }

  # Keep only keys that actually exist in data
  keys_names <- keys_names[keys_names %in% names(data)]
  if (length(keys_names) == 0) {
    return(invisible(TRUE))
  }

  # Ensure scenario_id exists and is character
  if (!"scenario_id" %in% names(data)) {
    data$scenario_id <- "0"
  }
  data$scenario_id <- as.character(data$scenario_id)

  paste_rows <- function(df, keys) {
    if (nrow(df) == 0) {
      return(character(0))
    }
    # ensure deterministic column order
    do.call(paste, c(df[keys], sep = "\r"))
  }

  combos_all <- unique(paste_rows(data, keys_names))
  combos_base <- unique(paste_rows(
    data[data$scenario_id == "0", , drop = FALSE],
    keys_names
  ))

  missing <- setdiff(combos_all, combos_base)
  if (length(missing) > 0) {
    missing_readable <- vapply(
      strsplit(missing, "\r", fixed = TRUE),
      function(vals) {
        paste(paste(keys_names, vals, sep = " = "), collapse = ", ")
      },
      character(1)
    )
    stop(
      sprintf(
        "Baseline scenario '0' missing key combinations in %s: %s. Ensure scenario_id == '0' contains all groups required for matching.",
        dataset_name,
        paste(missing_readable, collapse = "; ")
      )
    )
  }

  invisible(TRUE)
}
