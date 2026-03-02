#' Convert mcnode to Long Format for Plotting
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Converts an mcnode to long format suitable for ggplot2 and tidyverse analysis.
#' Each row represents one uncertainty iteration for one variate.
#'
#' @param mcmodule (mcmodule object, optional). Module containing the node.
#' @param mc_name (character, optional). Name of the mcnode in the module.
#' @param mcnode (mcnode object, optional). mcnode to convert directly.
#' @param data (data frame, optional). Input data; extracted from `mcmodule` if NULL.
#'   Default: NULL.
#' @param keys_names (character vector, optional). Column names for grouping variates.
#'   If NULL, uses node keys from module or all available keys. Default: NULL.
#' @param filter (expression, optional). Unquoted expression to filter variates
#'   (e.g., `pathogen == "a"` or `origin == "nord"`). Evaluated in context of
#'   keys data frame. Default: NULL.
#'
#' @return A long data frame with columns:
#'   \itemize{
#'     \item All key columns from `keys_names`.
#'     \item variate: Variate index (data row number).
#'     \item simulation: Uncertainty iteration index.
#'     \item value: mcnode value for that combination.
#'   }
#'
#' @details
#' Call signatures:
#' - `tidy_mcnode(mcmodule, \"node_name\")`
#' - `tidy_mcnode(mcnode = mcnode, data = data)`
#' - `tidy_mcnode(mcmodule, mcnode = mcnode)`
#'
#' @examples
#' # Using mcmodule and node name
#' long_data <- tidy_mcnode(imports_mcmodule, "w_prev")
#'
#' # Using with specific keys
#' long_data <- tidy_mcnode(imports_mcmodule, "w_prev",
#'   keys_names = "origin"
#' )
#'
#' # Using mcnode and data directly
#' w_prev <- imports_mcmodule$node_list$w_prev$mcnode
#' long_data <- tidy_mcnode(mcnode = w_prev, data = imports_data)
#'
#' # Filter variates
#' long_data <- tidy_mcnode(imports_mcmodule, "w_prev",
#'   filter = pathogen == "a"
#' )
#'
#' @export
tidy_mcnode <- function(
  mcmodule = NULL,
  mc_name = NULL,
  mcnode = NULL,
  data = NULL,
  keys_names = NULL,
  filter = NULL
) {
  # Input validation and setup
  if (!is.null(mcnode) && is.null(mc_name)) {
    mc_name <- deparse(substitute(mcnode))
  }

  # Capture filter expression before it gets evaluated
  filter_expr <- substitute(filter)

  if (!is.null(mcmodule)) {
    module_name <- deparse(substitute(mcmodule))

    if (is.null(mcnode)) {
      mcnode <- mcmodule$node_list[[mc_name]]$mcnode
    }

    if (!is.mcnode(mcnode)) {
      stop(sprintf("%s must be a mcnode present in %s", mc_name, module_name))
    }

    data_name <- mcmodule$node_list[[mc_name]]$data_name

    if (is.null(data)) {
      # Handle nodes with multiple data_names using existing summary if available
      if (
        length(data_name) > 1 && !is.null(mcmodule$node_list[[mc_name]]$summary)
      ) {
        data <- mcmodule$node_list[[mc_name]]$summary
      } else {
        data <- mcmodule$data[[data_name]]
      }
    }
  } else {
    if (is.null(data)) {
      stop("mcmodule or data must be provided")
    }
  }

  # Get mcnode dimensions
  # Dimension 1: uncertainty iterations
  # Dimension 2: variability iterations (usually 1)
  # Dimension 3: variates (scenarios/rows from data)
  dims <- dim(mcnode)
  dims <- if (is.null(dims)) length(mcnode) else dims
  dims <- c(dims, 1, 1)
  n_uncertainty <- dims[1]
  n_variability <- dims[2]
  n_variates <- dims[3]

  # Validate provided keys
  if (!is.null(keys_names)) {
    if (is.null(data)) {
      stop("keys_names requires a non-NULL data argument")
    }

    missing_keys <- keys_names[!keys_names %in% names(data)]
    if (length(missing_keys) > 0) {
      stop(sprintf(
        "keys_names (%s) must appear in data column names",
        paste(missing_keys, collapse = ", ")
      ))
    }
  }

  # Determine keys to use
  if (is.null(keys_names) && !is.null(mcmodule) && !is.null(data)) {
    keys_names <- mcmodule$node_list[[mc_name]]$keys
  }

  # Extract key columns from data or fall back to row_id
  if (!is.null(data) && !is.null(keys_names) && length(keys_names) > 0) {
    keys_df <- data[names(data) %in% keys_names]
  } else if (!is.null(data)) {
    keys_df <- data.frame(row_id = seq_len(nrow(data)))
  } else {
    keys_df <- data.frame(row_id = seq_len(n_variates))
  }

  # Apply filter if provided
  if (!is.null(filter_expr) && !identical(filter_expr, quote(NULL))) {
    # Evaluate filter expression in the context of keys_df
    filter_result <- eval(filter_expr, envir = keys_df, enclos = parent.frame())

    if (!is.logical(filter_result)) {
      stop("filter expression must evaluate to a logical vector")
    }

    if (length(filter_result) != nrow(keys_df)) {
      stop(sprintf(
        "filter expression length (%d) does not match number of variates (%d)",
        length(filter_result),
        nrow(keys_df)
      ))
    }

    # Keep track of which variates to include
    variate_indices <- which(filter_result)
    keys_df <- keys_df[filter_result, , drop = FALSE]
    n_variates <- nrow(keys_df)

    if (n_variates == 0) {
      warning("filter removed all variates, returning empty data frame")
      return(data.frame(
        variate = integer(0),
        simulation = integer(0),
        value = numeric(0)
      ))
    }
  } else {
    variate_indices <- seq_len(n_variates)
  }

  # Check that number of variates matches number of data rows
  if (length(variate_indices) != nrow(keys_df)) {
    stop(sprintf(
      "Mismatch: filtered variates (%d) but data has %d rows",
      length(variate_indices),
      nrow(keys_df)
    ))
  }

  # Extract all values and reshape
  # For each variate, we have n_uncertainty * n_variability simulations
  result_list <- list()

  for (idx in seq_len(n_variates)) {
    # Get the original variate index from the filtered indices
    i <- variate_indices[idx]

    # Extract all simulations for this variate
    variate_values <- as.numeric(mcnode[,, i])
    n_sims <- length(variate_values)

    # Create data frame for this variate
    variate_df <- cbind(
      keys_df[rep(idx, n_sims), , drop = FALSE],
      data.frame(
        variate = i,
        simulation = seq_len(n_sims),
        value = variate_values,
        stringsAsFactors = FALSE
      )
    )
    rownames(variate_df) <- NULL
    result_list[[idx]] <- variate_df
  }

  # Combine all variates
  long_df <- do.call(rbind, result_list)
  long_df <- as.data.frame(long_df, stringsAsFactors = FALSE)

  return(long_df)
}


#' Plot Monte Carlo Node Distribution with Boxplot and Scatter Points
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Creates a ggplot2 visualisation of Monte Carlo node data showing distributions
#' as semi-transparent boxplots overlaid with scatter points representing individual
#' uncertainty iterations.
#'
#' @param mcmodule (mcmodule object, optional). Module containing the node.
#' @param mc_name (character, optional). Name of the mcnode in the module.
#' @param mcnode (mcnode object, optional). mcnode to plot directly.
#' @param data (data frame, optional). Input data. If NULL, extracted from `mcmodule`.
#'   Default: NULL.
#' @param keys_names (character vector, optional). Column names for grouping variates.
#'   If NULL, uses node keys from module or row indices. Default: NULL.
#' @param color_by (character, optional). Column name to colour points and boxplot.
#'   Must be in `keys_names` or `data`. Default: NULL.
#' @param order_by (character, optional). Column name or "median" to reorder y-axis
#'   groups. If "median", groups ordered by median value. Default: NULL.
#' @param group_by (character, optional). Column name to group variates (e.g.,
#'   "commodity"). Variates organised so all scenarios per group appear together.
#'   Default: NULL.
#' @param filter (expression, optional). Unquoted expression to filter variates
#'   (e.g., `pathogen == "a"` or `origin == "nord"`). Passed to `tidy_mcnode()`.
#'   Default: NULL.
#' @param threshold (numeric, optional). Reference value for vertical dashed line.
#'   Default: NULL.
#' @param scale (character, optional). Transformation for x-axis: "identity"
#'   (default), "log10", "log", "sqrt", or "asinh". Default: NULL.
#' @param max_dots (integer). Maximum dots per variate; exceeding this triggers
#'   representative sampling. Boxplots always use all simulations. Default: 300.
#' @param point_alpha (numeric). Transparency for points (0–1). Default: 0.4.
#' @param boxplot_alpha (numeric). Transparency for boxplots (0–1). Default: 0.3.
#' @param color_pal (character vector, optional). Named vector of colours for
#'   `color_by` categories. Default: NULL.
#'
#'
#' @return A ggplot2 object for further customisation and display.
#'
#' @details
#' When `color_by` is NULL, scenarios are coloured by default:
#' — baseline scenario (scenario_id == "0"): blue (#6ABDEB);
#' — alternative scenarios: green (#A4CF96).
#' Boxplots show all uncertainty iterations for statistical accuracy;
#' scatter points are sampled to improve readability with many variates.
#'
#' @examples
#' # Basic plot using mcmodule and mc_name
#' mc_plot(imports_mcmodule, "w_prev")
#'
#' # Plot with custom coloring and ordering
#' mc_plot(imports_mcmodule, "w_prev",
#'   color_by = "origin",
#'   order_by = "median"
#' )
#'
#' # Plot with threshold and scale transformation
#' mc_plot(imports_mcmodule, "no_detect_a",
#'   threshold = 0.5,
#'   scale = "log10"
#' )
#'
#' @export
mc_plot <- function(
  mcmodule = NULL,
  mc_name = NULL,
  mcnode = NULL,
  data = NULL,
  keys_names = NULL,
  color_by = NULL,
  order_by = NULL,
  group_by = NULL,
  filter = NULL,
  threshold = NULL,
  scale = NULL,
  max_dots = 300,
  point_alpha = 0.4,
  boxplot_alpha = 0.3,
  color_pal = NULL
) {
  # Input validation
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "ggplot2 is required for mc_plot. Install it using: install.packages('ggplot2')"
    )
  }

  # Capture filter expression only if filter is not NULL
  if (
    !is.null(substitute(filter)) && !identical(substitute(filter), quote(NULL))
  ) {
    filter_expr <- substitute(filter)
    long_df <- tidy_mcnode(
      mcmodule = mcmodule,
      mc_name = mc_name,
      mcnode = mcnode,
      data = data,
      keys_names = keys_names,
      filter = eval(filter_expr)
    )
  } else {
    # Convert to long format without filter
    long_df <- tidy_mcnode(
      mcmodule = mcmodule,
      mc_name = mc_name,
      mcnode = mcnode,
      data = data,
      keys_names = keys_names
    )
  } # Y-axis will show individual variates (scenarios/rows from data)
  # Determine the grouping variable for y-axis labels
  key_cols <- setdiff(names(long_df), c("variate", "simulation", "value"))

  if (length(key_cols) > 0) {
    # Create combined label from all key columns
    if (length(key_cols) == 1) {
      long_df$y_label <- as.character(long_df[[key_cols[1]]])
    } else {
      # Combine multiple keys into a single label
      long_df$y_label <- apply(
        long_df[, key_cols, drop = FALSE],
        1,
        function(x) paste(x, collapse = " | ")
      )
    }
  } else {
    # Use row_id if no other keys
    long_df$y_label <- as.character(long_df$variate)
  }

  # Default grouping: group by all keys except scenario_id
  if (is.null(group_by)) {
    group_by_cols <- setdiff(key_cols, "scenario_id")
  } else {
    group_by_cols <- group_by
  }

  # Handle grouping to organize variates
  if (length(group_by_cols) > 0 && all(group_by_cols %in% key_cols)) {
    # Sort so that for each group, scenario "0" appears first (baseline),
    # then all other scenarios for that group appear together
    # First order by group_by columns, then ensure scenario "0" comes first
    other_keys <- setdiff(key_cols, group_by_cols)

    if (length(other_keys) > 0) {
      sort_cols <- c(group_by_cols, other_keys)
    } else {
      sort_cols <- group_by_cols
    }

    # Verify all sort_cols exist in long_df
    if (!all(sort_cols %in% names(long_df))) {
      # Skip grouping if required columns are missing
      warning("Required columns for grouping not found in data")
    } else {
      # Create a sort order: group_by cols, then scenario_id (0 first), then other keys
      if ("scenario_id" %in% other_keys) {
        # Separate scenario_id from other keys
        other_keys_no_scenario <- setdiff(other_keys, "scenario_id")
        if (length(other_keys_no_scenario) > 0) {
          sort_cols <- c(group_by_cols, "scenario_id", other_keys_no_scenario)
        } else {
          sort_cols <- c(group_by_cols, "scenario_id")
        }

        # Create sort order dataframe
        unique_combos <- unique(long_df[, sort_cols, drop = FALSE])

        # Sort: group_by cols (ascending), then scenario "0" first, then other scenarios (ascending)
        unique_combos <- unique_combos[
          order(
            do.call(paste, unique_combos[, group_by_cols, drop = FALSE]), # Group cols
            unique_combos$scenario_id != "0", # "0" comes first (FALSE < TRUE)
            unique_combos$scenario_id # Then alphabetically
          ),
        ]
      } else {
        unique_combos <- unique(long_df[, sort_cols, drop = FALSE])
        unique_combos <- unique_combos[
          order(do.call(paste, unique_combos[, sort_cols, drop = FALSE])),
        ]
      }

      # Handle empty unique_combos case
      if (is.data.frame(unique_combos) && nrow(unique_combos) > 0) {
        unique_combos$sort_order <- seq_len(nrow(unique_combos))

        # Merge sort order back and reorder
        long_df <- merge(long_df, unique_combos, by = sort_cols)
        long_df <- long_df[order(long_df$sort_order, long_df$simulation), ]
        long_df$sort_order <- NULL
      }

      # Recreate y_label in the new order to reflect grouped structure
      if (length(key_cols) == 1) {
        long_df$y_label <- as.character(long_df[[key_cols[1]]])
      } else {
        long_df$y_label <- apply(
          long_df[, key_cols, drop = FALSE],
          1,
          function(x) paste(x, collapse = " | ")
        )
      }

      # Factor y_label to preserve the sorted order
      long_df$y_label <- factor(
        long_df$y_label,
        levels = rev(unique(long_df$y_label))
      )
    }
  }

  # Add default scenario coloring if no color_by specified
  if (is.null(color_by) && "scenario_id" %in% key_cols) {
    # Get the first scenario value to highlight (baseline scenario)
    baseline_scenario <- long_df$scenario_id[1]
    long_df$scenario_color <- ifelse(
      long_df$scenario_id == baseline_scenario,
      "baseline",
      "alternative"
    )
  }

  # Adapt max_dots based on number of variates
  n_variates <- length(unique(long_df$variate))
  if (n_variates < 10) {
    adjusted_max_dots <- max_dots
  } else if (n_variates < 20) {
    adjusted_max_dots <- 100
  } else {
    adjusted_max_dots <- 0
    message(
      sprintf(
        "Plotting %d variates: showing only boxplots (no individual points). ",
        n_variates
      ),
      "Use max_dots parameter to override."
    )
  }

  # Sampling: select which simulation dots to plot per variate
  n_simulations <- length(unique(long_df$simulation))
  if (adjusted_max_dots > 0 && n_simulations > adjusted_max_dots) {
    # Use regular intervals for representative sampling
    simulation_indices <- round(seq(
      1,
      n_simulations,
      length.out = adjusted_max_dots
    ))
  } else if (adjusted_max_dots > 0) {
    simulation_indices <- seq_len(n_simulations)
  } else {
    # No dots, empty index
    simulation_indices <- integer(0)
  }

  # Create separate data: all simulations for boxplot, sampled for points
  long_df_boxplot <- long_df
  long_df_points <- long_df[long_df$simulation %in% simulation_indices, ]

  # Handle ordering by median if requested
  if (!is.null(order_by) && order_by == "median") {
    # Order variates by median value
    median_vals <- stats::aggregate(
      long_df_boxplot$value,
      list(y_label = long_df_boxplot$y_label),
      stats::median
    )
    ordered_labels <- median_vals$y_label[order(median_vals$x)]
    long_df_boxplot$y_label <- factor(
      long_df_boxplot$y_label,
      levels = ordered_labels
    )
    long_df_points$y_label <- factor(
      long_df_points$y_label,
      levels = ordered_labels
    )
  }

  # Create base plot with flipped axes (variates on y-axis, values on x-axis)
  # Start with boxplot using ALL variates
  p <- ggplot2::ggplot(
    long_df_boxplot,
    ggplot2::aes(x = .data$value, y = .data$y_label)
  )

  # Add boxplot using all variates with optional color mapping
  if (!is.null(color_by) && color_by %in% names(long_df_boxplot)) {
    p <- p +
      ggplot2::geom_boxplot(
        ggplot2::aes(fill = .data[[color_by]]),
        alpha = boxplot_alpha,
        outlier.alpha = 0,
        color = "gray30"
      )
  } else if (!is.null(long_df_boxplot$scenario_color)) {
    # Default scenario coloring
    p <- p +
      ggplot2::geom_boxplot(
        ggplot2::aes(fill = .data$scenario_color),
        alpha = boxplot_alpha,
        outlier.alpha = 0,
        color = "gray30"
      )
  } else {
    p <- p +
      ggplot2::geom_boxplot(
        alpha = boxplot_alpha,
        outlier.alpha = 0,
        color = "gray30",
        fill = "gray80"
      )
  }

  # Add min/max markers to boxplot
  min_max_df <- stats::aggregate(
    long_df_boxplot$value,
    list(y_label = long_df_boxplot$y_label),
    function(x) c(min = min(x), max = max(x))
  )
  min_max_df <- data.frame(
    y_label = min_max_df$y_label,
    min_value = min_max_df$x[, "min"],
    max_value = min_max_df$x[, "max"]
  )

  p <- p +
    ggplot2::geom_point(
      data = min_max_df,
      ggplot2::aes(x = .data$min_value, y = .data$y_label),
      shape = "|",
      size = 4,
      color = "gray30"
    ) +
    ggplot2::geom_point(
      data = min_max_df,
      ggplot2::aes(x = .data$max_value, y = .data$y_label),
      shape = "|",
      size = 4,
      color = "gray30"
    )

  # Add sampled points with optional color mapping (only if dots enabled)
  if (length(simulation_indices) > 0) {
    if (!is.null(color_by) && color_by %in% names(long_df_points)) {
      p <- p +
        ggplot2::geom_point(
          data = long_df_points,
          ggplot2::aes(color = .data[[color_by]]),
          alpha = point_alpha,
          position = ggplot2::position_jitter(width = 0, height = 0.15),
          size = 2
        )
    } else if (!is.null(long_df_points$scenario_color)) {
      # Default scenario coloring
      p <- p +
        ggplot2::geom_point(
          data = long_df_points,
          ggplot2::aes(color = .data$scenario_color),
          alpha = point_alpha,
          position = ggplot2::position_jitter(width = 0, height = 0.15),
          size = 2
        )
    } else {
      p <- p +
        ggplot2::geom_point(
          data = long_df_points,
          alpha = point_alpha,
          position = ggplot2::position_jitter(width = 0, height = 0.15),
          size = 2,
          color = "gray50"
        )
    }
  }

  # Apply color palette if provided or use default
  if (!is.null(color_by) && color_by %in% names(long_df_points)) {
    if (is.null(color_pal)) {
      # Default color palette from mc_network.R
      default_pal <- c(
        inputs = "#B0DFF9",
        in_node = "#6ABDEB",
        out_node = "#A4CF96",
        trials_info = "#FAE4CB",
        total = "#F39200",
        agg_total = "#C17816"
      )
      color_pal <- default_pal
    }

    # Get unique values in color_by column
    unique_vals <- unique(long_df_points[[color_by]])

    # If color_pal has names, use named mapping; otherwise cycle through colors
    if (!is.null(names(color_pal))) {
      # Named palette - map values that exist in names
      mapped_colors <- color_pal[unique_vals]
      # For values not in palette, cycle through available colors
      unmapped_idx <- is.na(mapped_colors)
      if (any(unmapped_idx)) {
        mapped_colors[unmapped_idx] <- color_pal[seq_len(sum(unmapped_idx))]
      }
      names(mapped_colors) <- unique_vals
    } else {
      # Unnamed palette - cycle through colors
      mapped_colors <- color_pal[
        seq_along(unique_vals) %% length(color_pal) + 1
      ]
      names(mapped_colors) <- unique_vals
    }

    p <- p +
      ggplot2::scale_color_manual(values = mapped_colors, na.value = "gray50") +
      ggplot2::scale_fill_manual(values = mapped_colors, na.value = "gray80")
  } else if (!is.null(long_df_points$scenario_color)) {
    # Default scenario coloring: blue for baseline, green for alternatives
    scenario_colors <- c(baseline = "#6ABDEB", alternative = "#A4CF96")
    p <- p +
      ggplot2::scale_color_manual(values = scenario_colors) +
      ggplot2::scale_fill_manual(values = scenario_colors)
  }

  # Add threshold line if specified (vertical line since value is on x-axis)
  if (!is.null(threshold)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = threshold,
        linetype = "dashed",
        color = "red",
        linewidth = 0.8,
        alpha = 0.7
      )
  }

  # Apply scale transformation if specified (to x-axis since value is on x-axis)
  x_axis_label <- if (!is.null(mc_name)) mc_name else "Value"

  if (!is.null(scale)) {
    if (scale == "log10") {
      p <- p + ggplot2::scale_x_log10()
      x_axis_label <- paste(x_axis_label, "(log10 scale)")
    } else if (scale == "log") {
      p <- p + ggplot2::scale_x_continuous(trans = "log")
      x_axis_label <- paste(x_axis_label, "(log scale)")
    } else if (scale == "sqrt") {
      p <- p + ggplot2::scale_x_sqrt()
      x_axis_label <- paste(x_axis_label, "(sqrt scale)")
    } else if (scale == "asinh") {
      p <- p + ggplot2::scale_x_continuous(trans = "asinh")
      x_axis_label <- paste(x_axis_label, "(asinh scale)")
    } else if (scale == "identity") {
      x_axis_label <- paste(x_axis_label, "(linear scale)")
    }
  }

  # Add labels and theme
  p <- p +
    ggplot2::labs(
      x = x_axis_label,
      y = "Variate",
      title = if (!is.null(mc_name)) {
        paste("Monte Carlo Plot:", mc_name)
      } else {
        "Monte Carlo Plot"
      }
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      legend.position = if (!is.null(color_by)) "right" else "none",
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  return(p)
}
