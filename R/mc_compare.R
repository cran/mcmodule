#' Compare Monte Carlo Node Against Baseline Scenario
#'
#' Compares an mcnode's what-if scenarios against a baseline scenario (default "0")
#' using various comparison metrics. Returns an mcmodule with a new comparison node.
#'
#' @param mcmodule (mcmodule object). Module containing the node.
#' @param mc_name (character). Name of the mcnode to compare.
#' @param baseline (character). Baseline scenario ID to compare against. Default: "0".
#' @param type (character). Type of comparison. One of:
#'   \itemize{
#'     \item "difference" (default): whatif - baseline (absolute change)
#'     \item "relative_difference": (whatif - baseline) / baseline (proportional change)
#'     \item "reduction": baseline - whatif (absolute reduction)
#'     \item "relative_reduction": (baseline - whatif) / baseline (proportional reduction)
#'   }
#' @param keys_names (character vector, optional). Column names for grouping.
#'   If NULL, uses keys from the node. Default: NULL.
#' @param name (character, optional). Name for the new comparison node. If NULL,
#'   auto-generated from `mc_name` and `suffix`. Default: NULL.
#' @param prefix (character, optional). Prefix for the auto-generated node name.
#'   Default: NULL.
#' @param suffix (character). Suffix appended to auto-generated name.
#'   Default: "compared".
#' @param summary (logical). If TRUE, compute summary statistics for the new node.
#'   Default: TRUE.
#' @param align_uncertainty (logical). If TRUE, align uncertainty iterations between
#'   baseline and what-if nodes using rank correlation (Spearman). This ensures that
#'   the same uncertainty iteration in both nodes represents similar uncertainty
#'   realizations, making comparisons more meaningful when nodes have multivariate
#'   dimensions. Default: TRUE.
#'
#' @details
#' This function compares what-if scenarios against a baseline by:
#' \enumerate{
#'   \item Filtering the baseline scenario (scenario_id == baseline)
#'   \item Filtering what-if scenarios (scenario_id != baseline)
#'   \item Matching them across scenarios using keys
#'   \item Optionally aligning uncertainty iterations using rank correlation
#'   \item Applying the selected comparison formula
#'   \item Creating a new comparison node in the mcmodule
#' }
#'
#' When `align_uncertainty = TRUE`, the function uses [mc2d::cornode()] to align
#' the uncertainty iterations between matched baseline and what-if nodes. For
#' multivariate nodes, correlation is applied independently to each variate.
#'
#' For derived nodes with pre-computed summaries (types `"filter"`, `"compare"`,
#' or `"agg_total"`), scenario filtering and key alignment use the node's
#' `summary` by default as the source data.
#'
#' The baseline scenario must contain all key combinations present in what-if
#' scenarios. If what-if scenarios are missing key combinations present in
#' baseline, those are interpreted as having baseline values (no change).
#'
#' @return Updated mcmodule with a new comparison node containing:
#'   \itemize{
#'     \item mcnode: Comparison values as mcnode object
#'     \item type: "compare"
#'     \item baseline: Baseline scenario ID
#'     \item compare_type: Type of comparison performed
#'     \item param: Original node name
#'     \item inputs: Original node name
#'     \item keys: Same keys as original node
#'     \item summary: Summary statistics (if summary = TRUE)
#'   }
#'
#' @examples
#' # Create example data with baseline and what-if scenarios
#' example_data <- data.frame(
#'   origin = c("A", "B", "A", "B"),
#'   scenario_id = c("0", "0", "1", "1")
#' )
#'
#' # Create mcnodes for each scenario
#' example_mcnode <- mc2d::mcstoc(
#'   runif,
#'   min = mc2d::mcdata(c(0.1, 0.2, 0.15, 0.25), type = "0", nvariates = 4),
#'   max = mc2d::mcdata(c(0.2, 0.3, 0.25, 0.35), type = "0", nvariates = 4),
#'   nvariates = 4
#' )
#'
#' # Create mcmodule
#' example_module <- list(
#'   data = list(example_data = example_data),
#'   node_list = list(
#'     risk = list(
#'       mcnode = example_mcnode,
#'       data_name = "example_data",
#'       keys = c("origin")
#'     )
#'   )
#' )
#'
#' # Compare what-if scenario "1" against baseline "0"
#' result <- mc_compare(
#'   example_module,
#'   "risk",
#'   baseline = "0",
#'   type = "relative_reduction"
#' )
#'
#' # View comparison results
#' result$node_list$risk_compared$summary
#' @export
mc_compare <- function(
  mcmodule,
  mc_name,
  baseline = "0",
  type = "difference",
  keys_names = NULL,
  name = NULL,
  prefix = NULL,
  suffix = "compared",
  summary = TRUE,
  align_uncertainty = TRUE
) {
  scenario_id <- NULL

  # Capture module name before processing
  module_name <- deparse(substitute(mcmodule))

  # Input validation
  if (!is.list(mcmodule) || is.null(mcmodule$node_list)) {
    stop("Invalid mcmodule structure")
  }

  if (!mc_name %in% names(mcmodule$node_list)) {
    stop(sprintf(
      "%s not found in %s",
      mc_name,
      module_name
    ))
  }

  # Validate type parameter
  valid_types <- c(
    "difference",
    "relative_difference",
    "reduction",
    "relative_reduction"
  )
  if (!type %in% valid_types) {
    stop(sprintf(
      "Invalid type '%s'. Must be one of: %s",
      type,
      paste(valid_types, collapse = ", ")
    ))
  }

  # Get node information
  node <- mcmodule$node_list[[mc_name]]
  mcnode <- node[["mcnode"]]
  data_name <- node[["data_name"]]
  node_type <- node[["type"]]
  uses_summary_data <-
    !is.null(node[["summary"]]) &&
    !is.null(node_type) &&
    node_type %in% c("filter", "compare", "agg_total")

  # Get keys from node if not provided
  if (is.null(keys_names)) {
    keys_names <- node[["keys"]]
  }

  # Get data and validate scenarios exist
  # For derived nodes, use summary as the source of truth
  if (uses_summary_data) {
    data <- node[["summary"]]
  } else if (length(data_name) > 1) {
    if (is.null(node[["summary"]])) {
      stop(sprintf(
        "%s has multiple data_names but no summary. Cannot determine scenarios.",
        mc_name
      ))
    }
    data <- node[["summary"]]
  } else {
    data <- mcmodule$data[[data_name]]
  }

  # Ensure scenario_id exists
  if (!"scenario_id" %in% names(data)) {
    data$scenario_id <- "0"
  }

  # Validate baseline exists
  if (!baseline %in% data$scenario_id) {
    stop(sprintf(
      "Baseline scenario '%s' not found in %s. Available scenarios: %s",
      baseline,
      mc_name,
      paste(unique(data$scenario_id), collapse = ", ")
    ))
  }

  # Check if there are any what-if scenarios
  whatif_scenarios <- unique(data$scenario_id[data$scenario_id != baseline])
  if (length(whatif_scenarios) == 0) {
    stop(sprintf(
      "No what-if scenarios found in %s (only baseline '%s' exists)",
      mc_name,
      baseline
    ))
  }

  # Generate temporary node names
  temp_baseline_name <- paste0(".temp_baseline_", mc_name)
  temp_whatif_name <- paste0(".temp_whatif_", mc_name)

  # For derived nodes, extract variates from the mcnode directly using summary data.
  # For regular nodes, use mc_filter to handle filtering and summary creation.
  if (uses_summary_data) {
    # For derived nodes, filter summary data and extract corresponding variates from mcnode
    baseline_data <- data[
      data$scenario_id == baseline,
      ,
      drop = FALSE
    ]
    whatif_data <- data[
      data$scenario_id != baseline,
      ,
      drop = FALSE
    ]

    # Get scenario indices from filtered summary data
    baseline_indices <- which(data$scenario_id == baseline)
    whatif_indices <- which(data$scenario_id != baseline)

    # Extract variates corresponding to each scenario
    if (length(baseline_indices) > 0) {
      if (length(baseline_indices) == 1) {
        mcnode_baseline <- mc2d::extractvar(mcnode, baseline_indices[1])
      } else {
        mcnode_baseline <- mc2d::extractvar(mcnode, baseline_indices[1])
        for (i in baseline_indices[-1]) {
          mcnode_baseline <- mc2d::addvar(
            mcnode_baseline,
            mc2d::extractvar(mcnode, i)
          )
        }
      }
    }

    if (length(whatif_indices) > 0) {
      if (length(whatif_indices) == 1) {
        mcnode_whatif <- mc2d::extractvar(mcnode, whatif_indices[1])
      } else {
        mcnode_whatif <- mc2d::extractvar(mcnode, whatif_indices[1])
        for (i in whatif_indices[-1]) {
          mcnode_whatif <- mc2d::addvar(
            mcnode_whatif,
            mc2d::extractvar(mcnode, i)
          )
        }
      }
    }

    # Add the extracted nodes to the module as filtered nodes
    mcmodule$node_list[[temp_baseline_name]] <- list(
      mcnode = mcnode_baseline,
      type = "filter",
      param = mc_name,
      inputs = mc_name,
      keys = keys_names,
      data_name = data_name,
      summary = baseline_data
    )

    mcmodule$node_list[[temp_whatif_name]] <- list(
      mcnode = mcnode_whatif,
      type = "filter",
      param = mc_name,
      inputs = mc_name,
      keys = keys_names,
      data_name = data_name,
      summary = whatif_data
    )
  } else {
    # Filter baseline scenario
    mcmodule <- mc_filter(
      mcmodule,
      mc_name,
      scenario_id == baseline,
      name = temp_baseline_name,
      suffix = "",
      summary = TRUE
    )

    # Filter what-if scenarios
    mcmodule <- mc_filter(
      mcmodule,
      mc_name,
      scenario_id != baseline,
      name = temp_whatif_name,
      suffix = "",
      summary = TRUE
    )
  }

  # Get the baseline and what-if summaries to validate completeness
  baseline_summary <- mcmodule$node_list[[temp_baseline_name]]$summary
  whatif_summary <- mcmodule$node_list[[temp_whatif_name]]$summary

  # Determine keys available for matching baseline and what-if nodes.
  available_match_keys <- intersect(
    names(baseline_summary),
    names(whatif_summary)
  )
  effective_keys_names <- intersect(keys_names, available_match_keys)

  # Validate that baseline has all key combinations present in what-if scenarios.
  # Skip check if no explicit keys are defined (comparison will use direct variate matching).
  if (!is.null(whatif_summary) && !is.null(baseline_summary)) {
    keys_for_check <- setdiff(effective_keys_names, "scenario_id")

    if (length(keys_for_check) > 0) {
      # Get unique key combinations from each
      baseline_keys_only <- baseline_summary[keys_for_check]
      whatif_keys_only <- whatif_summary[keys_for_check]

      # Check if all what-if keys exist in baseline
      whatif_keys_df <- unique(whatif_keys_only)
      baseline_keys_df <- unique(baseline_keys_only)

      # Find what-if keys not in baseline
      missing_in_baseline <- setdiff(
        apply(whatif_keys_df, 1, paste, collapse = "|"),
        apply(baseline_keys_df, 1, paste, collapse = "|")
      )

      if (length(missing_in_baseline) > 0) {
        stop(
          sprintf(
            "Baseline scenario '%s' is incomplete. Missing key combinations in baseline: %s",
            baseline,
            paste(missing_in_baseline, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }
  }

  # Match baseline and what-if variates using explicit keys.
  # When no keys are available (aggregated nodes), directly compare the single
  # baseline variate against each what-if variate without matching.
  if (
    length(effective_keys_names) == 0 &&
      dim(mcmodule$node_list[[temp_baseline_name]]$mcnode)[3] == 1
  ) {
    baseline_single <- mcmodule$node_list[[temp_baseline_name]]$mcnode
    whatif_matched <- mcmodule$node_list[[temp_whatif_name]]$mcnode

    # Recycle the single baseline variate across all what-if variates
    n_whatif <- dim(whatif_matched)[3]
    baseline_matched <- baseline_single
    if (n_whatif > 1) {
      for (i in 2:n_whatif) {
        baseline_matched <- mc2d::addvar(baseline_matched, baseline_single)
      }
    }
    keys_xy <- whatif_summary
  } else {
    match_result <- mc_match(
      mcmodule,
      temp_baseline_name,
      temp_whatif_name,
      keys_names = effective_keys_names,
      match_scenario = FALSE
    )

    baseline_matched <- match_result[[paste0(temp_baseline_name, "_match")]]
    whatif_matched <- match_result[[paste0(temp_whatif_name, "_match")]]
    keys_xy <- match_result$keys_xy
  }

  # Correlate matched nodes for uncertainty alignment
  # Only apply correlation if nodes have uncertainty (nsv > 1)
  if (align_uncertainty && dim(whatif_matched)[1] > 1) {
    # For multivariate nodes, cornode needs a 2x2 correlation matrix
    # Use 0.999 instead of 1.0 to ensure positive definiteness
    target_cor <- matrix(c(1, 0.999, 0.999, 1), ncol = 2)

    cornodes <- mc2d::cornode(
      node1 = whatif_matched,
      node2 = baseline_matched,
      target = target_cor,
      result = FALSE
    )
    whatif_matched <- cornodes[[1]]
    baseline_matched <- cornodes[[2]]
  }

  # Apply comparison formula
  comparison_node <- switch(
    type,
    "difference" = whatif_matched - baseline_matched,
    "relative_difference" = (whatif_matched - baseline_matched) /
      baseline_matched,
    "reduction" = baseline_matched - whatif_matched,
    "relative_reduction" = (baseline_matched - whatif_matched) /
      baseline_matched,
    stop("Unknown comparison type")
  )

  # Handle NA and Inf values from division by zero
  comparison_node <- mcnode_na_rm(comparison_node, na_value = 0)

  # Generate comparison node name
  normalized_suffix <- if (!is.null(suffix) && suffix != "") {
    sub("^_+", "", suffix)
  } else {
    ""
  }

  compare_mc_name <- if (!is.null(name)) {
    if (normalized_suffix != "") {
      paste0(name, "_", normalized_suffix)
    } else {
      name
    }
  } else {
    if (normalized_suffix != "") {
      paste0(mc_name, "_", normalized_suffix)
    } else {
      paste0(mc_name, "_cmp")
    }
  }

  # Add prefix if provided
  if (!is.null(prefix) && prefix != "") {
    prefix <- paste0(sub("_$", "", prefix), "_")
    compare_mc_name <- paste0(
      prefix,
      sub(paste0("^", prefix), "", compare_mc_name)
    )
    prefix <- sub("_$", "", prefix)
  }

  # Create comparison description
  type_description <- switch(
    type,
    "difference" = "difference (whatif - baseline)",
    "relative_difference" = "relative difference ((whatif - baseline) / baseline)",
    "reduction" = "reduction (baseline - whatif)",
    "relative_reduction" = "relative reduction ((baseline - whatif) / baseline)",
    type
  )

  # Prepare data for summary (extract what-if scenarios from keys_xy)
  compare_data <- keys_xy[keys_xy$scenario_id != baseline, ]
  # Remove technical columns
  compare_data <- compare_data[
    !names(compare_data) %in% c("g_id", "g_row.x", "g_row.y")
  ]
  # Add variate numbers
  compare_data$variate <- seq_len(nrow(compare_data))

  # Store comparison node
  mcmodule$node_list[[compare_mc_name]] <- list(
    mcnode = comparison_node,
    type = "compare",
    param = mc_name,
    inputs = mc_name,
    baseline = baseline,
    compare_type = type,
    description = sprintf(
      "Comparison of %s: %s vs baseline '%s'",
      mc_name,
      type_description,
      baseline
    ),
    module = module_name,
    keys = effective_keys_names,
    node_expression = sprintf(
      "mc_compare(%s, baseline = '%s', type = '%s')",
      mc_name,
      baseline,
      type
    ),
    scenario = compare_data$scenario_id,
    data_name = data_name,
    prefix = if (!is.null(prefix)) prefix else NULL
  )

  # Add summary if requested
  if (summary && nrow(compare_data) > 0) {
    # Include scenario_id in summary if it exists
    summary_keys <- effective_keys_names
    if (
      "scenario_id" %in%
        names(compare_data) &&
        !("scenario_id" %in% effective_keys_names)
    ) {
      summary_keys <- c("scenario_id", effective_keys_names)
    }

    # Create a temporary data frame for summary
    mcmodule$node_list[[compare_mc_name]][["summary"]] <-
      mc_summary(
        mcmodule = mcmodule,
        data = compare_data,
        mc_name = compare_mc_name,
        keys_names = summary_keys
      )
  }

  # Clean up temporary nodes
  mcmodule$node_list[[temp_baseline_name]] <- NULL
  mcmodule$node_list[[temp_whatif_name]] <- NULL

  return(mcmodule)
}
