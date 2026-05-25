#' Check Dimension Compatibility of Monte Carlo Nodes
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Validates that all mcnodes in a module have compatible dimensions for
#' sensitivity analysis by checking uncertainty and variate dimensions.
#'
#' @param mcmodule (mcmodule object). Module containing nodes.
#' @param mc_names (character vector, optional). Node names to check. If NULL,
#'   checks all nodes. Default: NULL.
#'
#' @return A list with: `n_mcnodes` (count), `n_variate` (variate count),
#'   `n_uncertainty` (uncertainty simulation count).
mcmodule_dim_check <- function(mcmodule, mc_names = NULL) {
  mc_names <- mc_names %||% names(mcmodule$node_list)

  # Check that all mcnodes have 1 or the same number of uncertainty values
  n_uncertainty <- unlist(unique(sapply(mc_names, function(x) {
    dim(mcmodule$node_list[[x]][["mcnode"]])[1]
  })))

  n_uncertainty <- n_uncertainty[n_uncertainty != 1]

  if (length(unique(n_uncertainty)) > 1) {
    stop(
      "All mcnode objects must have the same number of uncertanty simulations or no uncertainty."
    )
  }

  # Check that all mcnodes have 1 or the same number of variate simulations
  n_variate <- unlist(unique(sapply(mc_names, function(x) {
    dim(mcmodule$node_list[[x]][["mcnode"]])[3]
  })))

  n_variate <- n_variate[n_variate != 1]
  if (length(unique(n_variate)) > 1) {
    stop(
      "All mcnode objects must have the same number of variate simulations or one variate"
    )
  }

  list(
    n_mcnodes = length(mc_names),
    n_variate = ifelse(length(n_variate) == 0, 1, n_variate),
    n_uncertainty = ifelse(length(n_uncertainty) == 0, 1, n_uncertainty)
  )
}

#' Convert Monte Carlo Module to Matrices
#'
#' Transforms an mcmodule into a list of matrices, with one matrix per variate.
#' Each matrix has uncertainty simulations as rows and mcnodes as columns.
#'
#' @param mcmodule (mcmodule object). Module to convert.
#' @param mc_names (character vector, optional). Node names to include. If NULL,
#'   includes all nodes. Default: NULL.
#'
#' @return A list of matrices (one per variate). Each matrix has uncertainty
#'   simulations as rows and mcnodes as columns.
mcmodule_to_matrices <- function(mcmodule, mc_names = NULL) {
  mc_names <- mc_names[mc_names %in% names(mcmodule$node_list)]
  dims <- mcmodule_dim_check(mcmodule, mc_names)
  # Initialize list to store matrices: one per n_variate
  matrices <- vector("list", dims$n_variate)
  # Intitialize matrices (n_uncertainty x n_mcnodes)
  matrices <- lapply(matrices, function(x) {
    matrix(nrow = dims$n_uncertainty, ncol = dims$n_mcnodes)
  })

  for (i in seq_along(mc_names)) {
    mcnode_i <- mcmodule$node_list[[mc_names[i]]][["mcnode"]]
    for (j in seq_len(dim(mcnode_i)[3])) {
      variate_i_j <- mcnode_i[,, j]

      if (length(variate_i_j) == 1) {
        variate_i_j <- rep(variate_i_j, dims$n_uncertainty)
      }

      matrices[[j]][, i] <- variate_i_j
    }
  }
  matrices
}

#' Convert Monte Carlo Module to `mc2d` Objects
#'
#' Converts an mcmodule into one or more mc objects (from the mc2d package).
#' Returns either one mc object per variate or a single mc object with all
#' variates combined into the variability dimension.
#'
#' @param mcmodule (mcmodule object). Module to convert.
#' @param mc_names (character vector, optional). Node names to include. If NULL,
#'   includes all nodes. Default: NULL.
#' @param match (logical, unused). Reserved for future functionality. Default: FALSE.
#' @param variates_as_nsv (logical). If TRUE, combine all variates into a single
#'   mc object by multiplying variates by uncertainty simulations in the
#'   variability dimension. If FALSE, return one mc object per variate.
#'   Default: FALSE.
#'
#' @return If `variates_as_nsv = FALSE`, a list of mc objects (one per variate).
#'   If `variates_as_nsv = TRUE`, a single mc object with all variates combined
#'   into the variability dimension. Each mc object is compatible with mc2d
#'   package functions.
mcmodule_to_mc <- function(
  mcmodule,
  mc_names = NULL,
  match = FALSE,
  variates_as_nsv = FALSE
) {
  mc_names <- mc_names %||% names(mcmodule$node_list)
  dims <- mcmodule_dim_check(mcmodule, mc_names)

  mc_list <- vector("list", if (variates_as_nsv) 1 else dims$n_variate)

  for (i in seq_along(mc_names)) {
    mcnode_i <- mcmodule$node_list[[mc_names[i]]][["mcnode"]]
    if (variates_as_nsv) {
      if (dim(mcnode_i)[1] > 1) {
        mcnode_i <- mcdata(
          c(unmc(mcnode_i)),
          type = "V",
          nsv = dims$n_variate * dims$n_uncertainty
        )
      } else {
        mcnode_i <- mcdata(
          rep(unmc(mcnode_i), dims$n_uncertainty),
          type = "V",
          nsv = dims$n_variate * dims$n_uncertainty
        )
      }
    }

    for (j in seq_len(dim(mcnode_i)[3])) {
      variate_i_j <- extractvar(mcnode_i, j)
      if (is.null(mc_list[[j]])) {
        mc_list[[j]] <- mc(variate_i_j, name = mc_names[i])
      } else {
        mc_list[[j]] <- mc(mc_list[[j]], mc(variate_i_j, name = mc_names[i]))
      }
    }
  }
  if (variates_as_nsv) {
    return(mc_list[[1]])
  } else {
    return(mc_list)
  }
}

#' Calculate Correlation Coefficients Between Inputs and Outputs
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Computes correlation coefficients between mcmodule inputs and outputs using
#' tornado analysis (from the `mc2d` package). Supports multiple correlation methods
#' and captures warnings generated during calculation.
#'
#' @param mcmodule (mcmodule object). Module containing simulation results.
#' @param output (character, optional). Output node name. If NULL (default), uses
#'   the last node in `mcmodule$node_list`. If `by_exp = TRUE`, uses the last
#'   output node per expression. Default: NULL.
#' @param by_exp (logical). If TRUE, calculate correlations by expression output;
#'   if FALSE, use global output (last node). Default: FALSE.
#' @param match_variates (logical). If TRUE, match input nodes to output variates
#'   when data dimensions differ. Default: TRUE.
#' @param variates_as_nsv (logical). If TRUE, combine all variates into one `mc`
#'   object; if FALSE, analyse each variate separately. See [mcmodule_to_mc()].
#'   Default: FALSE.
#' @param print_summary (logical). If TRUE, print correlation analysis summary.
#'   Default: TRUE.
#' @param progress (logical). If TRUE, print progress information while running.
#'   Default: FALSE.
#' @param method (character). Correlation coefficient type: "spearman" (default),
#'   "kendall", or "pearson". See [stats::cor()]. Default: "spearman".
#' @param use (character). Method for handling missing values: "all.obs",
#'   "complete.obs", or "pairwise.complete.obs". See [stats::cor()].
#'   Default: "all.obs".
#' @param lim (numeric vector). Quantiles for credible interval computation (reserved
#'   for two-dimensional models). Default: `c(0.025, 0.975)`.
#' @param mc_names (character vector, optional). Node names to include in analysis.
#'   If NULL (default), includes all nodes in the module. Default: NULL.
#' @param plot (logical). If TRUE, plots a tornado plot generated from the
#'   computed correlation table using [mcmodule_tornado()]. Default: FALSE.
#' @return A data frame with correlation coefficients and metadata. Columns include:
#'   \itemize{
#'     \item exp: Expression name
#'     \item exp_n: Expression number
#'     \item variate: Variate number
#'     \item output: Output node names
#'     \item input: Input node name
#'     \item value: Correlation coefficient value
#'     \item strength: Qualitative strength of association (Very strong, Strong, Moderate, Weak, Very weak/None)
#'     \item method: Correlation method used (spearman, kendall, or pearson)
#'     \item use: Method for handling missing values (passed to the correlation function)
#'     \item warnings: Any warnings generated during correlation calculation (if present)
#'     \item Additional columns for global keys (e.g., pathogen, origin)
#'   }
#' @export
#'
#' @examples
#' mcmodule <- agg_totals(
#'   mcmodule = imports_mcmodule,
#'   mc_name = "no_detect",
#'   agg_keys = "pathogen"
#' )
#' cor_results <- mcmodule_corr(mcmodule)
#'
#' # Use single method
#' cor_results_spearman <- mcmodule_corr(mcmodule, method = "spearman")
#'
mcmodule_corr <- function(
  mcmodule,
  output = NULL,
  by_exp = FALSE,
  match_variates = TRUE,
  variates_as_nsv = FALSE,
  mc_names = NULL,
  print_summary = TRUE,
  progress = FALSE,
  method = c("spearman", "kendall", "pearson"),
  use = "all.obs",
  lim = c(0.025, 0.975),
  plot = FALSE
) {
  info <- mcmodule_info(mcmodule)
  module_names <- unique(info$module_exp_data$module)
  total_modules <- length(module_names)

  # Initialize correlation data frame
  coor <- data.frame(
    exp = character(),
    exp_n = integer(),
    variate = integer(),
    output = character(),
    input = character(),
    value = numeric(),
    method = character(),
    use = character()
  )

  # Process each module
  for (h in seq_along(module_names)) {
    exp_h <- info$module_exp_data$exp[
      info$module_exp_data$module == module_names[h]
    ]

    data_name_h <- info$module_exp_data$data_name[
      info$module_exp_data$module == module_names[h]
    ]

    if (progress) {
      exp_label <- paste(exp_h, collapse = ", ")
      cat(sprintf(
        "\n[Correlation analysis] Expression %s (%d/%d)\n",
        exp_label,
        h,
        total_modules
      ))
    }
    # Get input (type == "in_node") mcnodes names in mcmodule$node_list for this expression (module == exp_h) or from sample design (from_sample_design) if match_variates = TRUE
    # Only include nodes that have more than 1 uncertainty simulation (nrow > 1) or more than 1 variate (if variates_as_nsv = TRUE)
    exp_h_inputs <- names(mcmodule$node_list)[
      unlist(lapply(names(mcmodule$node_list), function(x) {
        mcnode_x <- mcmodule$node_list[[x]][["mcnode"]]
        (((mcmodule$node_list[[x]][["exp_name"]] %in%
          exp_h &&
          (mcmodule$node_list[[x]][["type"]] == "in_node")) ||
          (all(mcmodule$node_list[[x]][["data_name"]] == data_name_h) &&
            (mcmodule$node_list[[x]][["type"]] == "in_node")))) &&
          (dim(mcnode_x)[1] > 1 || (variates_as_nsv && dim(mcnode_x)[3] > 1))
      }))
    ]
    exp_h_inputs <- exp_h_inputs[exp_h_inputs %in% names(mcmodule$node_list)]
    if (!is.null(mc_names)) {
      exp_h_inputs <- exp_h_inputs[exp_h_inputs %in% mc_names]
    }

    if (is.null(output)) {
      if (by_exp) {
        exp_h_outputs <- names(mcmodule$node_list)[
          unlist(lapply(names(mcmodule$node_list), function(x) {
            mcmodule$node_list[[x]][["exp_name"]] %in%
              exp_h &&
              mcmodule$node_list[[x]][["type"]] == "out_node"
          }))
        ]
        output_h <- exp_h_outputs[length(exp_h_outputs)]
      } else {
        output_h <- names(mcmodule$node_list)[length(mcmodule$node_list)]
      }
    } else {
      output_h <- output
    }

    # Get output
    mc_output <- mcmodule$node_list[[output_h]][["mcnode"]]
    summary_output <- mcmodule$node_list[[output_h]][["summary"]]

    data_name_h <- unique(info$module_exp_data$data_name[
      info$module_exp_data$exp == exp_h
    ])

    if (!is.null(data_name_h) && !is.na(data_name_h)) {
      suppressMessages({
        mc_match_data_h <- mc_match_data(
          mcmodule,
          output_h,
          mcmodule$data[[data_name_h]]
        )
      })
      mc_output_h <- mc_match_data_h[[1]]
      data_h <- mc_match_data_h[[2]]
      keys_h <- mc_match_data_h[[3]]

      # Check data and keys are compatible
      if (
        !all(
          keys_h[intersect(names(keys_h), names(data_h))] ==
            data_h[intersect(names(keys_h), names(data_h))]
        )
      ) {
        stop(paste0(
          "Data and keys are not compatible for expression '",
          exp_h,
          "'"
        ))
      }

      # Create a copy of mcmodule to modify
      mcmodule_h <- mcmodule

      # Match input mcnodes to output mcnode if match_variates is TRUE and data dimensions differ
      if (
        match_variates &&
          (!all(
            dim(data_h) == dim(mcmodule_h$data[[data_name_h]]),
            na.rm = TRUE
          ) ||
            !all(data_h == mcmodule_h$data[[data_name_h]], na.rm = TRUE))
      ) {
        for (input_name in exp_h_inputs) {
          mc_input <- mcmodule_h$node_list[[input_name]][["mcnode"]]
          suppressMessages({
            mc_input_matched <- mc_match_data(
              mcmodule_h,
              input_name,
              data_h,
              keys_names = intersect(names(data_h), info$global_keys)
            )[[1]]
            mc_input_matched
            mcmodule_h$node_list[[input_name]][["mcnode"]] <- mc_input_matched
          })
        }
      }

      # Temporarily replace output mcnode with matched output
      mcmodule_h$node_list[[output_h]][["mcnode"]] <- mc_output_h
    } else {
      # Create a copy of mcmodule to modify
      mcmodule_h <- mcmodule

      # Check that all nodes only have one variate
      for (input_name in exp_h_inputs) {
        mc_input <- mcmodule_h$node_list[[input_name]][["mcnode"]]
        if (dim(mc_input)[3] > 1) {
          stop(paste0(
            "Input node '",
            input_name,
            "' has more than one variate. Please provide matching data or set match_variates = FALSE."
          ))
        }
      }

      if (dim(mc_output)[3] > 1) {
        stop(paste0(
          "Output node '",
          output_h,
          "' has more than one variate. Please provide matching data or set match_variates = FALSE."
        ))
      }
    }

    # Convert mcmodule to mc object with only inputs and output
    mc_h <- mcmodule_to_mc(
      mcmodule = mcmodule_h,
      mc_names = c(exp_h_inputs, output_h),
      variates_as_nsv = variates_as_nsv
    )

    # Wrap in list if variates_as_nsv = TRUE for consistent iteration
    if (variates_as_nsv && inherits(mc_h, "mc")) {
      mc_h <- list(mc_h)
    }
    warnings <- c()
    # Calculate correlation for this expression and variate
    for (i in seq_along(mc_h)) {
      tornado_result <- local({
        warnings_h_i <- character()

        tornado_h_i <- tryCatch(
          withCallingHandlers(
            tornado(mc_h[[i]], output = output_h, method = method),
            warning = function(w) {
              warnings_h_i <<- c(warnings_h_i, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          ),
          error = function(e) {
            warnings_h_i <<- c(
              warnings_h_i,
              paste("Error:", conditionMessage(e))
            )
            NULL
          }
        )

        list(tornado = tornado_h_i, warnings = warnings_h_i)
      })

      tornado_h_i <- tornado_result$tornado

      if (is.null(tornado_h_i)) {
        names_h_i <- exp_h_inputs
        values_h_i <- NA
      } else {
        names_h_i <- c(colnames(tornado_h_i[[1]][[1]]))
        values_h_i <- unlist(tornado_h_i[1])
      }

      coor_h_i <- data.frame(
        input = names_h_i,
        value = values_h_i,
        output = output_h,
        method = tornado_h_i$method,
        use = tornado_h_i$use,
        row.names = NULL
      )

      coor_h_i$variate <- i
      coor_h_i$exp <- paste(exp_h, collapse = ", ")
      coor_h_i$exp_n <- h
      coor_h_i$module <- module_names[h]
      if (!is.null(data_name_h) && !is.na(data_name_h)) {
        coor_h_i[intersect(names(data_h), info$global_keys)] <- data_h[
          i,
          intersect(names(data_h), info$global_keys)
        ]
      }
      if (length(tornado_result$warnings) > 0) {
        warnings <- c(warnings, tornado_result$warnings)
      }

      coor <- dplyr::bind_rows(coor, coor_h_i)
    }
  }

  # Add correlation strength classification
  coor$strength <- sapply(coor$value, function(r) {
    abs_r <- abs(r)
    if (is.na(abs_r)) {
      return(NA_character_)
    } else if (abs_r >= 0.8) {
      return("Very strong")
    } else if (abs_r >= 0.6) {
      return("Strong")
    } else if (abs_r >= 0.4) {
      return("Moderate")
    } else if (abs_r >= 0.2) {
      return("Weak")
    } else {
      return("Very weak/None")
    }
  })

  coor$strength <- ordered(
    coor$strength,
    levels = c("Very weak/None", "Weak", "Moderate", "Strong", "Very strong")
  )

  if (print_summary) {
    # Print correlation analysis summary
    cat("\n=== Correlation Analysis Summary ===\n")

    # Analysis parameters
    cat("\nAnalysis Parameters:")
    if (by_exp) {
      cat("\n- Analysis type: By expression")
      cat("\n- Output nodes per expression:")
      for (exp_name in unique(coor$exp)) {
        exp_outputs <- unique(coor$output[coor$exp == exp_name])
        cat(
          "\n  - ",
          exp_name,
          ": ",
          paste(exp_outputs, collapse = ", "),
          sep = ""
        )
      }
    } else {
      cat("\n- Analysis type: Global output")
      cat("\n- Output node:", output_h)
    }
    cat(
      "\n- Correlation method(s):",
      paste(unique(coor$method), collapse = ", ")
    )
    cat("\n- Missing value handling:", unique(coor$use))

    # Expression information
    cat("\n\nExpression Information:")
    for (mod_name in unique(coor$module)) {
      mod_exps <- unique(coor$exp[coor$module == mod_name])
      cat("\n- Module: ", mod_name, sep = "")
      for (exp_name in mod_exps) {
        exp_inputs <- unique(coor$input[coor$exp == exp_name])
        exp_variates <- length(unique(coor$variate[coor$exp == exp_name]))
        cat("\n  - Expression: ", exp_name, sep = "")
        cat(
          "\n    - Input nodes: ",
          paste(exp_inputs, collapse = ", "),
          sep = ""
        )
        cat("\n    - Variates analyzed: ", exp_variates, sep = "")
      }
    }

    # Results summary
    cat("\n\nResults Summary:")
    cat("\n- Total correlations calculated:", nrow(coor))

    # Correlation value statistics
    if (!all(is.na(coor$value))) {
      # Top n correlated inputs
      top_n <- 5
      mean_cors <- stats::aggregate(
        coor$value,
        list(input = coor$input),
        mean,
        na.rm = TRUE
      )
      mean_cors$abs_value <- abs(mean_cors$x)
      mean_cors <- mean_cors[order(mean_cors$abs_value, decreasing = TRUE), ]
      mean_cors <- utils::head(mean_cors, top_n)

      cat(
        "\n- Top ",
        min(top_n, nrow(mean_cors)),
        " most influential inputs (by absolute mean correlation):",
        sep = ""
      )
      for (i in seq_len(nrow(mean_cors))) {
        cat(
          "\n  ",
          i,
          ". ",
          mean_cors$input[i],
          ": ",
          sprintf("%.4f", mean_cors$x[i]),
          sep = ""
        )
      }

      # Classify inputs by correlation strength using the new classification
      cat("\n\nInput Correlation Strength Distribution:")
      strength_counts <- table(coor$strength)
      strength_order <- c(
        "Very strong",
        "Strong",
        "Moderate",
        "Weak",
        "Very weak/None"
      )
      for (s in strength_order) {
        if (s %in% names(strength_counts)) {
          pct_strength <- strength_counts[s] / nrow(coor) * 100
          cat(sprintf(
            "\n- %s: %d (%.1f%%)",
            s,
            strength_counts[s],
            pct_strength
          ))
        }
      }

      # Show inputs by strength category
      cat("\n\nInputs by Correlation Strength:")
      for (s in strength_order) {
        inputs_in_strength <- unique(coor$input[
          coor$strength == s & !is.na(coor$strength)
        ])
        if (length(inputs_in_strength) > 0) {
          cat(sprintf(
            "\n- %s: %s",
            s,
            paste(inputs_in_strength, collapse = ", ")
          ))
        }
      }
    }

    # Warning summary (only if warnings exist)
    if (length(warnings) > 0) {
      n_warnings <- length(warnings)
      if (n_warnings > 0) {
        cat("\n\nWarnings and Errors:")
        cat(
          "\n- Number of correlations with warnings:",
          n_warnings,
          sprintf("(%.2f%%)", n_warnings / nrow(coor) * 100)
        )

        cat("\n- Unique warning/error types:")
        unique_warnings <- unique(warnings)
        for (i in seq_along(unique_warnings)) {
          cat(
            "\n  ",
            i,
            ". ",
            substr(unique_warnings[i], 1, 80),
            if (nchar(unique_warnings[i]) > 80) "..." else "",
            sep = ""
          )
        }
      }
    }

    cat("\n")
  }

  if (isTRUE(plot)) {
    plot(mcmodule_tornado(corr_results = coor))
  }

  coor
}


#' Analyse Monte Carlo Simulation Convergence
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Analyses convergence in Monte Carlo simulations by computing standardised
#' and raw differences between consecutive iterations to evaluate stability and
#' convergence of statistical measures.
#'
#' @param mcmodule (mcmodule object). Module containing simulation results.
#' @param from_quantile (numeric). Lower bound quantile for analysis. Default: 0.95.
#' @param to_quantile (numeric). Upper bound quantile for analysis. Default: 1.
#' @param conv_threshold (numeric, optional). Custom convergence threshold for
#'   standardised differences. Default: NULL.
#' @param tiny_threshold (numeric). Threshold for identifying negligible differences, even if they don't meet the convergence threshold. Default: 0.0001.
#' @param print_summary (logical). If TRUE, print convergence analysis summary.
#'   Default: TRUE.
#' @param progress (logical). If TRUE, print progress information. Default: FALSE.
#' @param mc_names (character vector, optional). Node names to include in analysis.
#'   If NULL (default), includes all nodes in the module. Default: NULL.
#'
#' @return A data frame with convergence statistics. Each row represents one node.
#'   Key columns:
#'   \itemize{
#'     \item expression: Expression identifier.
#'     \item variate: Variate (data row) identifier.
#'     \item node: Node name.
#'     \item max_dif_scaled: Maximum standardised difference.
#'     \item max_dif: Maximum raw difference.
#'     \item conv_01, conv_025, conv_05: Convergence at 1%, 2.5%, 5% thresholds.
#'   }
#'
#' @details
#' The function performs the following:
#' \itemize{
#'   \item Calculates convergence statistics for specified quantile range
#'   \item Generates diagnostic plots for standardized and raw differences
#'   \item Provides detailed convergence summary including:
#'     \itemize{
#'       \item Total nodes analyzed
#'       \item Number and percentage of nodes converged at different thresholds
#'       \item Maximum/minimum deviations
#'       \item List of non-converged nodes (if any)
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' results <- mcmodule_converg(mc_results)
#' results <- mcmodule_converg(mc_results, from_quantile = 0.90, conv_threshold = 0.01)
#' }
#'
#' @export
mcmodule_converg <- function(
  mcmodule,
  from_quantile = 0.95,
  to_quantile = 1,
  conv_threshold = NULL,
  tiny_threshold = NULL,
  print_summary = TRUE,
  progress = FALSE,
  mc_names = NULL
) {
  # Helper function to calculate statistics (mean and quantiles) for convergence analysis
  mc_stat <- function(i, x) {
    c(
      mean = mean(x[1:i], na.rm = TRUE),
      stats::quantile(
        x[1:i],
        probs = c(0.5, 0.025, 0.975),
        na.rm = TRUE,
        names = FALSE
      )
    )
  }

  # Get mcmodule index information
  info <- mcmodule_info(mcmodule)

  if (info$n_modules == 1) {
    info$module_exp_data$module <- deparse(substitute(mcmodule))
  }

  module_exp_names <- paste(
    info$module_exp_data$module,
    info$module_exp_data$exp
  )

  # Initialize list to store convergence results
  mc_convergence_list <- list()
  list_index <- 1

  # Iterate through each module (expression group) in the Monte Carlo module
  for (h in seq_along(module_exp_names)) {
    module_exp_h <- module_exp_names[h]
    module_h <- info$module_exp_data$module[h]
    exp_h <- info$module_exp_data$exp[h]

    # Get all nodes for this expression
    exp_h_nodes <- names(mcmodule$node_list)[
      unlist(lapply(names(mcmodule$node_list), function(x) {
        mcmodule$node_list[[x]][["exp_name"]] %in% exp_h
      }))
    ]
    if (!is.null(mc_names)) {
      exp_h_nodes <- exp_h_nodes[exp_h_nodes %in% mc_names]
    }

    if (progress) {
      module_exp_label <- paste(module_exp_h, collapse = ", ")
      cat(sprintf(
        "\n[Convergence analysis] Module: '%s' Expression: '%s' (%d/%d)\n",
        module_h,
        exp_h,
        h,
        length(module_exp_names)
      ))
    }

    # Convert mcmodule to mc objects for this expression
    # mc_list <- mcmodule_to_mc(mcmodule, mc_names = exp_h_nodes)

    # Process each node
    for (j in seq_along(exp_h_nodes)) {
      node_name_j <- exp_h_nodes[j]
      mcnode_j <- mcmodule$node_list[[node_name_j]][["mcnode"]]

      # Analyze convergence for variate
      for (k in 1:dim(mcnode_j)[[3]]) {
        variate_k <- k
        x <- extractvar(mcnode_j, k)

        # Only analyze nodes with more than one iteration and that have variability/uncertainty
        if (dim(x)[1] > 1 & !max(x) == min(x)) {
          # Calculate convergence statistics for the specified quantile range
          conv_start <- floor(dim(x)[1] * from_quantile)
          conv_end <- floor(dim(x)[1] * to_quantile)
          n_sim <- dim(x)[1]
          n_sim_conv <- conv_end - conv_start

          # Stop if insufficient iterations for convergence analysis at the specified quantiles
          if (n_sim_conv < 3) {
            stop(paste0(
              "Node '",
              node_name_j,
              "' variate ",
              variate_k,
              ": Only ",
              n_sim_conv,
              " iterations available for convergence analysis between quantiles ",
              from_quantile,
              " and ",
              to_quantile,
              ". Please adjust quantiles or ensure sufficient iterations."
            ))
          }

          x_conv <- vapply(
            conv_start:conv_end,
            mc_stat,
            x = x,
            FUN.VALUE = numeric(4)
          )

          # Calculate differences between iterations
          x_conv_dif <- x_conv - cbind(0, x_conv[, 1:(ncol(x_conv) - 1)])
          x_conv_dif <- x_conv_dif[, -1]

          # Calculate convergence metrics
          max_dif <- max(x_conv_dif)
          mean_value <- mean(x_conv[1, ])
          max_dif_scaled <- max_dif / mean_value

          max_dif_mean <- max(abs(x_conv_dif[1, ]))
          max_dif_median <- max(abs(x_conv_dif[2, ]))
          max_dif_q025 <- max(abs(x_conv_dif[3, ]))
          max_dif_q975 <- max(abs(x_conv_dif[4, ]))

          mean_stat_mean <- mean(x_conv[1, ])
          mean_stat_median <- mean(x_conv[2, ])
          mean_stat_q025 <- mean(x_conv[3, ])
          mean_stat_q975 <- mean(x_conv[4, ])

          max_dif_mean_scaled <- ifelse(
            mean_stat_mean == 0,
            NA,
            max_dif_mean / mean_stat_mean
          )
          max_dif_median_scaled <- ifelse(
            mean_stat_median == 0,
            NA,
            max_dif_median / mean_stat_median
          )
          max_dif_q025_scaled <- ifelse(
            mean_stat_q025 == 0,
            NA,
            max_dif_q025 / mean_stat_q025
          )
          max_dif_q975_scaled <- ifelse(
            mean_stat_q975 == 0,
            NA,
            max_dif_q975 / mean_stat_q975
          )

          conv_01 <- abs(max_dif_scaled) < 0.01
          conv_025 <- abs(max_dif_scaled) < 0.025
          conv_05 <- abs(max_dif_scaled) < 0.05

          if (!is.null(tiny_threshold)) {
            tiny <- abs(max_dif) < tiny_threshold
            conv_01_tiny <- conv_01 | tiny
            conv_025_tiny <- conv_025 | tiny
            conv_05_tiny <- conv_05 | tiny
          }

          if (!is.null(conv_threshold)) {
            conv_manual <- abs(max_dif_scaled) < conv_threshold
            if (!is.null(tiny_threshold)) {
              conv_manual_tiny <- conv_manual | tiny
            }
          }

          if (!is.na(max_dif) && !is.na(max_dif_scaled)) {
            mc_convergence_list[[list_index]] <- data.frame(
              module = module_h,
              expression = exp_h,
              variate = variate_k,
              mcnode = node_name_j,
              mean_value = mean_value,
              max_dif = max_dif,
              max_dif_mean = max_dif_mean,
              max_dif_median = max_dif_median,
              max_dif_q025 = max_dif_q025,
              max_dif_q975 = max_dif_q975,
              max_dif_scaled = max_dif_scaled,
              max_dif_mean_scaled = max_dif_mean_scaled,
              max_dif_median_scaled = max_dif_median_scaled,
              max_dif_q025_scaled = max_dif_q025_scaled,
              max_dif_q975_scaled = max_dif_q975_scaled,
              conv_01 = conv_01,
              conv_025 = conv_025,
              conv_05 = conv_05
            )

            if (!is.null(conv_threshold)) {
              mc_convergence_list[[list_index]]$conv_manual <- conv_manual
              if (!is.null(tiny_threshold)) {
                mc_convergence_list[[
                  list_index
                ]]$conv_manual_tiny <- conv_manual_tiny
              }
            }

            if (!is.null(tiny_threshold)) {
              mc_convergence_list[[list_index]]$tiny <- tiny
              mc_convergence_list[[
                list_index
              ]]$conv_01_tiny <- conv_01_tiny
              mc_convergence_list[[
                list_index
              ]]$conv_025_tiny <- conv_025_tiny
              mc_convergence_list[[
                list_index
              ]]$conv_05_tiny <- conv_05_tiny
            }

            list_index <- list_index + 1
          }
        }
      }
    }
  }

  # Combine all results and return
  conv_df <- do.call(rbind, mc_convergence_list[1:(list_index - 1)])

  # Prepare data for plotting
  # Find the point with the highest max_dif_scaled per input
  max_points <- conv_df[order(conv_df$mcnode, -conv_df$max_dif_scaled), ]
  max_points <- max_points[!duplicated(max_points$mcnode), ]

  # Determine threshold for labeling
  label_threshold <- if (!is.null(conv_threshold)) conv_threshold else 0.025

  # Create label text for highest points - only for nodes diverging > threshold
  max_points$label <- ifelse(
    abs(max_points$max_dif_scaled) > label_threshold,
    sprintf("%.6f", max_points$max_dif),
    ""
  )

  # Convert max_dif_scaled to percentage for plotting
  conv_df$max_dif_scaled_pct <- conv_df$max_dif_scaled * 100
  max_points$max_dif_scaled_pct <- max_points$max_dif_scaled * 100

  # Calculate mean max_dif_scaled per node for ordering
  node_order <- stats::aggregate(
    conv_df$max_dif_scaled,
    list(node = conv_df$mcnode),
    max
  )
  node_order <- node_order[order(-node_order$x), ]

  # Convert node to factor with levels ordered by max difference
  conv_df$mcnode <- factor(conv_df$mcnode, levels = node_order$node)
  max_points$mcnode <- factor(max_points$mcnode, levels = node_order$node)

  total_nodes <- length(unique(conv_df$mcnode))

  if (print_summary) {
    # Print analysis results summary
    cat("\n=== Convergence Analysis Summary ===\n")
    cat("\nAnalysis Parameters")
    cat("\n- Number of simulations:", n_sim)
    cat("\n- Total nodes analyzed:", total_nodes)
    cat("\n- Total variates analyzed:", nrow(conv_df))
    cat("\n- Simulation quantile range:", from_quantile, "to", to_quantile)
    cat(
      "\n- Simulations range:",
      conv_start,
      "to",
      conv_end,
      paste0("(", n_sim_conv, " simulations)")
    )
    if (!is.null(conv_threshold)) {
      cat("\n- Custom convergence threshold:", conv_threshold)
    }
    if (!is.null(tiny_threshold)) {
      cat("\n- Tiny difference threshold:", tiny_threshold)
    }

    # Calculate convergence statistics
    pct <- function(x) {
      if (x == 0) {
        "0%"
      } else {
        sprintf("%.2f%%", x)
      }
    }

    cat("\n\nConvergence Results")
    cat(
      "\nMaximum divergence of node summary statistics (mean, median, 2.5th percentile and 97.5th percentile):"
    )

    format_non_converged <- function(x) {
      x <- unique(as.character(x))
      x <- x[!is.na(x) & nzchar(x)]
      if (length(x) == 0) {
        ""
      } else {
        paste0("\n", paste(x, collapse = ", "))
      }
    }

    if (!is.null(conv_threshold)) {
      diverged_manual <- length(unique(conv_df$mcnode[!conv_df$conv_manual]))
      diverged_manual_names <- conv_df$mcnode[!conv_df$conv_manual]

      cat(sprintf(
        "\n\n- More than %.4f divergence: %d (%s)",
        conv_threshold,
        diverged_manual,
        pct(diverged_manual / total_nodes * 100)
      ))
      cat(sprintf(
        "\n%s",
        conv_threshold,
        format_non_converged(diverged_manual_names)
      ))

      if (!is.null(tiny_threshold)) {
        diverged_manual_tiny <- length(unique(conv_df$mcnode[
          !conv_df$conv_manual_tiny
        ]))

        diverged_manual_tiny_names <- conv_df$mcnode[
          !conv_df$conv_manual_tiny
        ]

        cat(sprintf(
          "\n\n- More than %.4f divergence (over %.4f): %d (%s)",
          conv_threshold,
          tiny_threshold,
          diverged_manual_tiny,
          pct(diverged_manual_tiny / total_nodes * 100)
        ))
        cat(sprintf(
          "\n%s",
          conv_threshold,
          format_non_converged(diverged_manual_tiny_names)
        ))
      }
    }

    diverged_01 <- length(unique(conv_df$mcnode[!conv_df$conv_01]))
    diverged_025 <- length(unique(conv_df$mcnode[!conv_df$conv_025]))
    diverged_05 <- length(unique(conv_df$mcnode[!conv_df$conv_05]))

    cat(sprintf(
      "\n\n- More than 1%% divergence: %d (%s)",
      diverged_01,
      pct(diverged_01 / total_nodes * 100)
    ))
    diverged_names_01 <- conv_df$mcnode[
      !conv_df$conv_01 & !is.na(conv_df$conv_01)
    ]
    cat(format_non_converged(diverged_names_01))

    cat(sprintf(
      "\n\n- More than 2.5%% divergence: %d (%s)",
      diverged_025,
      pct(diverged_025 / total_nodes * 100)
    ))
    diverged_names_025 <- conv_df$mcnode[
      !conv_df$conv_025 & !is.na(conv_df$conv_025)
    ]
    cat(format_non_converged(diverged_names_025))

    cat(sprintf(
      "\n\n- More than 5%% divergence: %d (%s)",
      diverged_05,
      pct(diverged_05 / total_nodes * 100)
    ))
    diverged_names_05 <- conv_df$mcnode[
      !conv_df$conv_05 & !is.na(conv_df$conv_05)
    ]
    cat(format_non_converged(diverged_names_05))

    if (!is.null(tiny_threshold)) {
      diverged_05_tiny <- length(unique(conv_df$mcnode[
        !conv_df$conv_05_tiny
      ]))

      diverged_05_tiny_names <- conv_df$mcnode[
        !conv_df$conv_05_tiny & !is.na(conv_df$conv_05_tiny)
      ]

      cat(sprintf(
        "\n\n- More than 5%% divergence (over %.4f): %d (%s)",
        tiny_threshold,
        diverged_05_tiny,
        pct(diverged_05_tiny / total_nodes * 100)
      ))

      cat(format_non_converged(diverged_05_tiny_names))
    }

    # Happy message if all converged at 5% threshold
    if (diverged_01 == 0) {
      cat("\n\nAll nodes successfully converged at 1%% threshold! :D\n")
    } else if (diverged_025 == 0) {
      cat("\n\nAll nodes successfully converged at 2.5%% threshold! :)\n")
    } else if (diverged_05 == 0) {
      cat("\n\nAll nodes successfully converged at 5%% threshold! :)\n")
    } else {
      cat(sprintf(
        "\n\n%d (%s) nodes did not converge at 5%% threshold :(",
        diverged_05,
        pct((diverged_05 / total_nodes) * 100)
      ))
    }
  }

  return(conv_df)
}

#' Optimize Number of Variability Iterations Based on Convergence
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Automatically determines the minimum number of variability iterations (ndvar) required
#' for all input nodes in a Monte Carlo model to converge at the 5% threshold. Uses
#' an iterative algorithm starting with 1,001 variates and adjusting up or down
#' based on observed convergence.
#'
#' @param mctable (data frame). Table with `mcnode` and `sample_space`
#'   columns defining the sampling distribution for each input. Default: `set_mctable()`.
#' @param exp (language or list). Optional model expression(s) to evaluate. Default: NULL, will create an expression multipling all input nodes.
#' @param mc_names (character vector, optional). Specific node names to analyze.
#'   If NULL, analyzes all nodes. Default: NULL.
#' @param min_ndvar (integer). Minimum allowed ndvar. Default: 100.
#' @param max_ndvar (integer). Maximum allowed ndvar. Default: 50000.
#' @param start_ndvar (integer). Initial ndvar to test. Default: 1001.
#' @param conv_threshold (numeric). Convergence threshold at 5%. Default: 0.05.
#' @param print_summary (logical). If TRUE, print optimization summary. Default: TRUE.
#' @param progress (logical). If TRUE, print progress for each iteration. Default: FALSE.
#'
#' @return A list containing:
#'   \itemize{
#'     \item `optimal_ndvar`: The minimum ndvar where all nodes converge.
#'     \item `converged`: Logical indicating if convergence was achieved.
#'     \item `iterations`: Data frame with each iteration's details (ndvar, converged, reason).
#'     \item `convergence_results`: Convergence analysis results from [mcmodule_converg()].
#'   }
#'
#' @details
#' The optimization algorithm:
#' - Starts with `start_ndvar` (default 1,001)
#' - If convergence achieved: tries n/2 (lower bound search)
#' - If convergence not achieved: tries 2n (upper bound search)
#' - Continues until minimum converging ndvar is found
#' - Warns if limits (min_ndvar, max_ndvar) are reached
#'
#' @export
#'
#' @examples
#' # Define mctable
#' mctable <- data.frame(
#'   mcnode = c("input_a", "input_b"),
#'   sample_space = c("min = 0, max = 1", "min = 10, max = 20")
#' )
#'
#' # Optimize ndvar
#' result <- optim_ndvar(
#'   exp = quote({result <- input_a * input_b}),
#'   mctable = mctable
#' )
#'
#' result$optimal_ndvar
#'
optim_ndvar <- function(
  mctable = set_mctable(),
  exp = NULL,
  mc_names = NULL,
  min_ndvar = 100,
  max_ndvar = 50000,
  start_ndvar = 1001,
  conv_threshold = 0.05,
  print_summary = TRUE,
  progress = FALSE
) {
  # Input validation
  if (!is.data.frame(mctable)) {
    stop("mctable must be a data frame")
  }

  if (!all(c("mcnode", "sample_space") %in% names(mctable))) {
    stop(
      "mctable must contain columns 'mcnode' and 'sample_space'"
    )
  }

  if (min_ndvar < 1) {
    stop("min_ndvar must be >= 1")
  }

  if (max_ndvar <= min_ndvar) {
    stop("max_ndvar must be > min_ndvar")
  }

  if (start_ndvar < min_ndvar || start_ndvar > max_ndvar) {
    warning(
      sprintf(
        "start_ndvar (%d) is outside [min_ndvar, max_ndvar] range [%d, %d]. Using min_ndvar.",
        start_ndvar,
        min_ndvar,
        max_ndvar
      )
    )
    start_ndvar <- min_ndvar
  }

  if (is.null(exp)) {
    # Create a expression with all the input nodes, since we only care about convergence of input nodes
    rhs <- Reduce(
      function(a, b) call("*", a, b),
      lapply(as.character(mctable$mcnode), as.symbol)
    )

    exp <- as.call(list(
      as.symbol("{"),
      as.call(list(as.symbol("<-"), as.symbol("output"), rhs))
    ))
  }

  # Helper function to generate sample design from sample_space
  generate_from_sample_space <- function(mctable_input, n) {
    # Extract only mcnodes with valid sample_space
    valid_rows <- !is.na(mctable_input$sample_space) &
      nzchar(trimws(mctable_input$sample_space))
    mctable_valid <- mctable_input[valid_rows, ]

    if (nrow(mctable_valid) == 0) {
      stop("No valid sample_space entries found in mctable")
    }

    # Generate random samples for each node
    sample_design_list <- list()

    for (i in seq_len(nrow(mctable_valid))) {
      mcnode_name <- mctable_valid$mcnode[i]
      sample_space <- mctable_valid$sample_space[i]

      # Parse sample space and generate samples
      # Support formats: "c(min, max)", "min = X, max = Y"
      tryCatch(
        {
          # Extract numeric bounds
          if (grepl("^c\\(", sample_space)) {
            # Format: c(min, max)
            bounds_str <- sub("^c\\((.*)\\)$", "\\1", sample_space)
            bounds <- as.numeric(unlist(strsplit(bounds_str, ",")))
            if (length(bounds) >= 2) {
              sample_design_list[[mcnode_name]] <- stats::runif(
                n,
                bounds[1],
                bounds[2]
              )
            }
          } else if (grepl("min\\s*=|max\\s*=", sample_space)) {
            # Format: min = X, max = Y
            min_match <- regmatches(
              sample_space,
              regexec("min\\s*=\\s*([^,]+)", sample_space)
            )
            max_match <- regmatches(
              sample_space,
              regexec("max\\s*=\\s*([^,]+|$)", sample_space)
            )

            min_val <- if (length(min_match[[1]]) > 1) {
              as.numeric(trimws(min_match[[1]][2]))
            } else {
              0
            }

            max_val <- if (length(max_match[[1]]) > 1) {
              as.numeric(trimws(max_match[[1]][2]))
            } else {
              1
            }

            if (is.finite(min_val) && is.finite(max_val)) {
              sample_design_list[[mcnode_name]] <- stats::runif(n, min_val, max_val)
            }
          }
        },
        error = function(e) {
          warning(
            sprintf(
              "Could not parse sample_space for '%s': %s. Skipping.",
              mcnode_name,
              e$message
            )
          )
        }
      )
    }

    if (length(sample_design_list) == 0) {
      stop("Failed to generate sample design from sample_space_mctable")
    }

    # Convert list to data frame
    as.data.frame(
      sample_design_list,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  # Helper function to run convergence check
  check_convergence <- function(ndvar) {
    # Create sample design matrix with given ndvar
    sample_design <- generate_from_sample_space(
      mctable,
      n = ndvar
    )

    # Evaluate module
    mcmodule <- eval_module(
      exp = exp,
      mctable = mctable,
      sample_design = sample_design,
      summary = FALSE
    )

    # Run convergence analysis (suppress print)
    conv_result <- mcmodule_converg(
      mcmodule = mcmodule,
      from_quantile = 0.95,
      to_quantile = 1,
      mc_names = mc_names,
      print_summary = FALSE,
      progress = FALSE
    )

    # Check if all nodes converged at 5% threshold
    all_converged <- all(conv_result$conv_05, na.rm = TRUE)

    list(
      all_converged = all_converged,
      mcmodule = mcmodule,
      conv_result = conv_result
    )
  }

  # Initialize tracking
  iterations_list <- list()

  optimal_ndvar <- NA_integer_
  is_converged <- FALSE
  current_ndvar <- start_ndvar
  iteration_count <- 0
  last_converged_ndvar <- NA_integer_
  last_non_converged_ndvar <- NA_integer_
  convergence_results <- NULL

  # Optimization loop
  while (iteration_count < 100) {
    # Safety limit on iterations
    iteration_count <- iteration_count + 1

    if (progress) {
      cat(sprintf(
        "\n[Iteration %d] Testing ndvar = %d...",
        iteration_count,
        current_ndvar
      ))
    }

    # Check convergence at current ndvar
    error_occurred <- FALSE
    result <- tryCatch(
      {
        check_convergence(current_ndvar)
      },
      error = function(e) {
        error_occurred <<- TRUE
        warning(
          sprintf(
            "Error during convergence check at ndvar=%d: %s",
            current_ndvar,
            e$message
          )
        )
        NULL
      }
    )

    if (!error_occurred && !is.null(result)) {
      is_converged_current <- result$all_converged
      convergence_results <- result$conv_result

      if (progress) {
        cat(sprintf(
          " %s\n",
          if (is_converged_current) "CONVERGED" else "NOT CONVERGED"
        ))
      }

      # Record iteration
      reason <- if (iteration_count == 1) {
        "Initial"
      } else if (is_converged_current) {
        if (!is.na(last_converged_ndvar)) {
          "Converged, trying n/2"
        } else {
          "First convergence found"
        }
      } else {
        "Not converged, trying 2n"
      }

      iterations_list[[iteration_count]] <- list(
        iteration = iteration_count,
        ndvar = current_ndvar,
        converged = is_converged_current,
        reason = reason
      )

      # Update convergence tracking
      if (is_converged_current) {
        last_converged_ndvar <- current_ndvar
      } else {
        last_non_converged_ndvar <- current_ndvar
      }

      # Determine next ndvar
      if (is_converged_current) {
        # Try n/2 to find minimum
        next_ndvar <- floor(current_ndvar / 2)

        # Check if we've narrowed down to minimum
        if (
          !is.na(last_non_converged_ndvar) &&
            next_ndvar <= last_non_converged_ndvar
        ) {
          # Found optimal: current converges but next would not
          optimal_ndvar <- current_ndvar
          is_converged <- TRUE
          break
        }

        # Check limits
        if (next_ndvar < min_ndvar) {
          optimal_ndvar <- current_ndvar
          is_converged <- TRUE
          break
        }

        current_ndvar <- next_ndvar
      } else {
        # Try 2n to find upper bound
        next_ndvar <- current_ndvar * 2

        # Check limits
        if (next_ndvar > max_ndvar) {
          warning(
            sprintf(
              "Maximum ndvar limit (%d) reached without convergence.",
              max_ndvar
            )
          )
          optimal_ndvar <- max_ndvar
          is_converged <- FALSE
          break
        }

        current_ndvar <- next_ndvar
      }
    }

    # Safety check: if we're cycling, break
    if (iteration_count > 20) {
      optimal_ndvar <- current_ndvar
      warning(
        sprintf(
          "Optimization reached iteration limit. Using ndvar = %d.",
          current_ndvar
        )
      )
      break
    }
  }

  # Convert iterations list to data frame
  if (length(iterations_list) > 0) {
    iterations <- do.call(rbind, lapply(iterations_list, as.data.frame))
  } else {
    iterations <- data.frame(
      iteration = integer(),
      ndvar = integer(),
      converged = logical(),
      reason = character(),
      stringsAsFactors = FALSE
    )
  }

  # Print summary if requested
  if (print_summary) {
    cat("\n=== NDvar Optimization Summary ===\n")
    cat("\nOptimization Parameters:")
    cat("\n- Starting ndvar:", start_ndvar)
    cat("\n- Min ndvar limit:", min_ndvar)
    cat("\n- Max ndvar limit:", max_ndvar)
    cat("\n- Convergence threshold: 5%")

    cat("\n\nOptimization Results:")
    cat("\n- Total iterations:", iteration_count)
    cat("\n- Optimal ndvar found:", optimal_ndvar)
    cat("\n- Status:", if (is_converged) "CONVERGED" else "NOT CONVERGED")

    cat("\n\nIteration History:\n")
    print(iterations)

    if (is_converged) {
      cat(
        sprintf(
          "\nSuccessfully optimized ndvar to %d (all nodes converge at 5%% threshold) :)\n",
          optimal_ndvar
        )
      )
    } else {
      cat(
        "\nCould not find converging ndvar within specified limits :(\n"
      )
    }
  }

  list(
    optimal_ndvar = optimal_ndvar,
    converged = is_converged,
    iterations = iterations,
    convergence_results = convergence_results
  )
}
