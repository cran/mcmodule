#' Check Dimension Compatibility of Monte Carlo Nodes
#'
#' @description
#' `r lifecycle::badge("experimental")`
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
  n_uncertainty <- unique(sapply(mc_names, function(x) {
    dim(mcmodule$node_list[[x]][["mcnode"]])[1]
  }))
  n_uncertainty <- n_uncertainty[n_uncertainty != 1]

  if (length(unique(n_uncertainty)) > 1) {
    stop(
      "All mcnode objects must have the same number of uncertanty simulations or no uncertainty."
    )
  }

  # Check that all mcnodes have 1 or the same number of variate simulations
  n_variate <- unique(sapply(mc_names, function(x) {
    dim(mcmodule$node_list[[x]][["mcnode"]])[3]
  }))
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
  mc_names <- mc_names %||% names(mcmodule$node_list)
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
#'   object; if FALSE, analyse each variate separately. See `mcmodule_to_mc()`.
#'   Default: FALSE.
#' @param print_summary (logical). If TRUE, print correlation analysis summary.
#'   Default: TRUE.
#' @param progress (logical). If TRUE, print progress information while running.
#'   Default: FALSE.
#' @param method (character). Correlation coefficient type: "spearman" (default),
#'   "kendall", or "pearson". See `stats::cor()`. Default: "spearman".
#' @param use (character). Method for handling missing values: "all.obs",
#'   "complete.obs", or "pairwise.complete.obs". See `stats::cor()`.
#'   Default: "all.obs".
#' @param lim (numeric vector). Quantiles for credible interval computation (reserved
#'   for two-dimensional models). Default: `c(0.025, 0.975)`.
#' @return A data frame with correlation coefficients and metadata. Columns include:
#'   \itemize{
#'     \item exp: Expression name
#'     \item exp_n: Expression number
#'     \item variate: Variate number
#'     \item output: Output node name
#'     \item input: Input node name
#'     \item value: Correlation coefficient value
#'     \item strength: Qualitative strength of association (Very strong, Strong, Moderate, Weak, None)
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
#'   mc_name = "no_detect_a",
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
  print_summary = TRUE,
  progress = FALSE,
  method = c("spearman", "kendall", "pearson"),
  use = "all.obs",
  lim = c(0.025, 0.975)
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

    if (progress) {
      exp_label <- paste(exp_h, collapse = ", ")
      cat(sprintf(
        "\n[Correlation analysis] Expression %s (%d/%d)\n",
        exp_label,
        h,
        total_modules
      ))
    }
    # Get input (type == "in_node") mcnodes names in mcmodule$node_list for this expression (module == exp_h)
    # Only include nodes that have more than 1 uncertainty simulation (nrow > 1) or more than 1 variate (if variates_as_nsv = TRUE)
    exp_h_inputs <- names(mcmodule$node_list)[
      unlist(lapply(names(mcmodule$node_list), function(x) {
        mcnode_x <- mcmodule$node_list[[x]][["mcnode"]]
        mcmodule$node_list[[x]][["exp_name"]] %in%
          exp_h &&
          mcmodule$node_list[[x]][["type"]] == "in_node" &&
          (dim(mcnode_x)[1] > 1 || (variates_as_nsv && dim(mcnode_x)[3] > 1))
      }))
    ]

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

    # Calculate correlation for this expression and variate
    for (i in seq_along(mc_h)) {
      tornado_result <- local({
        warnings <- character()

        tornado_h_i <- tryCatch(
          withCallingHandlers(
            tornado(mc_h[[i]], output = output_h, method = method),
            warning = function(w) {
              warnings <<- c(warnings, conditionMessage(w))
              invokeRestart("muffleWarning")
            }
          ),
          error = function(e) {
            warnings <<- c(warnings, paste("Error:", conditionMessage(e)))
            NULL
          }
        )

        list(tornado = tornado_h_i, warnings = warnings)
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
      coor_h_i[intersect(names(data_h), info$global_keys)] <- data_h[
        i,
        intersect(names(data_h), info$global_keys)
      ]

      if (length(tornado_result$warnings) > 0) {
        coor_h_i$warnings <- paste(tornado_result$warnings, collapse = "; ")
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
      return("None")
    }
  })

  #Move warnings column to the end if it exists base R
  if ("warnings" %in% names(coor)) {
    coor <- coor[, c(setdiff(names(coor), "warnings"), "warnings")]
  }

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
      strength_order <- c("Very strong", "Strong", "Moderate", "Weak", "None")
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
    if ("warnings" %in% names(coor)) {
      n_warnings <- sum(!is.na(coor$warnings))
      if (n_warnings > 0) {
        cat("\n\nWarnings and Errors:")
        cat(
          "\n- Number of correlations with warnings:",
          n_warnings,
          sprintf("(%.2f%%)", n_warnings / nrow(coor) * 100)
        )

        # Get unique inputs with warnings
        inputs_with_warnings <- unique(coor$input[!is.na(coor$warnings)])
        cat(
          "\n- Input nodes with warnings: ",
          paste(inputs_with_warnings, collapse = ", "),
          sep = ""
        )

        cat("\n- Unique warning/error types:")
        unique_warnings <- unique(coor$warnings[!is.na(coor$warnings)])
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

  coor
}


#' Analyse Monte Carlo Simulation Convergence
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' Analyses convergence in Monte Carlo simulations by computing standardised
#' and raw differences between consecutive iterations to evaluate stability and
#' convergence of statistical measures.
#'
#' @param mcmodule (mcmodule object). Module containing simulation results.
#' @param from_quantile (numeric). Lower bound quantile for analysis. Default: 0.95.
#' @param to_quantile (numeric). Upper bound quantile for analysis. Default: 1.
#' @param conv_threshold (numeric, optional). Custom convergence threshold for
#'   standardised differences. Default: NULL.
#' @param print_summary (logical). If TRUE, print convergence analysis summary.
#'   Default: TRUE.
#' @param progress (logical). If TRUE, print progress information. Default: FALSE.
#'
#' @return A data frame with convergence statistics. Each row represents one node.
#'   Key columns:
#'   \itemize{
#'     \item expression: Expression identifier.
#'     \item variate: Variate (data row) identifier.
#'     \item node: Node name.
#'     \item max_dif_scaled: Maximum standardised difference.
#'     \item max_dif: Maximum raw difference.
#'     \item conv_threshold: Convergence at custom threshold, if provided.
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
  print_summary = TRUE,
  progress = FALSE
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
  module_names <- unique(info$module_exp_data$module)

  # Initialize list to store convergence results
  mc_convergence_list <- list()
  list_index <- 1

  # Iterate through each module (expression group) in the Monte Carlo module
  for (h in seq_along(module_names)) {
    expression <- module_names[h]
    exp_h <- info$module_exp_data$exp[
      info$module_exp_data$module == module_names[h]
    ]

    # Get all nodes for this expression
    exp_h_nodes <- names(mcmodule$node_list)[
      unlist(lapply(names(mcmodule$node_list), function(x) {
        mcmodule$node_list[[x]][["exp_name"]] %in% exp_h
      }))
    ]

    dims <- mcmodule_dim_check(mcmodule, exp_h_nodes)

    if (progress) {
      exp_label <- paste(exp_h, collapse = ", ")
      cat(sprintf(
        "\n[Convergence analysis] Expression %s (%d/%d)\n",
        exp_label,
        h,
        length(module_names)
      ))
    }

    # Convert mcmodule to mc objects for this expression
    mc_list <- mcmodule_to_mc(mcmodule, mc_names = exp_h_nodes)

    # Process each variate
    for (j in seq_along(mc_list)) {
      variate <- j
      mc_j <- mc_list[[j]]

      # Analyze convergence for each node
      for (k in seq_along(mc_j)) {
        node <- names(mc_j)[k]
        x <- mc_j[[k]]

        # Only analyze nodes with more than one iteration and that have variability/uncertainty
        if (dim(x)[1] > 1 & !max(x) == min(x)) {
          # Calculate convergence statistics for the specified quantile range
          conv_start <- floor(dim(x)[1] * from_quantile)
          conv_end <- floor(dim(x)[1] * to_quantile)
          n_sim <- dim(x)[1]
          n_sim_conv <- conv_end - conv_start

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

          if (!is.null(conv_threshold)) {
            conv_manual <- abs(max_dif_scaled) < conv_threshold
          }

          tiny <- max_dif < 0.001

          conv_01 <- abs(max_dif_scaled) < 0.01
          conv_025 <- abs(max_dif_scaled) < 0.025
          conv_05 <- abs(max_dif_scaled) < 0.05

          conv_01_tiny <- conv_01 | tiny
          conv_025_tiny <- conv_025 | tiny
          conv_05_tiny <- conv_05 | tiny

          if (!is.na(max_dif) && !is.na(max_dif_scaled)) {
            mc_convergence_list[[list_index]] <- data.frame(
              expression = expression,
              variate = variate,
              node = node,
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
              conv_manual = if (!is.null(conv_threshold)) conv_manual else NA,
              conv_01 = conv_01,
              conv_025 = conv_025,
              conv_05 = conv_05,
              tiny = tiny,
              conv_01_tiny = conv_01_tiny,
              conv_025_tiny = conv_025_tiny,
              conv_05_tiny = conv_05_tiny
            )

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
  max_points <- conv_df[order(conv_df$node, -conv_df$max_dif_scaled), ]
  max_points <- max_points[!duplicated(max_points$node), ]

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
    list(node = conv_df$node),
    max
  )
  node_order <- node_order[order(-node_order$x), ]

  # Convert node to factor with levels ordered by max difference
  conv_df$node <- factor(conv_df$node, levels = node_order$node)
  max_points$node <- factor(max_points$node, levels = node_order$node)

  if (print_summary) {
    # Print analysis results summary
    cat("\n=== Convergence Analysis Summary ===\n")
    cat("\nAnalysis Parameters:")
    cat("\n- Number of simulations:", n_sim)
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

    # Calculate convergence statistics
    total_nodes <- nrow(conv_df)

    pct <- function(x) {
      if (x == 0) {
        "0%"
      } else {
        sprintf("%.2f%%", x)
      }
    }

    cat("\n\nConvergence Results:")
    cat("\n- Total nodes analyzed:", total_nodes)
    if (!is.null(conv_threshold)) {
      converged_manual <- sum(conv_df$conv_manual, na.rm = TRUE)
      cat(sprintf(
        "\n- Nodes converged at %.4f threshold: %d (%s)",
        conv_threshold,
        converged_manual,
        pct(converged_manual / total_nodes * 100)
      ))
    }

    n_tiny <- sum(conv_df$tiny)
    n_conv_01 <- sum(conv_df$conv_01)

    converged_01 <- sum(conv_df$conv_01_tiny)
    converged_025 <- sum(conv_df$conv_025_tiny)
    converged_05 <- sum(conv_df$conv_05_tiny)

    no_converged_05 <- total_nodes - converged_05

    cat(sprintf(
      "\n- Nodes with divergence below 0.001: %d (%s)",
      n_tiny,
      pct(n_tiny / total_nodes * 100)
    ))

    cat(sprintf(
      "\n- Nodes with divergence below 1%% of their mean: %d (%s)",
      n_conv_01,
      pct(n_conv_01 / total_nodes * 100)
    ))

    cat(sprintf(
      "\n- Nodes with divergence below 0.001 or 1%% of their mean: %d (%s)",
      converged_01,
      pct(converged_01 / total_nodes * 100)
    ))

    # Only print 2.5% if not all nodes converged at 1%
    if (converged_01 < total_nodes) {
      cat(sprintf(
        "\n- Nodes with divergence below 0.001 or 2.5%% of their mean: %d (%s)",
        converged_025,
        pct(converged_025 / total_nodes * 100)
      ))
    }

    # Only print 5% if not all nodes converged at 2.5%
    if (converged_025 < total_nodes) {
      cat(sprintf(
        "\n- Nodes with divergence below 0.001 or 5%% of their mean: %d (%s)",
        converged_05,
        pct(converged_05 / total_nodes * 100)
      ))
    }

    # Print deviation statistics
    cat("\n\nStochastic Distributions Stability:")
    cat("\n- Maximum deviation of mean: ")
    cat(sprintf("%.6f", max(conv_df$max_dif_mean, na.rm = TRUE)))
    cat(" (standardized: ")
    cat(sprintf("%.6f", max(conv_df$max_dif_mean_scaled, na.rm = TRUE)))
    cat(")")
    cat("\n- Maximum deviation of median: ")
    cat(sprintf("%.6f", max(conv_df$max_dif_median, na.rm = TRUE)))
    cat(" (standardized: ")
    cat(sprintf("%.6f", max(conv_df$max_dif_median_scaled, na.rm = TRUE)))
    cat(")")
    cat("\n- Maximum deviation of 2.5% quantile: ")
    cat(sprintf("%.6f", max(conv_df$max_dif_q025, na.rm = TRUE)))
    cat(" (standardized: ")
    cat(sprintf("%.6f", max(conv_df$max_dif_q025_scaled, na.rm = TRUE)))
    cat(")")
    cat("\n- Maximum deviation of 97.5% quantile: ")
    cat(sprintf("%.6f", max(conv_df$max_dif_q975, na.rm = TRUE)))
    cat(" (standardized: ")
    cat(sprintf("%.6f", max(conv_df$max_dif_q975_scaled, na.rm = TRUE)))
    cat(")")

    # Happy message if all converged at 5% threshold
    if (converged_01 == total_nodes) {
      cat("\n\nAll nodes successfully converged at 1% threshold! :D\n")
    } else if (converged_025 == total_nodes) {
      cat("\n\nAll nodes successfully converged at 2.5% threshold! :)\n")
    } else if (converged_05 == total_nodes) {
      cat("\n\nAll nodes successfully converged at 5% threshold! :)\n")
    } else {
      cat(sprintf(
        "\n\n%d (%s) nodes did not converge at 5%% threshold :(",
        no_converged_05,
        pct((no_converged_05 / total_nodes) * 100)
      ))
    }
  }

  return(conv_df)
}
