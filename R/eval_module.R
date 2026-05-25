#' Evaluate Monte Carlo Model Expressions
#'
#' Evaluates a model expression or list of expressions to produce an mcmodule
#' object containing simulation results and metadata. Expression may use
#' [mc2d::mcstoc()] and [mc2d::mcdata()] to create nodes inline; nvariates is automatically
#' inferred from the data unless `sample_design` is provided.
#'
#' @details
#' - [mc2d::mcstoc()] and [mc2d::mcdata()] may be used directly inside model expressions.
#'   When these are used you should NOT explicitly supply nvariates, nvariates
#'   will be inferred automatically as the number of rows in the input `data`.
#'   If `sample_design` is provided, any inline `nvariates` argument is removed
#'   and the default `nvariates = 1` is used for inline nodes.
#'   Other arguments are preserved, for example specify `type = "0"` when
#'   providing data without variability/uncertainty (see [mc2d::mcdata()] and [mc2d::mcstoc()]).
#' - By design, mcmodule supports type = "V" (the default, with variability) and
#'   type = "0" (no variability) nodes. Expressions that specify other node
#'   types ("U" or "VU") are not fully supported and downstream compatibility is not
#'   guaranteed.
#' - An explicit `mctable` is optional but highly recommended. If no mctable is
#'   provided, any model nodes that match column names in `data` will be built
#'   from the data. If a `mctable` is provided and a node is
#'   not found there but exists as a data column, a warning will be issued and
#'   the node will be created from the data column. When `sample_design` is provided,
#'   required inputs that match `sample_design` column names are also created from
#'   `sample_design` even if they are not listed in `mctable`.
#' - Within expressions reference input mcnodes by their bare names (e.g.
#'   column1). Do not use `data$column1` or `data["column1"]`.
#'
#' @param exp (language or list). Model expression or list of expressions to evaluate.
#' @param data (data frame). Input data; number of rows determines nvariates for
#'   [mc2d::mcstoc()]/[mc2d::mcdata()] in expressions when `sample_design` is not
#'   provided. With `sample_design`, inline `nvariates` is removed and defaults to
#'   1. Default: NULL (can only be NULL
#'   if `sample_design` with all inputs is provided).
#' @param param_names (named character vector, optional). Names to rename parameters.
#'   Default: NULL.
#' @param prev_mcmodule (mcmodule or list, optional). Previous module(s) for
#'   dependent calculations. Default: NULL.
#' @param summary (logical). If TRUE, calculate summary statistics for output nodes.
#'   Default: FALSE.
#' @param mctable (data frame). Reference table for mcnodes with `mcnode` and
#'   `mc_func` columns. If NULL or not provided, nodes matching `data` column names
#'   are automatically created. If `sample_design` is provided, required inputs
#'   present in `sample_design` are created from it even when absent from
#'   `mctable`. Default [set_mctable()].
#' @param data_keys (list). Data structure and keys for input data. Default:
#'   [set_data_keys()].
#' @param match_keys (character vector, optional). Keys to match `prev_mcmodule`
#'   mcnodes with current data. Default: NULL.
#' @param keys (character vector, optional). Explicit keys for input data. Default: NULL.
#' @param overwrite_keys (logical or NULL). If NULL (default), becomes TRUE when
#'   `data_keys` is NULL or empty; otherwise FALSE.
#' @param sample_design (matrix, data frame, or list, optional). Sampling
#'   design used to create input nodes via [matrix_to_mcnodes()]. Accepts a
#'   matrix/data frame or a list with element `X` (typically output of
#'   [sensitivity::sensitivity] functions). Columns matching expression input nodes are created
#'   from this matrix. Defaults to [set_sample_design()].
#' @param if_not_sampled (character). How to fill input nodes that are required
#'   by the expression but do not appear as columns in `sample_design`. A fixed
#'   value is computed from `mctable$sample_space` and replicated across all
#'   samples. Options are `"median"` (default), `"mean"`, `"max"`, and `"min"`.
#' @param use_variation (character vector, optional). mcnode names to apply
#'   `sensi_variation` expression from `mctable` before node creation. Default: NULL.
#'
#' @return An mcmodule object (list) with elements:
#'   \itemize{
#'     \item data: List containing input data frames.
#'     \item exp: List of evaluated expressions.
#'     \item node_list: Named list of mcnode objects with metadata.
#'   }
#' @export
#'
#' @examples
#' # Basic usage with single expression
#' # Build a quoted expression using mcnodes defined in mctable or built with
#' # mcstoc()/mcdata within the expression (do NOT set nvariates, it is
#' # inferred from nrow(data) when evaluated by eval_module() unless
#' # sample_design is provided, in which case inline nvariates defaults to 1).
#' expr_example <- quote({
#'   # Within-herd prevalence (assigned from a pre-built mcnode w_prev)
#'   infected <- w_prev
#'
#'   # Estimate of clinic sensitivity
#'   clinic_sensi <- mcstoc(runif, min = 0.6, max = 0.8)
#'
#'   # Probability an infected animal is tested in origin and not detected
#'   false_neg <- infected * test_origin * (1 - test_sensi) * (1 - clinic_sensi)
#'
#'   # Probability an infected animal is not tested and not detected
#'   no_test <- infected * (1 - test_origin) * (1 - clinic_sensi)
#'
#'   # no_detect: total probability an infected animal is not detected
#'   no_detect <- false_neg + no_test
#' })
#'
#' # Evaluate
#' eval_module(
#'   exp = expr_example,
#'   data = imports_data,
#'   mctable = imports_mctable,
#'   data_keys = imports_data_keys
#' )
eval_module <- function(
  exp,
  data = NULL,
  param_names = NULL,
  prev_mcmodule = NULL,
  summary = FALSE,
  mctable = set_mctable(),
  data_keys = set_data_keys(),
  match_keys = NULL,
  keys = NULL,
  overwrite_keys = NULL,
  sample_design = set_sample_design(),
  if_not_sampled = c("median", "mean", "max", "min"),
  use_variation = NULL
) {
  if (is.null(data)) {
    data <- data.frame()
  }

  data_name <- deparse(substitute(data))

  sample_design_data <- NULL
  if (!is.null(sample_design)) {
    sample_design_input <- sample_design
    if (
      is.list(sample_design_input) &&
        !is.data.frame(sample_design_input) &&
        !is.matrix(sample_design_input)
    ) {
      if (!"X" %in% names(sample_design_input)) {
        stop("sample_design list must contain element 'X'")
      }
      sample_design_input <- sample_design_input$X
    }

    if (
      !(is.matrix(sample_design_input) || is.data.frame(sample_design_input))
    ) {
      stop(
        "sample_design must be a matrix, data frame, or list with element 'X'"
      )
    }
    sample_design_data <- as.data.frame(
      sample_design_input,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    if (nrow(sample_design_data) < 1) {
      stop("sample_design has 0 rows")
    }
    if (ncol(sample_design_data) < 1) {
      stop("sample_design has 0 columns")
    }
  }

  if_not_sampled <- match.arg(if_not_sampled)

  mctable <- check_mctable(mctable)

  # Validate that data is not empty
  if (nrow(data) < 1 && is.null(sample_design_data)) {
    stop(sprintf("data '%s' has 0 rows", data_name))
  }

  resize_input_ndvar <- function(mcnode_obj, target_ndvar) {
    if (is.null(target_ndvar) || !is.mcnode(mcnode_obj)) {
      return(mcnode_obj)
    }

    mc_type <- attr(mcnode_obj, "type", exact = TRUE)
    if (!identical(mc_type, "V")) {
      return(mcnode_obj)
    }

    mc_dim <- dim(mcnode_obj)
    if (length(mc_dim) < 3 || mc_dim[1] == target_ndvar) {
      return(mcnode_obj)
    }

    n_variates <- mc_dim[3]
    node_matrix <- matrix(NA_real_, nrow = target_ndvar, ncol = n_variates)

    for (vv in seq_len(n_variates)) {
      node_values <- as.numeric(mcnode_obj[, 1, vv])
      node_matrix[, vv] <- rep_len(node_values, target_ndvar)
    }

    mcdata(
      data = as.vector(node_matrix),
      type = "V",
      nsv = target_ndvar,
      nvariates = n_variates
    )
  }

  # Normalize optional OAT arguments
  use_variation <- if (is.null(use_variation)) {
    character()
  } else {
    unique(use_variation[!is.na(use_variation) & use_variation != ""])
  }

  data_eval <- data
  mctable_eval <- mctable

  if (length(use_variation) > 0) {
    target_nodes <- unique(use_variation)

    for (mc_name in target_nodes) {
      row_idx <- which(mctable_eval$mcnode == mc_name)
      if (length(row_idx) == 0) {
        warning(sprintf("%s not found in mctable", mc_name))
        next
      }
      row_idx <- row_idx[[1]]
      mc_row <- mctable_eval[row_idx, ]

      if (mc_name %in% use_variation) {
        transformation <- as.character(mc_row$transformation)
        if (!is.na(transformation)) {
          value_name <- ifelse(
            is.na(mc_row$from_variable),
            mc_name,
            as.character(mc_row$from_variable)
          )
          if (value_name %in% names(data_eval)) {
            assign("value", data_eval[[value_name]], envir = environment())
            data_eval[[mc_name]] <- eval(
              parse(text = transformation),
              envir = environment()
            )
            rm("value", envir = environment())
            mctable_eval[row_idx, "transformation"] <- NA
          }
        }

        variation_text <- as.character(mc_row$sensi_variation)
        if (is.na(variation_text) || variation_text == "") {
          warning(sprintf("sensi_variation not specified for %s", mc_name))
          next
        }

        if (!is.na(mc_row$mc_func)) {
          param_cols <- names(data_eval)[
            grepl(paste0("^", mc_name, "_"), names(data_eval))
          ]
          if (length(param_cols) == 0) {
            warning(sprintf("No input columns found for %s", mc_name))
            next
          }
          for (param_col in param_cols) {
            assign("value", data_eval[[param_col]], envir = environment())
            data_eval[[param_col]] <- eval(
              parse(text = variation_text),
              envir = environment()
            )
          }
          rm("value", envir = environment())
        } else if (mc_name %in% names(data_eval)) {
          assign("value", data_eval[[mc_name]], envir = environment())
          data_eval[[mc_name]] <- eval(
            parse(text = variation_text),
            envir = environment()
          )
          rm("value", envir = environment())
        } else {
          warning(sprintf("No input column found for %s", mc_name))
        }
      }
    }
  }

  data <- data_eval
  mctable <- mctable_eval

  # Determine default for overwrite_keys when not explicitly provided:
  # - If overwrite_keys is NULL and data_keys is NULL or an empty list -> default TRUE
  # - Otherwise default FALSE
  if (is.null(overwrite_keys)) {
    overwrite_keys <- is.null(data_keys) ||
      (is.list(data_keys) && length(data_keys) == 0)
  }

  # Validate keys argument (must be character vector or NULL)
  if (!is.null(keys) && !is.character(keys)) {
    stop("keys must be a character vector or NULL")
  }

  # If overwrite_keys is TRUE create a local data_keys entry for this data
  # (do not modify global data_keys)
  if (isTRUE(overwrite_keys)) {
    data_keys_original <- data_keys
    data_keys_local <- list(cols = names(data), keys = keys)
    data_keys <- list()
    data_keys[[data_name]] <- data_keys_local
    if (!is.null(keys)) {
      message(sprintf(
        "data_keys overwritten for %s with keys: %s",
        data_name,
        paste(keys, collapse = ", ")
      ))
    } else if (length(data_keys_original) > 0) {
      # Only message if data_keys were not NULL (to avoid message when both are NULL)
      message(sprintf("data_keys overwritten for %s", data_name))
    }
    # When overwritten, we do not need to forward keys separately
    keys_arg <- NULL
  } else {
    # Do not merge into existing data_keys; forward explicit keys to get_node_list
    keys_arg <- keys
  }

  # Convert single expression to list format
  if (is.list(exp)) {
    exp_list <- exp
  } else {
    # Determine a sensible name for the expression.
    # - If the caller passed a variable (e.g. `exp = test_exp`) use that symbol name.
    # - If the caller passed a quoted expression directly (e.g. `exp = quote({...})`)
    #   deparse(substitute(exp)) will be the expression text; warn and recommend
    #   explicitly naming expressions in that case.
    expr_sub <- substitute(exp)
    exp_name <- deparse(expr_sub)
    if (is.call(expr_sub) || is.expression(expr_sub)) {
      warning(
        "You passed a quoted expression directly to `exp`. Consider naming expressions explicitly, e.g. eval_module(exp = list(my_name = quote({...})))."
      )
    }

    exp_list <- list(exp)
    names(exp_list) <- exp_name
  }

  node_list <- list()
  sampled_nodes_all <- character()
  fixed_nodes_all <- character()
  target_ndvar <- if (!is.null(sample_design_data)) {
    nrow(sample_design_data)
  } else {
    NULL
  }

  # Process each expression in the list
  for (i in 1:length(exp_list)) {
    exp_i <- exp_list[[i]]
    exp_name_i <- names(exp_list)[[i]]

    # When sample_design is provided, rewrite inline mcdata/mcstoc so they use
    # nsv = nrow(sample_design) and omit nvariates.
    if (!is.null(sample_design_data)) {
      strip_inline_nvariates_ast <- function(expr) {
        if (is.call(expr)) {
          for (idx in seq_along(expr)) {
            if (idx == 1) {
              next
            }
            expr[[idx]] <- strip_inline_nvariates_ast(expr[[idx]])
          }

          fn_deparsed <- paste(deparse(expr[[1]]), collapse = "")
          is_target <- grepl("(^|::)mcdata$|(^|::)mcstoc$", fn_deparsed)

          if (is_target) {
            nm <- names(expr)
            expr_list <- as.list(expr)

            if (!is.null(nm) && "nvariates" %in% nm) {
              keep <- nm != "nvariates"
              expr_list <- expr_list[keep]
              nm <- nm[keep]
            }

            if (!is.null(nm) && "nsv" %in% nm) {
              expr_list[["nsv"]] <- nrow(sample_design_data)
            } else {
              expr_list[["nsv"]] <- nrow(sample_design_data)
            }

            expr <- as.call(expr_list)
          }
          return(expr)
        }

        if (is.expression(expr)) {
          for (idx in seq_along(expr)) {
            expr[[idx]] <- strip_inline_nvariates_ast(expr[[idx]])
          }
          return(expr)
        }

        expr
      }

      exp_i <- strip_inline_nvariates_ast(exp_i)
    }

    # Get initial node list (forward keys_arg as character vector or NULL)
    node_list_i <- get_node_list(
      exp = exp_i,
      param_names = param_names,
      mctable = mctable,
      data_keys = data_keys,
      keys = keys_arg
    )

    if (!is.null(sample_design_data)) {
      sampled_prev_nodes_i <- names(node_list_i)[
        sapply(node_list_i, function(x) identical(x[["type"]], "prev_node")) &
          names(node_list_i) %in% colnames(sample_design_data)
      ]

      if (length(sampled_prev_nodes_i) > 0) {
        for (mc_name_sampled in sampled_prev_nodes_i) {
          node_list_i[[mc_name_sampled]][["type"]] <- "in_node"
          node_list_i[[mc_name_sampled]][["mc_name"]] <- mc_name_sampled
          node_list_i[[mc_name_sampled]][["exp_name"]] <- exp_name_i
        }
      }
    }

    in_nodes_i <- names(node_list_i)[
      sapply(node_list_i, function(x) identical(x[["type"]], "in_node"))
    ]

    sampled_nodes_i <- character()
    if (!is.null(sample_design_data)) {
      # For any input node not present in the sample_design, create a fixed
      # column from mctable$sample_space and append it to sample_design_data.
      not_sampled_nodes_i <- setdiff(in_nodes_i, colnames(sample_design_data))
      if (length(not_sampled_nodes_i) > 0) {
        for (mc_name_fix in not_sampled_nodes_i) {
          row_idx_fix <- which(mctable$mcnode == mc_name_fix)
          if (length(row_idx_fix) == 0) {
            stop(sprintf(
              "Input '%s' is missing from sample_design and not found in mctable",
              mc_name_fix
            ))
          }
          row_idx_fix <- row_idx_fix[[1]]

          ss_fix <- as.character(mctable$sample_space[row_idx_fix])
          bounds_fix <- parse_sample_space_bounds(ss_fix)
          if (is.null(bounds_fix)) {
            stop(sprintf(
              "Input '%s' is missing from sample_design and has no numeric bounds in mctable$sample_space",
              mc_name_fix
            ))
          }

          fixed_val <- fixed_from_bounds(bounds_fix, if_not_sampled)
          sample_design_data[[mc_name_fix]] <- rep(
            fixed_val,
            nrow(sample_design_data)
          )

          fixed_nodes_all <- unique(c(fixed_nodes_all, mc_name_fix))
        }
      }

      # Recompute sampled nodes after appending fixed columns.
      sampled_nodes_i <- intersect(in_nodes_i, colnames(sample_design_data))
      sampled_nodes_all <- unique(c(sampled_nodes_all, sampled_nodes_i))

      if (length(sampled_nodes_i) > 0) {
        matrix_to_mcnodes(
          X = sample_design_data[, sampled_nodes_i, drop = FALSE],
          envir = environment()
        )
      }
    }

    # Identify nodes requiring previous module inputs
    all_prev_nodes <- names(node_list_i)[
      sapply(node_list_i, function(x) identical(x[["type"]], "prev_node"))
    ]

    # If sample_design is provided, and there is no data/prev_mcmodule to build
    # these nodes, attempt to create them as fixed columns from mctable$sample_space.
    if (
      !is.null(sample_design_data) && is.null(prev_mcmodule) && nrow(data) < 1
    ) {
      missing_prev <- setdiff(all_prev_nodes, colnames(sample_design_data))
      missing_prev <- setdiff(missing_prev, names(node_list))

      if (length(missing_prev) > 0) {
        for (mc_name_fix in missing_prev) {
          row_idx_fix <- which(mctable$mcnode == mc_name_fix)
          if (length(row_idx_fix) == 0) {
            stop(sprintf(
              "Input '%s' is missing from sample_design and not found in mctable",
              mc_name_fix
            ))
          }
          row_idx_fix <- row_idx_fix[[1]]

          ss_fix <- as.character(mctable$sample_space[row_idx_fix])
          bounds_fix <- parse_sample_space_bounds(ss_fix)
          if (is.null(bounds_fix)) {
            stop(sprintf(
              "Input '%s' is missing from sample_design and has no numeric bounds in mctable$sample_space",
              mc_name_fix
            ))
          }

          fixed_val <- fixed_from_bounds(bounds_fix, if_not_sampled)
          sample_design_data[[mc_name_fix]] <- rep(
            fixed_val,
            nrow(sample_design_data)
          )

          fixed_nodes_all <- unique(c(fixed_nodes_all, mc_name_fix))
        }

        # materialize these newly-added columns as mcnodes
        matrix_to_mcnodes(
          X = sample_design_data[, missing_prev, drop = FALSE],
          envir = environment()
        )
        sampled_nodes_all <- unique(c(sampled_nodes_all, missing_prev))
      }
    }

    prev_nodes <- all_prev_nodes[!all_prev_nodes %in% names(node_list)]

    data_nodes <- prev_nodes[prev_nodes %in% names(data)]

    # Process nodes requiring previous module inputs
    if (length(prev_nodes) > 0) {
      if (!is.null(prev_mcmodule)) {
        prev_mcmodule_list <- if (inherits(prev_mcmodule, "mcmodule")) {
          list(prev_mcmodule)
        } else {
          prev_mcmodule
        }

        # Previous modules
        for (j in 1:length(prev_mcmodule_list)) {
          prev_mcmodule_i <- prev_mcmodule_list[[j]]

          # Prefix matching for node names
          if (any(!prev_nodes %in% names(prev_mcmodule_i$node_list))) {
            prefixes <- unlist(sapply(
              prev_mcmodule_i$node_list,
              "[[",
              "prefix"
            ))
            if (!is.null(prefixes)) {
              new_names <- sapply(names(prefixes), function(x) {
                gsub(paste0("^", prefixes[x], "_"), "", x)
              })

              original_names <- names(prefixes)
              names(prefixes) <- new_names

              prev_nodes_names <- prev_nodes
              prev_nodes <- ifelse(
                prev_nodes %in% original_names,
                prev_nodes,
                ifelse(
                  is.na(prefixes[prev_nodes]),
                  prev_nodes,
                  paste0(prefixes[prev_nodes], "_", prev_nodes)
                )
              )
              names(prev_nodes) <- prev_nodes_names
              prev_param_names <- prev_nodes
            }
          }
          # Get nodes from previous module
          prev_node_list_i <- get_mcmodule_nodes(
            prev_mcmodule_i,
            mc_names = prev_nodes
          )

          #Check if all true prev_nodes are found in prev_mcmodule
          missing_prev_nodes <- prev_nodes[
            !prev_nodes %in% names(prev_node_list_i)
          ]

          if (length(missing_prev_nodes) > 0) {
            # If any missing prev_nodes, check if they are allowed to be missing due to null_rm
            missing_prev_nodes <- missing_prev_nodes[
              !sapply(missing_prev_nodes, function(x) {
                isTRUE(node_list_i[[x]][["null_rm"]])
              })
            ]
            # If there are still missing prev_nodes, throw error
            if (length(missing_prev_nodes) > 0) {
              stop(sprintf(
                "%s not found in prev_mcmodule",
                paste0(missing_prev_nodes, collapse = ", ")
              ))
            }
            # Update prev_nodes to only those that are actually in prev_mcmodule
            prev_nodes <- prev_nodes[
              prev_nodes %in% names(prev_node_list_i)
            ]
          }

          # Process each previous node
          if (length(prev_nodes) > 0) {
            for (k in 1:length(prev_nodes)) {
              mc_name <- prev_nodes[k]
              node_list_i[[mc_name]] <- prev_node_list_i[[mc_name]]
              # Check if previous node is an aggregated node,
              # - if it is NOT an aggregated node it will be matched by its reference data_name
              # - if it is an aggregated node it will be matched by its summary
              if (
                is.null(prev_node_list_i[[mc_name]][["agg_keys"]]) ||
                  prev_node_list_i[[mc_name]][["keep_variates"]]
              ) {
                data_name_k <- prev_node_list_i[[mc_name]]$data_name

                if (length(data_name_k) > 1) {
                  message(sprintf(
                    "Multiple data_name found for %s: %s. Using summary to match dimensions.",
                    mc_name,
                    paste(data_name_k, collapse = ", ")
                  ))
                  prev_data <- NULL
                } else {
                  prev_data <- prev_mcmodule_i$data[[data_name_k]]
                }

                # Match if previous node data is not equal to new data or if node has multiple data_names
                # IF IT PASSES ALL THE CHECKS IT ALREADY MATCHES AND NO MATCH IS NEEDED
                if (
                  is.null(prev_data) ||
                    !(nrow(prev_data) == nrow(data) &&
                      ncol(prev_data) == ncol(data) &&
                      nrow(prev_data) ==
                        dim(prev_mcmodule$node_list[[mc_name]]$mcnode)[[3]] &&
                      all(names(prev_data) == names(data)) &&
                      all(prev_data == data, na.rm = TRUE))
                ) {
                  match_prev <- mc_match_data(
                    prev_mcmodule,
                    mc_name,
                    data,
                    keys_names = match_keys
                  )
                  match_prev_mcnode <- match_prev[[1]]
                  data <- match_prev[["data_match"]]

                  assign(mc_name, match_prev_mcnode)
                }
              } else {
                # Match previous aggregated node with current data and update data
                agg_keys <- prev_node_list_i[[mc_name]][["agg_keys"]]

                if (!is.null(match_keys)) {
                  if (!all(agg_keys %in% match_keys)) {
                    message(sprintf(
                      "Using match_keys (%s) instead of: %s",
                      paste(match_keys, collapse = ", "),
                      paste(agg_keys, collapse = ", ")
                    ))
                    agg_keys <- match_keys
                  }
                }

                message(sprintf(
                  "Matching agg prev_nodes dimensions by: %s",
                  paste(agg_keys, collapse = ", ")
                ))

                match_agg_prev <- mc_match_data(
                  mcmodule = prev_mcmodule,
                  mc_name = mc_name,
                  data = data,
                  keys_names = agg_keys
                )

                match_prev_mcnode <- match_agg_prev[[1]]
                data <- match_agg_prev[["data_match"]]

                assign(mc_name, match_prev_mcnode)
              }
            }
          }
        }
      } else if (length(data_nodes) > 0) {
        # Update prev_nodes to only those not in data
        prev_nodes <- prev_nodes[!prev_nodes %in% names(data)]
        # If prev_nodes are in data, create mcnodes directly from data
        data_mctable <- data.frame(
          mcnode = data_nodes,
          mc_func = NA
        )

        create_mcnodes(data = data, mctable = data_mctable)

        # Update node list
        node_list_i_data <- get_node_list(
          exp = exp_i,
          param_names = param_names,
          mctable = data_mctable,
          data_keys = data_keys,
          keys = keys_arg
        )

        node_list_i[data_nodes] <- node_list_i_data[data_nodes]

        if (nrow(mctable) == 0) {
          message("Creating mcnodes from data (mctable not provided)")
        } else {
          message(sprintf(
            "The following nodes are present in data but not in the mctable: %s.",
            paste(data_nodes, collapse = ", ")
          ))
        }
      } else {
        # Error if prev_nodes are neither in prev_mcmodule nor in data
        stop(sprintf(
          "The following nodes are not present in data or in prev_mcmodule: %s.",
          paste(prev_nodes, collapse = ", ")
        ))
      }
    }

    # Combine node lists
    node_list <- c(node_list, node_list_i)

    # Update parameter names
    new_param_names <- if (exists("prev_param_names")) {
      c(param_names, prev_param_names)
    } else {
      param_names
    }

    # Handle parameter renaming
    if (!is.null(new_param_names)) {
      for (j in 1:length(new_param_names)) {
        exp_name <- names(new_param_names)[j]
        param_name <- new_param_names[j]

        if (exists(param_name)) {
          assign(exp_name, get(param_name))
        } else if (!is.null(prev_mcmodule$node_list[[param_name]])) {
          assign(
            exp_name,
            prev_mcmodule$node_list[[param_name]][["mcnode"]]
          )
        }
      }
    }

    # Create mcnodes for the current expression
    mctable_i = mctable[
      mctable$mcnode %in% in_nodes_i & !(mctable$mcnode %in% sampled_nodes_i),
    ]

    if (!is.null(sample_design_data) && nrow(mctable_i) > 0 && nrow(data) < 1) {
      stop(sprintf(
        "data has 0 rows and the following input nodes are not provided in sample_design and cannot be created from mctable$sample_space: %s",
        paste(mctable_i$mcnode, collapse = ", ")
      ))
    }

    if (nrow(mctable_i) > 0) {
      create_mcnodes(data = data, mctable = mctable_i)

      if (!is.null(sample_design_data)) {
        for (mc_name_resize in mctable_i$mcnode) {
          if (exists(mc_name_resize)) {
            assign(
              mc_name_resize,
              resize_input_ndvar(get(mc_name_resize), target_ndvar),
              envir = environment()
            )
          }
        }
      }
    }

    # Add/remove nvariates/nsv in mcstoc/mcdata in current expression
    # - without sample_design: enforce inferred nvariates = nrow(data)
    # - with sample_design: enforce nsv = nrow(sample_design) and remove explicit nvariates
    # Only run if at least one node was created inside the expression
    if (any(sapply(node_list_i, function(x) isTRUE(x$created_in_exp)))) {
      add_nvariates_ast <- function(
        expr,
        data_name = "data",
        use_sample_design = FALSE
      ) {
        # Recursively walk and modify calls
        if (is.call(expr)) {
          # Recurse into function name if it's a call (e.g. pkg::fn)
          # then recurse into arguments
          for (i in seq_along(expr)) {
            if (i == 1) {
              next
            }
            expr[[i]] <- add_nvariates_ast(
              expr[[i]],
              data_name,
              use_sample_design
            )
          }

          fn_deparsed <- paste(deparse(expr[[1]]), collapse = "")
          is_target <- grepl("(^|::)mcdata$|(^|::)mcstoc$", fn_deparsed)

          if (is_target) {
            nm <- names(expr)

            if (isTRUE(use_sample_design)) {
              expr_list <- as.list(expr)

              # Remove explicit nvariates; inline nodes should use default nvariates (=1)
              if (!is.null(nm) && "nvariates" %in% nm) {
                keep <- nm != "nvariates"
                expr_list <- expr_list[keep]
                nm <- nm[keep]
              }

              # Force compatibility with sample_design-created nodes.
              expr_list[["nsv"]] <- call("nrow", as.name("sample_design_data"))
              expr <- as.call(expr_list)
            } else {
              if (!is.null(nm) && "nvariates" %in% nm) {
                stop("Remove 'nvariates' argument")
              }
              # append nvariates = nrow(data)
              idx <- length(expr) + 1
              expr[[idx]] <- call("nrow", as.name(data_name))
              nms <- names(expr)
              if (is.null(nms)) {
                nms <- rep("", length(expr))
              }
              nms[idx] <- "nvariates"
              names(expr) <- nms
            }
          }
          return(expr)
        } else if (is.expression(expr)) {
          # expression vector: apply to each element
          for (i in seq_along(expr)) {
            expr[[i]] <- add_nvariates_ast(
              expr[[i]],
              data_name,
              use_sample_design
            )
          }
          return(expr)
        } else {
          return(expr)
        }
      }

      # modify the quoted expression in place
      exp_i <- add_nvariates_ast(
        exp_i,
        data_name = "data",
        use_sample_design = !is.null(sample_design_data)
      )
    }
    # Evaluate current expression
    eval(exp_i)
    message(sprintf("%s evaluated", exp_name_i))

    # Update node metadata
    for (j in 1:length(node_list)) {
      mc_name <- names(node_list)[j]

      # Skip processing for prev_nodes that are NOT in data
      # (prev_nodes in data will be handled separately and still need metadata updates)
      if (mc_name %in% all_prev_nodes && !mc_name %in% data_nodes) {
        next
      }

      # Update input references
      inputs <- node_list[[mc_name]][["inputs"]]
      node_list[[mc_name]][["exp_param"]] <- inputs

      inputs[inputs %in% names(new_param_names)] <-
        new_param_names[inputs[inputs %in% names(new_param_names)]]
      node_list[[mc_name]][["inputs"]] <- inputs

      # Update keys and add exp name for output nodes
      if (
        ((!is.null(prev_mcmodule)) | (length(exp) > 1)) &
          node_list[[mc_name]][["type"]] == "out_node"
      ) {
        keys_names <- unique(unlist(lapply(inputs, function(x) {
          if (is.null(node_list[[x]][["agg_keys"]])) {
            node_list[[x]][["keys"]]
          } else {
            node_list[[x]][["agg_keys"]]
          }
        })))
        node_list[[mc_name]][["keys"]] <- keys_names
      }

      # Scalar to mcnode conversion
      mcnode <- get(mc_name)
      if (!is.mcnode(mcnode) & is.numeric(mcnode)) {
        mcnode <- mcdata(mcnode, type = "0", nvariates = length(mcnode))
      }

      # Update node metadata
      node_list[[mc_name]][["mcnode"]] <- mcnode

      # Update data_name and sample design flags
      if (!is.null(sample_design_data)) {
        node_list[[mc_name]][["data_name"]] <- NULL
      } else {
        node_list[[mc_name]][["data_name"]] <- data_name
      }

      if (mc_name %in% sampled_nodes_all) {
        node_list[[mc_name]][["from_sample_design"]] <- TRUE

        if (mc_name %in% fixed_nodes_all) {
          node_list[[mc_name]][["from_sample_design_fixed"]] <- TRUE
        } else {
          node_list[[mc_name]][["from_sample_design_fixed"]] <- FALSE
        }
      }

      # If all mcnode inputs are from sample_design, mark this node as from_sample_design
      if (
        length(inputs) > 0 &&
          all(inputs %in% names(node_list)) &&
          !is.null(sample_design) &&
          all(sapply(inputs, function(x) {
            isTRUE(node_list[[x]][["from_sample_design"]]) ||
              isTRUE(node_list[[x]][["type"]] == "scalar") ||
              isTRUE(node_list[[x]][["created_in_exp"]])
          }))
      ) {
        node_list[[mc_name]][["from_sample_design"]] <- TRUE
      }

      node_list[[mc_name]][["mc_name"]] <- mc_name

      # Set module name
      if (
        length(node_list[[mc_name]][["exp_name"]]) == 0 ||
          node_list[[mc_name]][["exp_name"]] %in% "exp_i"
      ) {
        node_list[[mc_name]][["exp_name"]] <- exp_name_i
      }

      # Calculate summary statistics if requested
      if (summary & is.mcnode(mcnode)) {
        inputs_names <- node_list[[mc_name]][["inputs"]]

        keys_names <- if (is.null(node_list[[mc_name]][["agg_keys"]])) {
          node_list[[mc_name]][["keys"]]
        } else {
          node_list[[mc_name]][["agg_keys"]]
        }

        node_summary <- mc_summary(
          data = data,
          mcnode = mcnode,
          mc_name = mc_name,
          keys_names = keys_names
        )

        node_list[[mc_name]][["summary"]] <- node_summary
      }
    }
  }

  # Remove temporary previous nodes
  node_list <- node_list[!(sapply(node_list, "[[", "type") == "prev_node")]

  # Return results
  mcmodule <- list(
    data = list(data),
    exp = exp_list,
    node_list = node_list
  )

  names(mcmodule$data) <- data_name
  class(mcmodule) <- "mcmodule"

  message(sprintf(
    "mcmodule created (expressions: %s)",
    paste(names(exp_list), collapse = ", ")
  ))

  return(mcmodule)
}


#' Get Nodes from Monte Carlo Module
#'
#' Retrieves nodes from a Monte Carlo module and assigns them to the parent environment
#'
#' @param mcmodule An mcmodule or mcnode_list object
#' @param mc_names Optional vector of node names to retrieve
#' @param envir Environment where MC nodes will be created (default: parent.frame())
#'
#' @return A subset of the node list containing requested nodes
get_mcmodule_nodes <- function(
  mcmodule,
  mc_names = NULL,
  envir = parent.frame()
) {
  if (inherits(mcmodule, "mcmodule")) {
    node_list <- mcmodule$node_list
  } else if (inherits(mcmodule, "mcnode_list")) {
    node_list <- mcmodule
  } else {
    stop("mcmodule or mcnode_list object must be provided")
  }

  all_mc_names <- names(node_list)
  mc_names <- all_mc_names[all_mc_names %in% mc_names]

  if (length(mc_names) > 0) {
    for (i in 1:length(mc_names)) {
      mc_name <- mc_names[i]
      assign(mc_name, node_list[[mc_name]][["mcnode"]], envir = envir)
    }
  }

  return(node_list[mc_names])
}
