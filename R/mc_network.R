#' Generate Edge Table for Network Visualisation
#'
#' Creates a data frame containing edge relationships between nodes in a
#' Monte Carlo module network. Each row represents a directed edge from one
#' node to another.
#'
#' @param mcmodule (mcmodule object). Module containing node relationships.
#' @param inputs (logical). If TRUE, include non-node inputs (datasets,
#'   dataframes, and columns). Default: FALSE.
#'
#' @return A data frame with columns `node_from` and `node_to` representing
#'   network edges.
#' @examples
#' edge_table <- get_edge_table(imports_mcmodule)
#' @export
get_edge_table <- function(mcmodule, inputs = FALSE) {
  node_list <- mcmodule$node_list
  edge_table <- data.frame()

  if (all(is.null(unlist(sapply(node_list, "[[", "input_dataset"))))) {
    message("input_dataset not found, using data_name")
  }

  for (i in seq_along(node_list)) {
    node_to <- names(node_list)[i]
    if (inputs & "inputs_col" %in% names(node_list[[i]])) {
      node_from <- node_list[[i]][["inputs_col"]]
      dataset_from <- node_list[[i]][["input_dataset"]]
      data_from <- node_list[[i]][["data_name"]]

      if (!is.null(dataset_from)) {
        edge_table_dataset <- data.frame(
          node_from = dataset_from,
          node_to = data_from
        )
        edge_table_inputs <- data.frame(
          node_from = data_from,
          node_to = node_from
        )
        edge_table_inputs <- rbind(edge_table_dataset, edge_table_inputs)
      } else {
        edge_table_inputs <- data.frame(
          node_from = data_from,
          node_to = node_from
        )
      }
    } else {
      node_from <- node_list[[i]][["inputs"]]
      edge_table_inputs <- NULL
    }

    if (!length(node_from) > 0) {
      next
    }

    edge_table_i <- data.frame(node_from, node_to)
    edge_table <- rbind(edge_table, edge_table_i, edge_table_inputs)
  }

  edge_table <- unique(edge_table)
  rownames(edge_table) <- NULL
  return(edge_table)
}

#' Generate Node Table for Network Visualisation
#'
#' Creates a data frame containing node information from a Monte Carlo module
#' network. Includes node attributes, values, and relationships.
#'
#' @param mcmodule (mcmodule object). Module containing node information.
#' @param variate (integer). Which variate to extract. Default: 1.
#' @param inputs (logical). If TRUE, include non-node inputs (datasets,
#'   dataframes, and columns). Default: FALSE.
#'
#' @return A data frame containing node information and attributes.
#' @examples
#' node_table <- get_node_table(imports_mcmodule)
#' @export
get_node_table <- function(mcmodule, variate = 1, inputs = FALSE) {
  data <- mcmodule$data
  node_list <- mcmodule$node_list
  node_table <- data.frame()

  # Process node information
  for (i in seq_along(node_list)) {
    node <- node_list[[i]]

    node_value <- if (length(node[["mcnode"]]) > 0) {
      summary_value <- data.frame(summary(extractvar(
        node[["mcnode"]],
        variate
      ))[[1]])

      if (length(summary_value$mean) > 0) {
        if (grepl("_n$|_n_|_time$", names(node_list)[i])) {
          format_numeric_summary(summary_value)
        } else {
          format_percentage_summary(summary_value)
        }
      } else {
        as.character(summary_value[1, ])
      }
    } else {
      "Not Calc"
    }

    node[c("mcnode", "summary")] <- NULL
    node <- lapply(node, paste, collapse = ", ")

    node_table_i <- do.call(cbind.data.frame, node)
    node_table_i$name <- names(node_list)[i]
    node_table_i$value <- node_value
    # Inputs: if 'inputs' is NA, 'inputs_col'. If 'inputs_col' is NA, NA
    # Ensure 'inputs' and 'inputs_col' exist, then prefer 'inputs' and fall back to 'inputs_col'
    if (!"inputs" %in% names(node_table_i)) {
      node_table_i$inputs <- NA
    }
    if (!"inputs_col" %in% names(node_table_i)) {
      node_table_i$inputs_col <- NA
    }

    node_table_i$inputs <- dplyr::coalesce(
      node_table_i$inputs,
      node_table_i$inputs_col
    )

    node_table <- dplyr::bind_rows(node_table, node_table_i)
  }

  # Process non-node information (data-sets, data-frames and columns)
  if (inputs) {
    for (i in 1:length(node_list)) {
      node <- node_list[[i]]

      if (length(node[["inputs_col"]]) > 0) {
        inputs_col <- node[["inputs_col"]]
        if (is.list(data)) {
          value_col <- c()
          for (j in 1:length(data)) {
            data_name <- names(data)[j]
            data_j <- data[[j]]
            if (all(inputs_col %in% names(data_j))) {
              value_j <- as.character(unlist(data_j[variate, inputs_col]))
              value_col <- value_j
            }
          }
        } else {
          value_col <- as.character(unlist(data[variate, inputs_col]))
        }

        if (is.null(value_col)) {
          value_col <- "Not Found"
        }

        inputs_col_table <- data.frame(
          name = node[["inputs_col"]],
          type = "inputs_col",
          inputs = paste(
            c(node[["data_name"]], node[["input_dataset"]]),
            sep = ", ",
            collapse = ", "
          ),
          input_data = node[["data_name"]],
          value = value_col
        )

        node_table <- dplyr::bind_rows(node_table, inputs_col_table)

        input_data_table <- data.frame(
          name = node[["data_name"]],
          type = "input_data"
        )

        node_table <- dplyr::bind_rows(node_table, input_data_table)

        if (!is.null(node[["input_dataset"]])) {
          input_dataset_table <- data.frame(
            name = node[["input_dataset"]],
            type = "input_dataset"
          )

          inputs_col_table$input_dataset <- node[["input_dataset"]]

          input_data_table <- data.frame(
            inputs = node[["input_dataset"]],
            input_dataset = node[["input_dataset"]]
          )

          node_table <- dplyr::bind_rows(node_table, input_dataset_table)
        }
      }
    }
  }

  node_table <- dplyr::relocate(node_table, "name")
  rownames(node_table) <- NULL
  return(node_table)
}

#' Generate Formatted Network Node Table for Visualisation
#'
#' Creates a formatted node table for visualisation with visNetwork.
#' Includes styling and formatting for interactive network display.
#'
#' @param mcmodule (mcmodule object). Module containing network structure.
#' @param variate (integer). Which variate to extract. Default: 1.
#' @param color_pal (character vector, optional). Custom colour palette for nodes.
#'   Default: NULL.
#' @param color_by (character, optional). Column name to determine node colours.
#'   Default: NULL.
#' @param inputs (logical). If TRUE, include non-node inputs. Default: FALSE.
#'
#' @return A data frame formatted for visNetwork with columns: id, label, color,
#'   grouping, expression, and title (hover text).
visNetwork_nodes <- function(
  mcmodule,
  variate = 1,
  color_pal = NULL,
  color_by = NULL,
  inputs = FALSE
) {
  nodes <- get_node_table(
    mcmodule = mcmodule,
    variate = variate,
    inputs = inputs
  )

  color <- assign_color_pal(
    nodes = nodes,
    color_pal = color_pal,
    color_by = color_by
  )
  color_pal <- color[["pal"]]
  color_by <- color[["by"]]

  # Ensure all columns required by the subsequent transmute exist on `nodes`.
  required_cols <- c(
    "mc_func",
    "exp_param",
    "node_exp",
    "module",
    "type",
    "keys",
    "value",
    "inputs"
  )
  for (col in required_cols) {
    if (!col %in% colnames(nodes)) nodes[[col]] <- NA
  }

  # Ensure the dynamic color_by column exists. Prefer an existing 'color_by' column if present.
  if (!color_by %in% colnames(nodes)) {
    if ("color_by" %in% colnames(nodes)) {
      nodes[[color_by]] <- nodes[["color_by"]]
    } else {
      nodes[[color_by]] <- NA
    }
  }

  nodes <- nodes %>%
    dplyr::distinct(.data$name, .keep_all = TRUE) %>%
    dplyr::transmute(
      id = .data$name,
      color = color_pal[.data[[color_by]]],
      color_by = .data[[color_by]],
      grouping = ifelse(is.na(.data$module), .data$type, .data$module),
      expression = ifelse(
        .data$type == "in_node",
        ifelse(
          is.na(.data$keys),
          "user",
          ifelse(is.na(.data$mc_func), "mcdata", .data$mc_func)
        ),
        .data$node_exp
      ),
      title = generate_node_title(
        .data$name,
        .data$grouping,
        .data$value,
        .data$expression,
        .data$exp_param,
        .data$inputs
      ),
      type = .data$type
    )

  if (!color_by %in% names(nodes)) {
    nodes[[color_by]] <- nodes$color_by
  }

  return(nodes)
}

#' Generate Formatted visNetwork Edge Table
#'
#' Creates a formatted edge table suitable for visualisation with visNetwork.
#'
#' @param mcmodule (mcmodule object). Module containing node relationships.
#' @param inputs (logical). If TRUE, include non-node inputs. Default: FALSE.
#'
#' @return A data frame containing edge information for visNetwork with columns:
#'   from, to, and id.
visNetwork_edges <- function(mcmodule, inputs = FALSE) {
  get_edge_table(mcmodule = mcmodule, inputs = inputs) %>%
    transmute(
      from = .data$node_from,
      to = .data$node_to,
      id = row_number()
    )
}

#' Create Interactive Network Visualisation
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Generates an interactive network visualisation using visNetwork library.
#' The visualisation includes interactive features for exploring model structure
#' and relationships.
#'
#' By default, nodes are colored as:
#' \itemize{
#'   \item \strong{inputs} (light blue, #B0DFF9): Input datasets, data frames, files, and columns
#'   \item \strong{in_node} (blue, #6ABDEB): Input nodes and scalar values
#'   \item \strong{out_node} (green, #A4CF96): Output nodes
#'   \item \strong{filter} (light purple, #E8A5E5): Filtered nodes created with \code{mc_filter()}
#'   \item \strong{compare} (medium purple, #D88FD5): Comparison nodes created with \code{mc_compare()}
#'   \item \strong{trials_info} (light orange, #FAE4CB): Trial, subset, and related information nodes
#'   \item \strong{total} (orange, #F39200): Total nodes created with \code{at_least_one()}
#'   \item \strong{agg_total} (dark orange, #C17816): Aggregated total nodes created with \code{agg_totals()}
#' }
#'
#' @param mcmodule (mcmodule object). Module containing network to visualise.
#' @param variate (integer). Which variate to visualise. Default: 1.
#' @param color_pal (character vector, optional). Custom colour palette for nodes.
#'   Default: NULL.
#' @param color_by (character, optional). Column name to determine node colours.
#'   Default: NULL.
#' @param legend (logical). If TRUE, show colours legend. Default: FALSE.
#' @param inputs (logical). If TRUE, show non-node inputs. Default: FALSE.
#'
#' @return An interactive visNetwork object with highlighting of connected nodes,
#'   node selection and filtering by module, directional arrows, hierarchical
#'   layout, and draggable nodes.
#' @export
#' @examples
#' \donttest{
#' network <- mc_network(mcmodule=imports_mcmodule)
#' }
mc_network <- function(
  mcmodule,
  variate = 1,
  color_pal = NULL,
  color_by = NULL,
  legend = FALSE,
  inputs = FALSE
) {
  if (
    !all(
      requireNamespace("visNetwork", quietly = TRUE) &
        requireNamespace("igraph", quietly = TRUE)
    )
  ) {
    stop(
      "This function needs 'visNetwork' and 'igraph' packages.
    Install them using:
         install.packages(c('visNetwork','igraph'))"
    )
  }

  nodes <- visNetwork_nodes(
    mcmodule,
    variate = variate,
    color_pal = color_pal,
    color_by = color_by,
    inputs = inputs
  )
  edges <- visNetwork_edges(mcmodule, inputs = inputs)

  network <- visNetwork::visNetwork(nodes, edges, width = "100%") %>%
    visNetwork::visOptions(
      highlightNearest = list(enabled = TRUE, degree = 2),
      nodesIdSelection = TRUE,
      selectedBy = if (is.null(color_by)) "grouping" else color_by
    ) %>%
    visNetwork::visEdges(arrows = "to") %>%
    visNetwork::visIgraphLayout(
      layout = "layout_with_sugiyama",
      maxiter = 500
    ) %>%
    visNetwork::visPhysics(enabled = FALSE) %>%
    visNetwork::visInteraction(dragNodes = TRUE)

  if (legend) {
    color <- assign_color_pal(
      nodes = nodes,
      color_pal = color_pal,
      color_by = color_by,
      is_legend = TRUE
    )
    color_pal <- color[["pal"]]
    color_by <- color[["by"]]
    # passing custom nodes and/or edges
    lnodes <- data.frame(
      label = names(color_pal),
      color = color_pal,
      shape = "dot",
      title = "Node type",
      font.size = 15
    )
    network <- network %>%
      visNetwork::visLegend(
        addNodes = lnodes,
        useGroups = FALSE,
        ncol = ifelse(nrow(lnodes) > 5, 2, 1),
        zoom = FALSE
      )

    return(network)
  } else {
    return(network)
  }
}

# Helper functions
format_numeric_summary <- function(summary_value) {
  # Extract quantiles
  median_val <- signif_round(summary_value[["X50."]], 2)
  lower_val <- signif_round(summary_value[["X2.5."]], 2)
  upper_val <- signif_round(summary_value[["X97.5."]], 2)

  # Format string
  result <- paste0(median_val, " (", lower_val, "-", upper_val, ")")

  return(result)
}

format_percentage_summary <- function(summary_value) {
  # Extract and format percentages
  median_pct <- paste0(signif_round(summary_value[["X50."]] * 100, 2), "%")
  lower_pct <- paste0(signif_round(summary_value[["X2.5."]] * 100, 2), "%")
  upper_pct <- paste0(signif_round(summary_value[["X97.5."]] * 100, 2), "%")

  # Format string
  result <- paste0(median_pct, " (", lower_pct, "-", upper_pct, ")")

  return(result)
}

generate_node_title <- function(
  name,
  grouping,
  value,
  expression,
  exp_param,
  inputs
) {
  paste0(
    '<p style="text-align: center;"><strong><span style="font-size: 18px;"><u>',
    name,
    '</u><br></span></strong><span style="font-size: 12px;">',
    grouping,
    '</span></p>
    <p style="text-align: center;"><strong>',
    ifelse(is.na(value), "", value),
    "<br></strong>",
    ifelse(is.na(expression), "", expression),
    '</p>
    <table style="width: 100%; border-collapse: collapse; margin: 0px auto;">
      <tbody>
        <tr>
          <td style="width: 50%; text-align: center; background-color: rgb(239, 239, 239);"><strong>param</strong></td>
          <td style="width: 50%; text-align: center; background-color: rgb(239, 239, 239);"><strong>inputs</strong></td>
        </tr>
        <tr>
          <td style="width: 50%; text-align: center;">',
    gsub(",", "<br>", ifelse(is.na(exp_param), "", exp_param)),
    '</td>
          <td style="width: 50%; text-align: center;">',
    gsub(",", "<br>", ifelse(is.na(inputs), "", inputs)),
    "<br></td>
        </tr>
      </tbody>
    </table>"
  )
}

# Default color palette if none provided
default_color_pal <- c(
  input_dataset = "#B0DFF9",
  input_data = "#B0DFF9",
  input_file = "#B0DFF9",
  inputs_col = "#B0DFF9",
  scalar = "#6ABDEB",
  in_node = "#6ABDEB",
  out_node = "#A4CF96",
  filter = "#E8A5E5",
  compare = "#D88FD5",
  trials_n = "#FAE4CB",
  subsets_n = "#FAE4CB",
  subsets_p = "#FAE4CB",
  total = "#F39200",
  agg_total = "#C17816"
)

default_color_legend <- c(
  inputs = "#B0DFF9",
  in_node = "#6ABDEB",
  out_node = "#A4CF96",
  filter = "#E8A5E5",
  compare = "#D88FD5",
  trials_info = "#FAE4CB",
  total = "#F39200",
  agg_total = "#C17816"
)

assign_color_pal <- function(nodes, color_pal, color_by, is_legend = FALSE) {
  # Assign color by selected node table column
  if (is.null(color_by)) {
    # Default to coloring by "type" if no color_by specified
    color_by <- "type"
    color_levels <- levels(as.factor(nodes[[color_by]]))

    # Assign colors if palette was not provided
    if (is.null(color_pal)) {
      color_pal <- if (is_legend) default_color_legend else default_color_pal
      color_pal <- if (is_legend) {
        color_pal[color_pal %in% nodes$color]
      } else {
        color_pal[names(color_pal) %in% color_levels]
      }
    } else {
      color_pal <- color_pal[1:length(color_levels)]
      names(color_pal) <- color_levels
    }
  } else {
    # Use provided color_by column
    color_levels <- levels(as.factor(nodes[[color_by]]))

    if (is.null(color_pal)) {
      # Default color palette
      color_pal <- default_color_legend
      color_pal <- color_pal[1:length(color_levels)]
      names(color_pal) <- color_levels
    } else if (is.null(names(color_pal))) {
      # Default color mapping
      color_pal <- color_pal[1:length(color_levels)]
      names(color_pal) <- color_levels
    } else {
      # Use provided color mapping
      color_pal <- color_pal[color_levels]
    }
  }

  return(list(pal = color_pal, by = color_by))
}
