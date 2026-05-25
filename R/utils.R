#' Add Group Identifiers to Data Frame
#'
#' Adds group IDs for matching and alignment between data frames.
#'
#' @param x (data frame). First dataset.
#' @param by (character vector, optional). Column names for grouping.
#'   If NULL, auto-detected from categorical columns. Default: NULL.
#' @param y (data frame, optional). Second dataset. If provided, aligns with `x`.
#'   Default: NULL.
#'
#' @return Data frame or list of data frames with added group identifiers (g_id, g_row).
#' @keywords internal

add_group_id <- function(x, y = NULL, by = NULL) {
  if (!is.null(y)) {
    if (is.null(by)) {
      # Get categorical variables for each dataframe
      cat_x <- names(x)[vapply(
        x,
        function(col) is.character(col) | is.factor(col),
        logical(1)
      )]
      cat_y <- names(y)[vapply(
        y,
        function(col) is.character(col) | is.factor(col),
        logical(1)
      )]

      # Find intersection of categorical variables
      by <- intersect(cat_x, cat_y)
      by <- by[!by %in% c("g_id", "g_row", "scenario_id")]
      message("Group by: ", paste(by, collapse = ", "))
    }

    if (!all(by %in% names(x))) {
      stop(paste0(
        paste(by[!by %in% names(x)]),
        " columns not found in ",
        deparse(substitute(x)),
        "\n"
      ))
    }
    if (!all(by %in% names(y))) {
      stop(paste0(
        paste(by[!by %in% names(y)]),
        " columns not found in ",
        deparse(substitute(y)),
        "\n"
      ))
    }

    x[["df"]] <- "x"
    y[["df"]] <- "y"

    xy <- dplyr::bind_rows(x[c(by, "df")], y[c(by, "df")])
    xy <- dplyr::mutate(xy, g_id = NULL, g_row = NULL)

    #Exclude special columns from group_vars
    group_vars <- by[!by %in% c("g_id", "g_row", "scenario_id")]

    xy <- dplyr::group_by(xy, dplyr::across(dplyr::all_of(group_vars)))
    xy <- dplyr::mutate(xy, g_id = dplyr::cur_group_id())
    x_filtered <- dplyr::filter(xy, .data$df == "x")
    x_cols <- names(x)[!names(x) %in% c(by, "df", "g_id", "g_row")]
    x_data <- x[x_cols]
    x_result <- dplyr::bind_cols(x_filtered, x_data)
    x_result <- x_result[, !duplicated(names(x_result))]
    x_result <- dplyr::mutate(x_result, df = NULL)
    x_result <- dplyr::mutate(x_result, g_row = dplyr::cur_group_rows())
    x_result <- dplyr::relocate(x_result, "g_id", "g_row")
    x_result <- dplyr::ungroup(x_result)

    # Add scenario_id column if missing
    if (!"scenario_id" %in% names(x_result)) {
      x_result[["scenario_id"]] <- "0"
    }

    # Same for y dataset
    y_filtered <- dplyr::filter(xy, .data$df == "y")
    y_cols <- names(y)[!names(y) %in% c(by, "df", "g_id", "g_row")]
    y_data <- y[y_cols]
    y_result <- dplyr::bind_cols(y_filtered, y_data)
    y_result <- y_result[, !duplicated(names(y_result))]
    y_result <- dplyr::mutate(y_result, df = NULL)
    y_result <- dplyr::mutate(y_result, g_row = dplyr::cur_group_rows())
    y_result <- dplyr::relocate(y_result, "g_id", "g_row")
    y_result <- dplyr::ungroup(y_result)

    # Add scenario_id column if missing
    if (!"scenario_id" %in% names(y_result)) {
      y_result[["scenario_id"]] <- "0"
    }

    return(list(x = x_result, y = y_result))
  } else {
    group_vars <- by
    x_grouped <- dplyr::group_by(x, dplyr::across(dplyr::all_of(group_vars)))

    x_result <- dplyr::mutate(
      x_grouped,
      g_id = dplyr::cur_group_id(),
      g_row = dplyr::cur_group_rows()
    )
    x_result <- dplyr::relocate(x_result, "g_id", "g_row")
    x_result <- dplyr::ungroup(x_result)

    return(x_result)
  }
}


#' Match and Align Keys Between Datasets
#'
#' Matches and aligns keys between two datasets for downstream operations.
#'
#' @param x First dataset containing keys to match
#' @param y Second dataset containing keys to match
#' @param keys_names Names of columns to use as matching keys. If NULL, uses common columns
#' @param match_scenario (logical). If TRUE, exclude scenario_id from matching keys.
#'   If FALSE, include scenario_id. Default: TRUE.
#' @return A list containing:
#'   \item{x}{First dataset with group IDs}
#'   \item{y}{Second dataset with group IDs}
#'   \item{xy}{Matched datasets with aligned group and scenario IDs}
#' @import dplyr
keys_match <- function(x, y, keys_names = NULL, match_scenario = TRUE) {
  # Add common group ids
  keys_list <- add_group_id(x, y, keys_names)

  # Define keys_names if not provided
  if (is.null(keys_names)) {
    # Get categorical variables for each dataframe
    cat_x <- names(x)[vapply(
      x,
      function(col) is.character(col) | is.factor(col),
      logical(1)
    )]
    cat_y <- names(y)[vapply(
      y,
      function(col) is.character(col) | is.factor(col),
      logical(1)
    )]

    # Find intersection of categorical variables
    keys_names <- unique(intersect(cat_x, cat_y))
  }

  #Exclude special columns from keys_names (conditionally exclude scenario_id)
  if (match_scenario) {
    keys_names <- keys_names[!keys_names %in% c("g_id", "g_row", "scenario_id")]
  } else {
    keys_names <- keys_names[!keys_names %in% c("g_id", "g_row")]
  }

  #Get x and y keys dataframes
  keys_x <- keys_list$x[unique(c("g_id", "g_row", "scenario_id", keys_names))]
  keys_y <- keys_list$y[unique(c("g_id", "g_row", "scenario_id", keys_names))]

  # If keys_x and keys_y are identical return them directly
  if (identical(as.data.frame(keys_x), as.data.frame(keys_y))) {
    keys_xy <- dplyr::mutate(
      keys_x,
      g_row.x = .data$g_row,
      g_row = NULL
    )
    keys_xy <- dplyr::bind_cols(
      keys_xy,
      dplyr::transmute(keys_y, g_row.y = .data$g_row)
    )
    return(list(x = keys_x, y = keys_y, xy = keys_xy))
  }

  # Group and scenario matching
  # When match_scenario=FALSE, exclude scenario_id from join to enable cross-scenario matching
  join_by_cols <- if (match_scenario) {
    unique(c("g_id", "scenario_id", keys_names))
  } else {
    # Cross-scenario matching: join only on g_id and non-scenario keys
    unique(c("g_id", setdiff(keys_names, "scenario_id")))
  }

  keys_xy <- dplyr::full_join(
    keys_x,
    keys_y,
    by = join_by_cols,
    relationship = "many-to-many"
  )

  # Handle scenario_id columns after cross-scenario join
  if (
    !match_scenario &&
      "scenario_id.x" %in% names(keys_xy) &&
      "scenario_id.y" %in% names(keys_xy)
  ) {
    # For cross-scenario matching, use y's scenario_id (whatif scenarios)
    keys_xy <- dplyr::mutate(
      keys_xy,
      scenario_id = .data$scenario_id.y
    )
    keys_xy$scenario_id.x <- NULL
    keys_xy$scenario_id.y <- NULL
  }

  keys_xy <- dplyr::relocate(
    keys_xy,
    "g_id",
    "scenario_id",
    dplyr::all_of(setdiff(keys_names, "scenario_id"))
  )

  # Baseline scenario filling logic - only applies when match_scenario=TRUE
  if (match_scenario) {
    # Get group ids for baseline scenario (scenario_id = 0)
    temp_xy_0 <- dplyr::full_join(
      keys_xy,
      keys_y,
      by = join_by_cols,
      relationship = "many-to-many"
    )

    temp_xy_0 <- dplyr::filter(temp_xy_0, .data$scenario_id == "0")
    keys_xy_0 <- dplyr::transmute(
      temp_xy_0,
      g_id = .data$g_id,
      g_row.x_0 = .data$g_row.x,
      g_row.y_0 = .data$g_row.y
    )

    # Fill in missing values using baseline scenario
    keys_xy <- dplyr::left_join(
      keys_xy,
      keys_xy_0,
      by = "g_id",
      relationship = "many-to-many"
    )

    g_row_x <- keys_xy$g_row.x
    g_row_x_0 <- keys_xy$g_row.x_0
    g_row_y <- keys_xy$g_row.y
    g_row_y_0 <- keys_xy$g_row.y_0

    keys_xy$g_row.x <- ifelse(is.na(g_row_x), g_row_x_0, g_row_x)
    keys_xy$g_row.x_0 <- NULL
    keys_xy$g_row.y <- ifelse(is.na(g_row_y), g_row_y_0, g_row_y)
    keys_xy$g_row.y_0 <- NULL
  }

  # Remove duplicates
  keys_xy <- dplyr::distinct(keys_xy)

  return(list(
    x = keys_x,
    y = keys_y,
    xy = keys_xy
  ))
}
