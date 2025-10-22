#' Add Group IDs to Data Frames
#'
#' @param x First dataset
#' @param by Grouping variables
#' @param y Optional second dataset
#' @return Dataframe or list of dataframes with added group IDs
#' @import dplyr

add_group_id <- function(x, y = NULL, by = NULL) {
  if (!is.null(y)) {
    if (is.null(by)) {
      # Get categorical variables for each dataframe
      cat_x <- names(x)[vapply(x, function(col) is.character(col) | is.factor(col), logical(1))]
      cat_y <- names(y)[vapply(y, function(col) is.character(col) | is.factor(col), logical(1))]

      # Find intersection of categorical variables
      by <- intersect(cat_x, cat_y)
      by <- by[!by %in% c("g_id", "g_row", "scenario_id")]
      message("Group by: ", paste(by, collapse = ", "))
    }

    if (!all(by %in% names(x))) {
      stop(paste0(paste(by[!by %in% names(x)]), " columns not found in ", deparse(substitute(x)),"\n"))
    }
    if (!all(by %in% names(y))) {
      stop(paste0(paste(by[!by %in% names(y)]), " columns not found in ", deparse(substitute(y)),"\n"))
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
    x_result<-x_result[, !duplicated(names(x_result))]
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
    y_result<-y_result[, !duplicated(names(y_result))]
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

    x_result <- dplyr::mutate(x_grouped,
                              g_id = dplyr::cur_group_id(),
                              g_row = dplyr::cur_group_rows())
    x_result <- dplyr::relocate(x_result, "g_id", "g_row")
    x_result <- dplyr::ungroup(x_result)

    return(x_result)
  }
}


#' Match and align keys between two datasets
#'
#' @param x First dataset containing keys to match
#' @param y Second dataset containing keys to match
#' @param keys_names Names of columns to use as matching keys. If NULL, uses common columns
#' @return List containing:
#'   \item{x}{First dataset with group IDs}
#'   \item{y}{Second dataset with group IDs}
#'   \item{xy}{Matched datasets with aligned group and scenario IDs}
#' @import dplyr
keys_match <- function(x, y, keys_names = NULL) {
  # Add common group ids
  keys_list <- add_group_id(x, y, keys_names)

  # Define keys_names if not provided
  if (is.null(keys_names)) {
    # Get categorical variables for each dataframe
    cat_x <- names(x)[vapply(x, function(col) is.character(col) | is.factor(col), logical(1))]
    cat_y <- names(y)[vapply(y, function(col) is.character(col) | is.factor(col), logical(1))]

    # Find intersection of categorical variables
    keys_names <- unique(intersect(cat_x, cat_y))
  }

  #Exclude special columns from keys_names
  keys_names <- keys_names[!keys_names %in% c("g_id", "g_row", "scenario_id")]

  #Get x and y keys dataframes
  keys_x <- keys_list$x[c("g_id", "g_row", "scenario_id", keys_names)]
  keys_y <- keys_list$y[c("g_id", "g_row", "scenario_id", keys_names)]

  # Group and scenario matching
  keys_xy <- dplyr::full_join(
    keys_x,
    keys_y,
    by = c("g_id", "scenario_id", keys_names)
  )
  keys_xy <- dplyr::relocate(keys_xy, "g_id", "scenario_id", dplyr::all_of(keys_names))

  # Get group ids for baseline scenario (scenario_id = 0)
  temp_xy_0 <- dplyr::full_join(
    keys_xy,
    keys_y,
    by = c("g_id", "scenario_id", keys_names)
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

  # Remove duplicates
  keys_xy <- dplyr::distinct(keys_xy)

  return(list(
    x = keys_x,
    y = keys_y,
    xy = keys_xy
  ))
}
