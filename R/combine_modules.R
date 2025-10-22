#' Combine Two Modules
#'
#' @description
#' Combines two mcmodules into a single mcmodule by merging their data and components.
#'
#' @param mcmodule_x First module to combine
#' @param mcmodule_y Second module to combine
#'
#' @return A combined mcmodule object
#'
#' @examples
#' module_x <- list(
#'   data = list(data_x = data.frame(x = 1:3)),
#'   node_list = list(
#'     node1 = list(type = "in_node"),
#'     node2 = list(type = "out_node")
#'   ),
#'   modules = c("module_x"),
#'   exp = quote({node2 <- node1 * 2})
#' )
#'
#' module_y <- list(
#'   data = list(data_y = data.frame(y = 4:6)),
#'   node_list = list(node3 = list(type = "out_node")),
#'   modules = c("module_y"),
#'   exp = quote({node3 <- node1 + node2})
#' )
#'
#' module_xy <- combine_modules(module_x, module_y)
#'
#' @export
combine_modules <- function(mcmodule_x, mcmodule_y) {
  mcmodule <- list()

  # Extract names of input modules
  name_x <- deparse(substitute(mcmodule_x))
  name_y <- deparse(substitute(mcmodule_y))

  # Combine data based on structure
  if (identical(mcmodule_x$data, mcmodule_y$data)) {
    mcmodule$data <- mcmodule_x$data
  } else {
    mcmodule$data <- unique(c(mcmodule_x$data, mcmodule_y$data))
    names(mcmodule$data)<-unique(names(c(mcmodule_x$data, mcmodule_y$data)))
  }

  # Combine model expressions
  mcmodule$exp <- list(
    mcmodule_x$exp,
    mcmodule_y$exp
  )
  names(mcmodule$exp) <- c(name_x, name_y)

  # Combine node lists and modules
  mcmodule$node_list <- c(mcmodule_x$node_list, mcmodule_y$node_list)
  mcmodule$modules <- unique(c(mcmodule_x$modules, mcmodule_y$modules))

  # Set class and return
  class(mcmodule) <- "mcmodule"
  return(mcmodule)
}
