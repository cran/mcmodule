test_that("combine_modules works", {
  # Create test mcmodules
  module_x <- list(
    data = list(data_x=data.frame(x = 1:3)),
    node_list = list(node1 = list(type = "in_node"),
                     node2 = list(type = "out_node")),
    modules = c("module_x"),
    model_expression = quote({node2 <- node1 * 2})
  )

  module_y <- list(
    data = list(data_y=data.frame(y = 4:6)),
    node_list = list(node3 = list(type = "out_node")),
    modules = c("module_y"),
    model_expression = quote({node3 <- node1 + node2})

  )

  # Test combination
  result <- combine_modules(module_x, module_y)

  expect_type(result, "list")
  expect_equal(names(result$data), c("data_x","data_y"))
  expect_equal(names(result$node_list), c("node1","node2","node3"))
  expect_equal(result$modules, c("module_x","module_y"))
})
