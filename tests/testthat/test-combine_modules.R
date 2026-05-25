test_that("combine_modules works", {
  # Create test mcmodules
  module_x <- list(
    data = list(data_x = data.frame(x = 1:3)),
    node_list = list(
      node1 = list(type = "in_node"),
      node2 = list(type = "out_node")
    ),
    exp = quote({
      node2 <- node1 * 2
    })
  )

  module_y <- list(
    data = list(data_y = data.frame(y = 4:6)),
    node_list = list(node3 = list(type = "out_node")),
    exp = quote({
      node3 <- node1 + node2
    })
  )

  # Test combination
  result <- combine_modules(module_x, module_y)

  expect_type(result, "list")
  expect_equal(names(result$data), c("data_x", "data_y"))
  expect_equal(names(result$node_list), c("node1", "node2", "node3"))
})


test_that("mcmodule_info handles combined modules", {
  data_ab = data.frame(key1 = c("x", "y"))
  module_a <- list(
    exp = list(
      exp_a = quote({
        a <- 1
      })
    ),
    node_list = list(
      a = list(
        type = "out_node",
        exp_name = "exp_a",
        data_name = "data_ab",
        keys = c("key1")
      )
    ),
    data = list(data_ab = data_ab)
  )
  class(module_a) <- "mcmodule"

  result <- mcmodule_info(module_a)

  expect_equal(result$is_combined, FALSE)
  expect_equal(result$n_modules, 1)
  expect_equal(result$module_names, "module_a")
  expect_s3_class(result$module_exp_data, "data.frame")
  expect_equal(nrow(result$module_exp_data), 1)
  expect_equal(result$module_exp_data$module, "module_a")
  expect_equal(result$module_exp_data$exp, "exp_a")
  expect_equal(result$global_keys, "key1")

  module_b <- list(
    exp = list(
      exp_b1 = quote({
        b1 <- 2
      }),
      exp_b2 = quote({
        b2 <- b1 + 3
      })
    ),
    node_list = list(
      b1 = list(
        type = "out_node",
        exp_name = "exp_b1",
        data_name = "data_ab",
        keys = c("key1")
      ),
      b2 = list(
        type = "out_node",
        exp_name = "exp_b2",
        data_name = "data_ab",
        keys = c("key1")
      )
    ),
    data = list(data_ab = data_ab)
  )
  class(module_b) <- "mcmodule"

  combined_ab <- combine_modules(module_a, module_b)

  result <- mcmodule_info(combined_ab)

  expect_equal(result$n_modules, 2)
  expect_equal(result$is_combined, TRUE)
  expect_equal(result$module_names, c("module_a", "module_b"))
  expect_s3_class(result$module_exp_data, "data.frame")
  expect_equal(nrow(result$module_exp_data), 3) # 1 from module_a + 2 from module_b
  expect_equal(
    result$module_exp_data$module,
    c("module_a", "module_b", "module_b")
  )
  expect_equal(result$module_exp_data$exp, c("exp_a", "exp_b1", "exp_b2"))

  data_c = data.frame(
    key1 = c("x", "x", "y", "y"),
    key2 = c("a", "b", "a", "b")
  )

  module_c <- list(
    exp = list(
      exp_c = quote({
        c <- a + b2
      })
    ),
    node_list = list(
      c = list(
        type = "out_node",
        exp_name = "exp_c",
        data_name = "data_c",
        keys = c("key1", "key2")
      )
    ),
    data = list(data_c = data_c)
  )
  class(module_c) <- "mcmodule"

  combined_abc <- combine_modules(combined_ab, module_c)
  result <- mcmodule_info(combined_abc)

  expect_equal(result$n_modules, 3)
  expect_equal(result$is_combined, TRUE)
  expect_equal(result$module_names, c("module_a", "module_b", "module_c"))
  expect_s3_class(result$module_exp_data, "data.frame")
  expect_equal(nrow(result$module_exp_data), 4) # 1 + 2 + 1
  expect_equal(
    result$module_exp_data$module,
    c("module_a", "module_b", "module_b", "module_c")
  )
  expect_equal(
    result$module_exp_data$exp,
    c("exp_a", "exp_b1", "exp_b2", "exp_c")
  )

  # node counts and traceability info should be present
  det <- mcmodule_info(combined_abc)
  expect_true(is.list(det))
  expect_true("module_exp_data" %in% names(det))
  # If node_counts available, it should be a data.frame with module names
  if ("node_counts" %in% names(det)) {
    expect_s3_class(det$node_counts, "data.frame")
    expect_true(all(det$node_counts$module %in% det$module_names))
  }
})
