test_that("tidy_mcnode works with mcmodule", {
  test_module <- list(
    node_list = list(
      p1 = list(
        mcnode = mcstoc(
          runif,
          min = mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3),
          max = mcdata(c(0.2, 0.3, 0.4), type = "0", nvariates = 3),
          nvariates = 3
        ),
        data_name = "test_data",
        keys = c("category")
      )
    ),
    data = list(
      test_data = data.frame(
        category = c("A", "B", "C"),
        scenario_id = c("0", "0", "0")
      )
    )
  )

  # Test basic tidy_mcnode from mcmodule
  result <- tidy_mcnode(test_module, "p1")
  expect_true(is.data.frame(result))
  expect_true(all(c("category", "variate", "value") %in% names(result)))
  # Should have 3 rows (category A, B, C) * 1001 iterations each = 3003
  expect_equal(nrow(result), 3 * 1001)

  # Test tidy_mcnode from data
  result_data <- tidy_mcnode(
    mcnode = test_module$node_list$p1$mcnode,
    data = test_module$data$test_data
  )
  expect_true(is.data.frame(result_data))
  expect_true("row_id" %in% names(result_data))

  # Test with keys_names parameter
  result_keys <- tidy_mcnode(test_module, "p1", keys_names = c("category"))
  expect_true("category" %in% names(result_keys))
  expect_false("scenario_id" %in% names(result_keys))

  # Note: tidy_mcnode always returns all variates, sampling happens in mc_plot
})

test_that("mc_plot works with mcmodule", {
  skip_if_not_installed("ggplot2")

  test_module <- list(
    node_list = list(
      p1 = list(
        mcnode = mcstoc(
          runif,
          min = mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3),
          max = mcdata(c(0.2, 0.3, 0.4), type = "0", nvariates = 3),
          nvariates = 3
        ),
        data_name = "test_data",
        keys = c("category")
      )
    ),
    data = list(
      test_data = data.frame(
        category = c("A", "B", "C"),
        scenario_id = c("0", "0", "0")
      )
    )
  )

  # Test basic plot
  p <- mc_plot(test_module, "p1")
  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")

  # Test plot with color_by
  p_color <- mc_plot(test_module, "p1", color_by = "category")
  expect_s3_class(p_color, "gg")

  # Test plot with order_by median
  p_order <- mc_plot(test_module, "p1", order_by = "median")
  expect_s3_class(p_order, "gg")

  # Test plot with threshold
  p_threshold <- mc_plot(test_module, "p1", threshold = 0.2)
  expect_s3_class(p_threshold, "gg")

  # Test plot with scale
  p_scale <- mc_plot(test_module, "p1", scale = "log10")
  expect_s3_class(p_scale, "gg")

  # Test plot with custom color palette
  custom_pal <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF")
  p_custom <- mc_plot(
    test_module,
    "p1",
    color_by = "category",
    color_pal = custom_pal
  )
  expect_s3_class(p_custom, "gg")

  # Test plot with max_dots parameter (sampling occurs in mc_plot)
  p_sampled <- mc_plot(test_module, "p1", max_dots = 50)
  expect_s3_class(p_sampled, "gg")
})

test_that("mc_plot works with mcnode and data", {
  skip_if_not_installed("ggplot2")

  mcnode <- mcstoc(
    runif,
    min = mcdata(c(0.1, 0.2), type = "0", nvariates = 2),
    max = mcdata(c(0.3, 0.4), type = "0", nvariates = 2),
    nvariates = 2
  )

  data <- data.frame(
    category = c("X", "Y")
  )

  # Test plot with mcnode and data
  p <- mc_plot(mcnode = mcnode, data = data)
  expect_s3_class(p, "gg")
})

test_that("tidy_mcnode handles errors correctly", {
  test_module <- list(
    node_list = list(
      p1 = list(
        mcnode = mcstoc(
          runif,
          min = mcdata(c(0.1, 0.2), type = "0", nvariates = 2),
          max = mcdata(c(0.2, 0.3), type = "0", nvariates = 2),
          nvariates = 2
        ),
        data_name = "test_data",
        keys = c("category")
      )
    ),
    data = list(
      test_data = data.frame(
        category = c("A", "B")
      )
    )
  )

  # Test error when neither mcmodule nor data provided
  expect_error(tidy_mcnode(mc_name = "p1"))

  # Test error when keys_names not in data
  expect_error(tidy_mcnode(test_module, "p1", keys_names = c("nonexistent")))
})
