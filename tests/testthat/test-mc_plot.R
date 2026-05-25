suppressMessages({
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

  test_that("mc_plot works with mc_filter nodes", {
    skip_if_not_installed("ggplot2")

    # Create test module
    test_module <- list(
      node_list = list(
        p1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.3, 0.4), type = "0", nvariates = 4),
            max = mcdata(c(0.2, 0.3, 0.4, 0.5), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "test_data",
          keys = c("category", "region")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "A", "B"),
          region = c("North", "North", "South", "South"),
          scenario_id = c("0", "0", "0", "0")
        )
      )
    )

    # Create filtered node
    filtered_module <- mc_filter(
      test_module,
      "p1",
      category == "A",
      name = "p1_A"
    )

    # Test basic plot with filtered node
    p <- mc_plot(filtered_module, "p1_A_filtered")
    expect_s3_class(p, "gg")
    expect_s3_class(p, "ggplot")

    # Test plot with color_by on filtered node
    p_color <- mc_plot(filtered_module, "p1_A_filtered", color_by = "region")
    expect_s3_class(p_color, "gg")

    # Test plot with order_by on filtered node
    p_order <- mc_plot(filtered_module, "p1_A_filtered", order_by = "median")
    expect_s3_class(p_order, "gg")

    # Verify that filtered plot only shows filtered variates
    tidy_data <- tidy_mcnode(filtered_module, "p1_A_filtered")
    expect_true(all(tidy_data$category == "A"))
  })

  test_that("mc_plot works with mc_compare nodes", {
    skip_if_not_installed("ggplot2")

    # Create test module with comparison
    test_module <- list(
      node_list = list(
        p1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.1, 0.2), type = "0", nvariates = 4),
            max = mcdata(c(0.2, 0.3, 0.2, 0.3), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "test_data",
          keys = c("category")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1")
        )
      )
    )

    # Create comparison node
    compared_module <- mc_compare(
      test_module,
      "p1",
      baseline = "0",
      type = "difference",
      name = "p1_diff"
    )

    # Test basic plot with compared node
    p <- mc_plot(compared_module, "p1_diff_compared")
    expect_s3_class(p, "gg")
    expect_s3_class(p, "ggplot")

    # Test plot with color_by on compared node
    p_color <- mc_plot(
      compared_module,
      "p1_diff_compared",
      color_by = "category"
    )
    expect_s3_class(p_color, "gg")

    # Test plot with threshold (useful for comparing against zero)
    p_threshold <- mc_plot(compared_module, "p1_diff_compared", threshold = 0)
    expect_s3_class(p_threshold, "gg")

    # Verify tidy data only shows what-if scenarios (not baseline)
    tidy_data <- tidy_mcnode(compared_module, "p1_diff_compared")
    expect_true(is.data.frame(tidy_data))
    expect_true("value" %in% names(tidy_data))
  })

  test_that("mcmodule_tornado works with corr_results input", {
    skip_if_not_installed("ggplot2")

    corr_results <- mcmodule_corr(
      imports_mcmodule,
      print_summary = FALSE,
      progress = FALSE
    )

    p <- mcmodule_tornado(corr_results = corr_results)
    expect_s3_class(p, "gg")
    expect_s3_class(p, "ggplot")

    p_no_colour <- mcmodule_tornado(
      corr_results = corr_results,
      colour = FALSE
    )
    expect_s3_class(p_no_colour, "gg")
  })

  test_that("mcmodule_tornado works with mcmodule input", {
    skip_if_not_installed("ggplot2")

    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    p <- mcmodule_tornado(
      mcmodule = test_module,
      print_summary = FALSE,
      progress = FALSE
    )

    expect_s3_class(p, "gg")
    expect_s3_class(p, "ggplot")
  })

  test_that("mcmodule_tornado validates inputs", {
    skip_if_not_installed("ggplot2")

    expect_error(
      mcmodule_tornado(),
      "Provide either mcmodule or corr_results"
    )

    expect_error(
      mcmodule_tornado(corr_results = data.frame(value = 0.1)),
      "missing required columns"
    )

    expect_error(
      mcmodule_tornado(
        corr_results = data.frame(input = c("a", "b"), value = c(NA, NA))
      ),
      "No non-missing correlation values"
    )
  })
})
