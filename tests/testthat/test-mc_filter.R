suppressMessages({
  library(mc2d)
  test_that("mc_filter works with mcmodule", {
    # Create test module
    test_module <- list(
      node_list = list(
        p_1 = list(
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

    # Test basic filtering - single condition
    result <- mc_filter(
      test_module,
      "p_1",
      category == "A",
      name = "p_1_A"
    )

    # Check that new filtered node exists
    expect_true("p_1_A_filtered" %in% names(result$node_list))

    # Check dimensions - should have 2 variates (2 "A" categories)
    expect_equal(dim(result$node_list$p_1_A_filtered$mcnode)[3], 2)

    # Check node attributes
    expect_equal(result$node_list$p_1_A_filtered$type, "filter")
    expect_equal(result$node_list$p_1_A_filtered$param, "p_1")
    expect_equal(result$node_list$p_1_A_filtered$inputs, "p_1")
    expect_true(grepl(
      "category == \"A\"",
      result$node_list$p_1_A_filtered$description
    ))

    # Check summary exists and has correct rows
    expect_equal(nrow(result$node_list$p_1_A_filtered$summary), 2)
    expect_true(all(result$node_list$p_1_A_filtered$summary$category == "A"))
  })

  test_that("mc_filter works with multiple conditions", {
    test_module <- list(
      node_list = list(
        p_1 = list(
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

    # Filter with multiple conditions
    result <- mc_filter(
      test_module,
      "p_1",
      category == "A",
      region == "North",
      name = "p_1_A_North"
    )

    # Should have only 1 variate
    expect_equal(dim(result$node_list$p_1_A_North_filtered$mcnode)[3], 1)

    # Check description contains both conditions
    expect_true(grepl(
      "category == \"A\"",
      result$node_list$p_1_A_North_filtered$filter_conditions
    ))
    expect_true(grepl(
      "region == \"North\"",
      result$node_list$p_1_A_North_filtered$filter_conditions
    ))
  })

  test_that("mc_filter works with data and mcnode directly", {
    # Create test data and mcnode
    test_data <- data.frame(
      category = c("A", "B", "C"),
      scenario_id = c("0", "0", "0")
    )

    test_mcnode <- mcstoc(
      runif,
      min = mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3),
      max = mcdata(c(0.2, 0.3, 0.4), type = "0", nvariates = 3),
      nvariates = 3
    )

    # Filter without mcmodule - should return raw mcnode
    # Note: filter conditions must come before named arguments
    result <- mc_filter(
      category == "B",
      data = test_data,
      mcnode = test_mcnode
    )

    # Result should be an mcnode, not an mcmodule
    expect_true(is.mcnode(result))

    # Should have 1 variate
    expect_equal(dim(result)[3], 1)
  })

  test_that("mc_filter handles custom naming options", {
    test_module <- list(
      node_list = list(
        p_1 = list(
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

    # Test with custom name only
    result1 <- mc_filter(test_module, "p_1", category == "A", name = "custom")
    expect_true("custom_filtered" %in% names(result1$node_list))

    # Test with custom filter_suffix
    result2 <- mc_filter(
      test_module,
      "p_1",
      category == "A",
      filter_suffix = "subset"
    )
    expect_true("p_1_subset" %in% names(result2$node_list))

    # Test with empty filter_suffix
    result3 <- mc_filter(
      test_module,
      "p_1",
      category == "A",
      name = "exact_name",
      filter_suffix = ""
    )
    expect_true("exact_name" %in% names(result3$node_list))

    # Test with prefix
    result4 <- mc_filter(test_module, "p_1", category == "A", prefix = "pre")
    expect_true("pre_p_1_filtered" %in% names(result4$node_list))
    expect_equal(result4$node_list$pre_p_1_filtered$prefix, "pre")
  })

  test_that("mc_filter preserves keys from original node", {
    test_module <- list(
      node_list = list(
        p_1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3),
            max = mcdata(c(0.2, 0.3, 0.4), type = "0", nvariates = 3),
            nvariates = 3
          ),
          data_name = "test_data",
          keys = c("category", "type")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "C"),
          type = c("X", "Y", "Z"),
          scenario_id = c("0", "0", "0")
        )
      )
    )

    result <- mc_filter(test_module, "p_1", category == "A")

    # Keys should be preserved
    expect_equal(result$node_list$p_1_filtered$keys, c("category", "type"))
  })

  test_that("mc_filter works without summary", {
    test_module <- list(
      node_list = list(
        p_1 = list(
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

    result <- mc_filter(test_module, "p_1", category == "A", summary = FALSE)

    # Summary should not exist
    expect_null(result$node_list$p_1_filtered$summary)
  })

  test_that("mc_filter handles edge cases", {
    test_module <- list(
      node_list = list(
        p_1 = list(
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

    # Filter that results in zero rows
    expect_warning(
      mc_filter(test_module, "p_1", category == "Z"),
      "Filter conditions resulted in zero rows"
    )

    # Filter that keeps all rows
    result <- mc_filter(test_module, "p_1", scenario_id == "0")
    expect_equal(dim(result$node_list$p_1_filtered$mcnode)[3], 3)
  })

  test_that("mc_filter error handling", {
    test_module <- list(
      node_list = list(
        p_1 = list(
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

    # No filter conditions provided
    expect_error(
      mc_filter(test_module, "p_1"),
      "At least one filter condition must be provided"
    )

    # Missing data when mcmodule is NULL
    expect_error(
      mc_filter(category == "A", mcnode = test_module$node_list$p_1$mcnode),
      "mcmodule or data must be provided"
    )

    # Missing mcnode when mcmodule is NULL
    expect_error(
      mc_filter(category == "A", data = test_module$data$test_data),
      "mcnode must be provided when mcmodule is NULL"
    )

    # Node not found in mcmodule
    expect_error(
      mc_filter(test_module, "nonexistent_node", category == "A"),
      "must be a mcnode present in"
    )
  })

  test_that("mc_filter works with numeric comparisons", {
    test_module <- list(
      node_list = list(
        p_1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.3, 0.4), type = "0", nvariates = 4),
            max = mcdata(c(0.2, 0.3, 0.4, 0.5), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "test_data",
          keys = c("category")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "C", "D"),
          value = c(10, 20, 30, 40),
          scenario_id = c("0", "0", "0", "0")
        )
      )
    )

    # Filter with numeric comparison
    result <- mc_filter(test_module, "p_1", value > 15, name = "high_value")

    # Should keep 3 rows (20, 30, 40)
    expect_equal(dim(result$node_list$high_value_filtered$mcnode)[3], 3)

    # Filter with multiple numeric conditions
    result2 <- mc_filter(test_module, "p_1", value >= 20, value <= 30)
    expect_equal(dim(result2$node_list$p_1_filtered$mcnode)[3], 2)
  })

  test_that("mc_filter works with %in% operator", {
    test_module <- list(
      node_list = list(
        p_1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.3, 0.4, 0.5), type = "0", nvariates = 5),
            max = mcdata(c(0.2, 0.3, 0.4, 0.5, 0.6), type = "0", nvariates = 5),
            nvariates = 5
          ),
          data_name = "test_data",
          keys = c("category")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "C", "D", "E"),
          scenario_id = c("0", "0", "0", "0", "0")
        )
      )
    )

    # Filter with %in%
    result <- mc_filter(test_module, "p_1", category %in% c("A", "C", "E"))

    # Should keep 3 rows
    expect_equal(dim(result$node_list$p_1_filtered$mcnode)[3], 3)
    expect_true(all(
      result$node_list$p_1_filtered$summary$category %in% c("A", "C", "E")
    ))
  })

  test_that("mc_filter handles single variate result", {
    test_module <- list(
      node_list = list(
        p_1 = list(
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

    # Filter to single result
    result <- mc_filter(test_module, "p_1", category == "B")

    # Should have exactly 1 variate
    expect_equal(dim(result$node_list$p_1_filtered$mcnode)[3], 1)
    expect_true(is.mcnode(result$node_list$p_1_filtered$mcnode))
  })

  test_that("mc_filter metadata is correctly set", {
    test_module <- list(
      node_list = list(
        p_1 = list(
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
          category = c("A", "B"),
          scenario_id = c("0", "0")
        )
      )
    )

    result <- mc_filter(
      test_module,
      "p_1",
      category == "A",
      name = "filtered_A"
    )

    # Check all metadata fields
    expect_equal(result$node_list$filtered_A_filtered$type, "filter")
    expect_equal(result$node_list$filtered_A_filtered$param, "p_1")
    expect_equal(result$node_list$filtered_A_filtered$inputs, "p_1")
    expect_true(!is.null(result$node_list$filtered_A_filtered$description))
    expect_true(!is.null(result$node_list$filtered_A_filtered$node_expression))
    expect_true(
      !is.null(result$node_list$filtered_A_filtered$filter_conditions)
    )
    expect_equal(result$node_list$filtered_A_filtered$data_name, "test_data")
  })

  test_that("mc_filter works with complex dplyr expressions", {
    test_module <- list(
      node_list = list(
        p_1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.3, 0.4), type = "0", nvariates = 4),
            max = mcdata(c(0.2, 0.3, 0.4, 0.5), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "test_data",
          keys = c("category")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "C", "D"),
          value = c(10, 20, 30, 40),
          scenario_id = c("0", "0", "0", "0")
        )
      )
    )

    # Filter with complex expression
    result <- mc_filter(
      test_module,
      "p_1",
      category != "D",
      value <= 25
    )

    # Should keep rows with category != "D" AND value <= 25
    # That's A (10) and B (20)
    expect_equal(dim(result$node_list$p_1_filtered$mcnode)[3], 2)
  })
})
