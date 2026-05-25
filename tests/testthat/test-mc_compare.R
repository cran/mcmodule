suppressMessages({
  library(mc2d)
  library(testthat)

  # Test 1: Basic mc_compare with difference type
  test_that("mc_compare works with basic difference comparison", {
    # Create test module with baseline and what-if scenarios
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(
              c(0.1, 0.2, 0.1, 0.2, 0.1, 0.2),
              type = "0",
              nvariates = 6
            ),
            max = mcdata(
              c(0.2, 0.3, 0.2, 0.3, 0.2, 0.3),
              type = "0",
              nvariates = 6
            ),
            nvariates = 6
          ),
          data_name = "test_data",
          keys = c("category")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1", "2", "2"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Run comparison
    result <- mc_compare(
      test_module,
      "p_test",
      baseline = "0",
      type = "difference",
      name = "p_diff"
    )

    # Check comparison node was created
    expect_true("p_diff_compared" %in% names(result$node_list))

    # Check node metadata
    expect_equal(result$node_list$p_diff_compared$type, "compare")
    expect_equal(result$node_list$p_diff_compared$baseline, "0")
    expect_equal(result$node_list$p_diff_compared$compare_type, "difference")
    expect_equal(result$node_list$p_diff_compared$param, "p_test")

    # Check dimensions - should have 4 what-if variates (2 from scenario 1, 2 from scenario 2)
    expect_equal(dim(result$node_list$p_diff_compared$mcnode)[3], 4)

    # Check summary was created
    expect_true(!is.null(result$node_list$p_diff_compared$summary))
    expect_equal(nrow(result$node_list$p_diff_compared$summary), 4)

    # Verify scenarios in summary
    expect_true(all(
      result$node_list$p_diff_compared$summary$scenario_id %in% c("1", "2")
    ))
    expect_false(any(
      result$node_list$p_diff_compared$summary$scenario_id == "0"
    ))
  })

  # Test 2: All four comparison types
  test_that("mc_compare works with all comparison types", {
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcdata(c(10, 20, 15, 25), type = "0", nvariates = 4),
          data_name = "test_data",
          keys = c("group")
        )
      ),
      data = list(
        test_data = data.frame(
          group = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Test difference
    result_diff <- mc_compare(
      test_module,
      "p_test",
      baseline = "0",
      type = "difference"
    )
    expect_true("p_test_compared" %in% names(result_diff$node_list))

    # Test relative_difference
    result_rel_diff <- mc_compare(
      test_module,
      "p_test",
      baseline = "0",
      type = "relative_difference",
      name = "rel_diff"
    )
    expect_true("rel_diff_compared" %in% names(result_rel_diff$node_list))

    # Test reduction
    result_red <- mc_compare(
      test_module,
      "p_test",
      baseline = "0",
      type = "reduction",
      name = "reduction"
    )
    expect_true("reduction_compared" %in% names(result_red$node_list))

    # Test relative_reduction
    result_rel_red <- mc_compare(
      test_module,
      "p_test",
      baseline = "0",
      type = "relative_reduction",
      name = "rel_red"
    )
    expect_true("rel_red_compared" %in% names(result_rel_red$node_list))
  })

  # Test 3: Missing baseline scenario (should error)
  test_that("mc_compare errors when baseline scenario doesn't exist", {
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcdata(c(10, 20), type = "0", nvariates = 2),
          data_name = "test_data",
          keys = c("group")
        )
      ),
      data = list(
        test_data = data.frame(
          group = c("A", "B"),
          scenario_id = c("1", "1"),
          stringsAsFactors = FALSE
        )
      )
    )

    expect_error(
      mc_compare(test_module, "p_test", baseline = "0", type = "difference"),
      "Baseline scenario '0' not found"
    )
  })

  # Test 4: No what-if scenarios (should error)
  test_that("mc_compare errors when no what-if scenarios exist", {
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcdata(c(10, 20), type = "0", nvariates = 2),
          data_name = "test_data",
          keys = c("group")
        )
      ),
      data = list(
        test_data = data.frame(
          group = c("A", "B"),
          scenario_id = c("0", "0"),
          stringsAsFactors = FALSE
        )
      )
    )

    expect_error(
      mc_compare(test_module, "p_test", baseline = "0", type = "difference"),
      "No what-if scenarios found"
    )
  })

  # Test 5: Missing what-if data (should interpret as baseline - no change)
  test_that("mc_compare handles missing what-if data correctly", {
    # Baseline has A, B, C
    # What-if scenario 1 has only A, B (missing C)
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.3, 0.1, 0.2), type = "0", nvariates = 5),
            max = mcdata(c(0.2, 0.3, 0.4, 0.2, 0.3), type = "0", nvariates = 5),
            nvariates = 5
          ),
          data_name = "test_data",
          keys = c("category")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "C", "A", "B"),
          scenario_id = c("0", "0", "0", "1", "1"),
          stringsAsFactors = FALSE
        )
      )
    )

    # This should work - missing C in scenario 1 is treated as zero change
    result <- mc_compare(
      test_module,
      "p_test",
      baseline = "0",
      type = "difference"
    )

    expect_true("p_test_compared" %in% names(result$node_list))

    # Should have 3 variates (A, B, C from scenario 1)
    # C will have zero values (no change from baseline)
    expect_equal(dim(result$node_list$p_test_compared$mcnode)[3], 3)
  })

  # Test 6: Missing baseline data (should error via check_baseline_keys)
  test_that("mc_compare errors when baseline is incomplete", {
    # Baseline has A, B
    # What-if scenario has A, B, C (extra C not in baseline)
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.1, 0.2, 0.3), type = "0", nvariates = 5),
            max = mcdata(c(0.2, 0.3, 0.2, 0.3, 0.4), type = "0", nvariates = 5),
            nvariates = 5
          ),
          data_name = "test_data",
          keys = c("category")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "A", "B", "C"),
          scenario_id = c("0", "0", "1", "1", "1"),
          stringsAsFactors = FALSE
        )
      )
    )

    # This should error because baseline doesn't have C
    expect_error(
      mc_compare(test_module, "p_test", baseline = "0", type = "difference"),
      "Baseline scenario.*is incomplete.*Missing key combinations"
    )
  })

  # Test 7: Division by zero handling
  test_that("mc_compare handles division by zero correctly", {
    # Create test with baseline = 0 for some values
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcdata(c(0, 10, 5, 15), type = "0", nvariates = 4),
          data_name = "test_data",
          keys = c("group")
        )
      ),
      data = list(
        test_data = data.frame(
          group = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Test relative_difference with division by zero
    result <- mc_compare(
      test_module,
      "p_test",
      baseline = "0",
      type = "relative_difference"
    )

    expect_true("p_test_compared" %in% names(result$node_list))

    # Check that NA/Inf values were replaced with 0
    comparison_values <- unmc(result$node_list$p_test_compared$mcnode)
    expect_false(any(is.na(comparison_values)))
    expect_false(any(is.infinite(comparison_values)))
  })

  # Test 8: Integration with mc_filter
  test_that("mc_compare works with filtered nodes", {
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(
              c(0.1, 0.2, 0.3, 0.1, 0.2, 0.3, 0.15, 0.25, 0.35),
              type = "0",
              nvariates = 9
            ),
            max = mcdata(
              c(0.2, 0.3, 0.4, 0.2, 0.3, 0.4, 0.25, 0.35, 0.45),
              type = "0",
              nvariates = 9
            ),
            nvariates = 9
          ),
          data_name = "test_data",
          keys = c("category", "region")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "C", "A", "B", "C", "A", "B", "C"),
          region = c(
            "North",
            "North",
            "North",
            "South",
            "South",
            "South",
            "North",
            "North",
            "North"
          ),
          scenario_id = c("0", "0", "0", "1", "1", "1", "1", "1", "1"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Filter to only North region
    filtered_module <- mc_filter(
      test_module,
      "p_test",
      region == "North",
      name = "p_north"
    )

    # Compare filtered node (North baseline vs North what-if scenarios)
    result <- mc_compare(
      filtered_module,
      "p_north_filtered",
      baseline = "0",
      type = "difference"
    )

    expect_true("p_north_filtered_compared" %in% names(result$node_list))

    # Should have 3 variates (one for each North category in what-if scenario 1)
    expect_equal(dim(result$node_list$p_north_filtered_compared$mcnode)[3], 3)
  })

  # Test 9: Custom naming with prefix and suffix
  test_that("mc_compare respects name, prefix, and suffix parameters", {
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcdata(c(10, 20, 15, 25), type = "0", nvariates = 4),
          data_name = "test_data",
          keys = c("group")
        )
      ),
      data = list(
        test_data = data.frame(
          group = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Test with custom name
    result1 <- mc_compare(test_module, "p_test", name = "custom")
    expect_true("custom_compared" %in% names(result1$node_list))

    # Test with custom suffix
    result2 <- mc_compare(test_module, "p_test", suffix = "_delta")
    expect_true("p_test_delta" %in% names(result2$node_list))

    # Test with prefix
    result3 <- mc_compare(test_module, "p_test", prefix = "analysis")
    expect_true("analysis_p_test_compared" %in% names(result3$node_list))

    # Test with empty suffix
    result4 <- mc_compare(test_module, "p_test", name = "my_comp", suffix = "")
    expect_true("my_comp" %in% names(result4$node_list))
  })

  # Test 10: Summary generation
  test_that("mc_compare generates summary correctly", {
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.1, 0.2), type = "0", nvariates = 4),
            max = mcdata(c(0.2, 0.3, 0.2, 0.3), type = "0", nvariates = 4),
            nvariates = 4,
            nsv = 1000
          ),
          data_name = "test_data",
          keys = c("category")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1"),
          stringsAsFactors = FALSE
        )
      )
    )

    # With summary
    result_with <- mc_compare(test_module, "p_test", summary = TRUE)
    expect_true(!is.null(result_with$node_list$p_test_compared$summary))
    expect_true(is.data.frame(result_with$node_list$p_test_compared$summary))
    expect_true(
      "mean" %in% names(result_with$node_list$p_test_compared$summary)
    )
    expect_true("sd" %in% names(result_with$node_list$p_test_compared$summary))

    # Without summary
    result_without <- mc_compare(
      test_module,
      "p_test",
      summary = FALSE,
      name = "no_sum"
    )
    expect_true(is.null(result_without$node_list$no_sum_compared$summary))
  })

  # Test 11: Multiple scenarios
  test_that("mc_compare works with multiple what-if scenarios", {
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcdata(
            c(10, 20, 15, 25, 12, 22, 18, 28),
            type = "0",
            nvariates = 8
          ),
          data_name = "test_data",
          keys = c("group")
        )
      ),
      data = list(
        test_data = data.frame(
          group = c("A", "B", "A", "B", "A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1", "2", "2", "3", "3"),
          stringsAsFactors = FALSE
        )
      )
    )

    result <- mc_compare(
      test_module,
      "p_test",
      baseline = "0",
      type = "difference"
    )

    # Should have 6 variates (2 groups × 3 scenarios)
    expect_equal(dim(result$node_list$p_test_compared$mcnode)[3], 6)

    # Check that all three scenarios are in the summary
    scenarios <- unique(result$node_list$p_test_compared$summary$scenario_id)
    expect_equal(length(scenarios), 3)
    expect_true(all(c("1", "2", "3") %in% scenarios))
    expect_false("0" %in% scenarios)
  })

  # Test 12: Invalid type parameter
  test_that("mc_compare errors with invalid type", {
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcdata(c(10, 20, 15, 25), type = "0", nvariates = 4),
          data_name = "test_data",
          keys = c("group")
        )
      ),
      data = list(
        test_data = data.frame(
          group = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1"),
          stringsAsFactors = FALSE
        )
      )
    )

    expect_error(
      mc_compare(test_module, "p_test", type = "invalid_type"),
      "Invalid type 'invalid_type'"
    )
  })

  # Test 13: Node not found error
  test_that("mc_compare errors when node doesn't exist", {
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcdata(c(10, 20), type = "0", nvariates = 2),
          data_name = "test_data",
          keys = c("group")
        )
      ),
      data = list(
        test_data = data.frame(
          group = c("A", "B"),
          scenario_id = c("0", "0"),
          stringsAsFactors = FALSE
        )
      )
    )

    expect_error(
      mc_compare(test_module, "nonexistent_node", type = "difference"),
      "nonexistent_node not found"
    )
  })

  # Test 14: Verify comparison formulas are correct
  test_that("mc_compare calculates comparison values correctly", {
    # Create deterministic test data
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcdata(c(10, 20, 15, 25), type = "0", nvariates = 4),
          data_name = "test_data",
          keys = c("group")
        )
      ),
      data = list(
        test_data = data.frame(
          group = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Test difference: 15 - 10 = 5, 25 - 20 = 5
    result_diff <- mc_compare(
      test_module,
      "p_test",
      type = "difference",
      name = "diff"
    )
    values_diff <- unmc(result_diff$node_list$diff_compared$mcnode)
    expect_equal(values_diff[1], 5)
    expect_equal(values_diff[2], 5)

    # Test relative_difference: (15-10)/10 = 0.5, (25-20)/20 = 0.25
    result_rel_diff <- mc_compare(
      test_module,
      "p_test",
      type = "relative_difference",
      name = "rel_diff"
    )
    values_rel_diff <- unmc(result_rel_diff$node_list$rel_diff_compared$mcnode)
    expect_equal(values_rel_diff[1], 0.5)
    expect_equal(values_rel_diff[2], 0.25)

    # Test reduction: 10 - 15 = -5, 20 - 25 = -5
    result_red <- mc_compare(
      test_module,
      "p_test",
      type = "reduction",
      name = "red"
    )
    values_red <- unmc(result_red$node_list$red_compared$mcnode)
    expect_equal(values_red[1], -5)
    expect_equal(values_red[2], -5)

    # Test relative_reduction: (10-15)/10 = -0.5, (20-25)/20 = -0.25
    result_rel_red <- mc_compare(
      test_module,
      "p_test",
      type = "relative_reduction",
      name = "rel_red"
    )
    values_rel_red <- unmc(result_rel_red$node_list$rel_red_compared$mcnode)
    expect_equal(values_rel_red[1], -0.5)
    expect_equal(values_rel_red[2], -0.25)
  })

  # Test 15: Compare works with aggregated totals nodes
  test_that("mc_compare works with agg_total nodes", {
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcdata(c(0.10, 0.20, 0.15, 0.25), type = "0", nvariates = 4),
          data_name = "test_data",
          keys = c("group")
        )
      ),
      data = list(
        test_data = data.frame(
          group = c("A", "A", "A", "A"),
          scenario_id = c("0", "0", "1", "1"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Create aggregated node by scenario_id so output has one variate per scenario
    test_module <- agg_totals(
      test_module,
      mc_name = "p_test",
      agg_keys = "scenario_id"
    )

    # Ensure aggregated node exists and carries summary
    expect_true("p_test_agg" %in% names(test_module$node_list))
    expect_equal(test_module$node_list$p_test_agg$type, "agg_total")
    expect_true(!is.null(test_module$node_list$p_test_agg$summary))

    # Compare aggregated what-if vs baseline
    result <- mc_compare(
      test_module,
      "p_test_agg",
      baseline = "0",
      type = "relative_reduction",
      name = "p_test_agg_rrr"
    )

    expect_true("p_test_agg_rrr_compared" %in% names(result$node_list))
    expect_equal(result$node_list$p_test_agg_rrr_compared$type, "compare")
    expect_true(!is.null(result$node_list$p_test_agg_rrr_compared$summary))
    expect_equal(nrow(result$node_list$p_test_agg_rrr_compared$summary), 1)
    expect_true(all(
      result$node_list$p_test_agg_rrr_compared$summary$scenario_id == "1"
    ))
  })

  # Test 16: align_uncertainty parameter works correctly
  test_that("mc_compare align_uncertainty parameter aligns uncertainty iterations", {
    set.seed(123)

    # Create test module with multivariate nodes with uncertainty
    test_module <- list(
      node_list = list(
        p_test = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.1, 0.2), type = "0", nvariates = 4),
            max = mcdata(c(0.2, 0.3, 0.2, 0.3), type = "0", nvariates = 4),
            nvariates = 4,
            nsv = 100
          ),
          data_name = "test_data",
          keys = c("category")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Test with align_uncertainty = TRUE (default)
    result_aligned <- mc_compare(
      test_module,
      "p_test",
      baseline = "0",
      type = "difference",
      align_uncertainty = TRUE,
      name = "aligned"
    )

    # Test with align_uncertainty = FALSE
    result_unaligned <- mc_compare(
      test_module,
      "p_test",
      baseline = "0",
      type = "difference",
      align_uncertainty = FALSE,
      name = "unaligned"
    )

    # Both should produce valid comparison nodes
    expect_true("aligned_compared" %in% names(result_aligned$node_list))
    expect_true("unaligned_compared" %in% names(result_unaligned$node_list))

    # Both should have same dimensions
    expect_equal(
      dim(result_aligned$node_list$aligned_compared$mcnode),
      dim(result_unaligned$node_list$unaligned_compared$mcnode)
    )

    # The results should be different due to alignment
    # (unless by chance they're identical, which is extremely unlikely)
    aligned_values <- unmc(result_aligned$node_list$aligned_compared$mcnode)
    unaligned_values <- unmc(
      result_unaligned$node_list$unaligned_compared$mcnode
    )

    # Check that not all values are identical
    expect_false(all(aligned_values == unaligned_values))
  })

  # Test 17: Regression - compare total node with multiple data_names
  test_that("mc_compare works for total node with multiple data_names", {
    module_1_data <- data.frame(
      group = c("A", "B", "A", "B"),
      scenario_id = c("0", "0", "1", "1"),
      p_mod1_x = c(0.20, 0.30, 0.10, 0.15),
      p_mod1_y = c(0.05, 0.10, 0.02, 0.03)
    )

    module_2_data <- data.frame(
      group = c("A", "B", "A", "B"),
      scenario_id = c("0", "0", "2", "2"),
      p_mod2_x = c(0.25, 0.35, 0.12, 0.18),
      p_mod2_y = c(0.08, 0.12, 0.03, 0.05)
    )
    example_keys <- list(
      module_1_data = list(cols = names(module_1_data), keys = c("group")),
      module_2_data = list(cols = names(module_2_data), keys = c("group"))
    )
    module_1_exp <- quote({
      p_mod1 <- p_mod1_x + p_mod1_y
    })

    module_1 <- eval_module(
      exp = module_1_exp,
      data = module_1_data,
      data_keys = example_keys
    )

    module_2_exp <- quote({
      p_mod2 <- p_mod2_x + p_mod2_y
    })

    module_2 <- eval_module(
      exp = module_2_exp,
      data = module_2_data,
      data_keys = example_keys
    )

    module_1 <- agg_totals(
      module_1,
      mc_name = "p_mod1",
      agg_keys = "scenario_id"
    )
    module_2 <- agg_totals(
      module_2,
      mc_name = "p_mod2",
      agg_keys = "scenario_id"
    )

    combined <- combine_modules(module_1, module_2)

    combined <- at_least_one(
      combined,
      mc_name = c("p_mod1_agg", "p_mod2_agg"),
      name = "total",
      summary = TRUE
    )

    expect_true("total" %in% names(combined$node_list))
    expect_true(length(combined$node_list$total$data_name) > 1)

    combined <- mc_compare(
      combined,
      "total",
      type = "relative_reduction",
      suffix = "rr"
    )
    expect_true("total_rr" %in% names(combined$node_list))
  })
})
