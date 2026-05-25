suppressMessages({
  test_that("at_least_one works", {
    # Create test module
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
        ),
        p_2 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.5, 0.6, 0.7), type = "0", nvariates = 3),
            max = mcdata(c(0.6, 0.7, 0.8), type = "0", nvariates = 3),
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

    # Test basic functionality
    result <- at_least_one(test_module, c("p_1", "p_2"), name = "p_combined")

    # Check node attributes
    expect_equal(result$node_list$p_combined$type, "total")
    expect_equal(result$node_list$p_combined$param, c("p_1", "p_2"))

    # Test error on missing nodes
    expect_error(at_least_one(test_module, c("p_1", "missing")), "not found")
  })

  test_that("at_least_one match works", {
    # Create test module
    test_module <- list(
      node_list = list(
        p_1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3),
            max = mcdata(c(0.2, 0.3, 0.4), type = "0", nvariates = 3),
            nvariates = 3
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        p_2 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.5, 0.6, 0.7), type = "0", nvariates = 3),
            max = mcdata(c(0.6, 0.7, 0.8), type = "0", nvariates = 3),
            nvariates = 3
          ),
          data_name = "data_y",
          keys = c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B", "C"),
          scenario_id = c("0", "0", "0")
        ),
        data_y = data.frame(
          category = c("B", "B", "B"),
          scenario_id = c("0", "1", "2")
        )
      )
    )

    # Test basic functionality
    result <- at_least_one(test_module, c("p_1", "p_2"), name = "p_combined")

    # Check node attributes
    expect_equal(result$node_list$p_combined$type, "total")
    expect_equal(result$node_list$p_combined$param, c("p_1", "p_2"))

    # Verify expected keys_xy
    expect_equal(
      result$node_list$p_combined$summary$category,
      c("A", "B", "C", "B", "B")
    )

    # Test error on missing nodes
    expect_error(at_least_one(test_module, c("p_1", "missing")), "not found")
  })

  test_that("at_least_one works with two from_sample_design nodes", {
    # Create test module with from_sample_design nodes
    sample_design <- data.frame(
      input_a = c(0.1, 0.2, 0.3),
      stringsAsFactors = FALSE
    )

    sample_module <- eval_module(
      exp = list(
        sample = quote({
          result_a <- input_a
          result_b <- input_a + 2
        })
      ),
      data = data.frame(),
      sample_design = sample_design
    )

    # Test basic functionality
    result <- at_least_one(
      sample_module,
      c("result_a", "result_b"),
      name = "p_combined"
    )

    # Check node attributes
    expect_equal(result$node_list$p_combined$type, "total")
    expect_equal(result$node_list$p_combined$param, c("result_a", "result_b"))
    expect_equal(result$node_list$p_combined$from_sample_design, TRUE)
  })

  test_that("generate_all_name works", {
    # Basic functionality
    expect_equal(generate_all_name(c("test_a", "test_b")), "test_all")
    expect_equal(
      generate_all_name(c(
        "good_special_a",
        "good_special_b",
        "good_special_top"
      )),
      "good_special_all"
    )

    # Error cases
    expect_error(
      generate_all_name(c("good_special_a", "bad_special_b")),
      "Input strings do not share a common prefix"
    )
    expect_error(
      generate_all_name(c("test_a", "test_all")),
      "One of the mc_names already contains '_all' suffix"
    )
  })

  test_that("agg_totals works correctly", {
    # Create test data
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
        ),
        p_2 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.5, 0.6, 0.7), type = "0", nvariates = 3),
            max = mcdata(c(0.6, 0.7, 0.8), type = "0", nvariates = 3),
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

    # Test default agg
    result <- agg_totals(test_module, "p_1")

    expect_equal(
      result$node_list[["p_1_agg"]]$description,
      "Combined probability assuming independence by: scenario_id"
    )

    # Test aggregation methods
    result_sum <- agg_totals(test_module, "p_1", agg_func = "sum")
    expect_equal(
      result_sum$node_list[["p_1_agg"]]$description,
      "Sum by: scenario_id"
    )

    result_avg <- agg_totals(test_module, "p_1", agg_func = "avg")
    expect_equal(
      result_avg$node_list[["p_1_agg"]]$description,
      "Average value by: scenario_id"
    )

    # Test error handling
    expect_error(agg_totals(test_module, "test_node", agg_func = "invalid"))
    expect_error(agg_totals(test_module, "nonexistent_node"))
  })

  # Helper function to setup the test module
  setup_test_mcmodule <- function() {
    # Test module with mock data including:
    # - p_1_x and p_1_y: Two probability nodes with uniform distribution
    # - p_2: Another probability node for subset calculations
    # - times_n: Number of trials for each category/scenario combination
    mcmodule <- list(
      node_list = list(
        p_1_x = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.3, 0.5), type = "0", nvariates = 4),
            max = mcdata(c(0.2, 0.3, 0.4, 0.6), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "test_data",
          keys = c("category", "scenario_id")
        ),
        p_1_y = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.8, 0.7, 0.6, 0.5), type = "0", nvariates = 4),
            max = mcdata(c(0.9, 0.8, 0.7, 0.6), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "test_data",
          keys = c("category", "scenario_id")
        ),
        p_2 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.5, 0.6, 0.7, 0.8), type = "0", nvariates = 4),
            max = mcdata(c(0.6, 0.7, 0.8, 0.9), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "test_data",
          keys = c("category", "scenario_id")
        ),
        times_n = list(
          mcnode = mcdata(c(3, 4, 5, 6), type = "0", nvariates = 4),
          data_name = "test_data",
          keys = c("category", "scenario_id")
        )
      ),
      # Test data frame with categories A/B and scenarios 0/1
      data = list(
        test_data = data.frame(
          category = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1"),
          times_n = c(3, 4, 5, 6),
          sites_n_min = c(2, 2, 2, 2),
          sites_n_max = c(2, 3, 4, 5)
        )
      )
    )

    # Setup the test mctable
    test_mctable <- data.frame(
      mcnode = c("times_n", "sites_n"),
      description = c("Number of trials", "Number of sites"),
      mc_func = c("runif", "runif"),
      from_variable = c(NA, NA),
      transformation = c(NA, NA),
      sample_space = c("min = 1, max = 10", "min = 2, max = 5")
    )
    set_mctable(test_mctable)

    return(mcmodule)
  }

  test_that("trial_totals basic functionality works", {
    # Create a test module with mock data
    test_module <- setup_test_mcmodule()

    # Basic trial_totals with two probability nodes
    result <- trial_totals(
      mcmodule = test_module,
      mc_names = c("p_1_x", "p_1_y"),
      trials_n = "times_n"
    )

    expect_true("p_1_all_set" %in% names(result$node_list))
    expect_true(is.null(result$node_list$sites_n))
    expect_true(is.null(result$node_list$sites_p))

    reset_mctable()
  })

  test_that("trial_totals works with subset probabilities", {
    # Create a test module with mock data
    test_module <- setup_test_mcmodule()

    # Add sites_n as subset probability (multilevel: hierarchical_p)
    result <- trial_totals(
      test_module,
      mc_names = c("p_1_x", "p_1_y"),
      trials_n = "times_n",
      subsets_p = "p_2"
    )
    expect_true("p_1_all_set" %in% names(result$node_list))
    expect_true(is.null(result$node_list$sites_n))

    reset_mctable()
  })

  test_that("trial_totals works with both subset types", {
    # Create a test module with mock data
    test_module <- setup_test_mcmodule()

    # Test with single probability node and both subset types
    result <- trial_totals(
      test_module,
      mc_names = "p_1_x",
      trials_n = "times_n",
      subsets_n = "sites_n",
      subsets_p = "p_2"
    )

    expect_true("p_1_x_set" %in% names(result$node_list))

    reset_mctable()
  })

  test_that("trial_totals handles custom level suffixes", {
    # Create a test module with mock data
    test_module <- setup_test_mcmodule()

    # Test with custom level suffix
    result <- trial_totals(
      test_module,
      mc_names = "p_1_x",
      trials_n = "times_n",
      subsets_n = "sites_n",
      subsets_p = "p_2",
      level_suffix = c(
        trial = "singletime",
        subset = "singlesite",
        set = "allsites"
      )
    )

    expect_true("p_1_x_singletime" %in% names(result$node_list))

    # Test with partial custom level suffix
    result <- trial_totals(
      test_module,
      mc_names = "p_1_x",
      trials_n = "times_n",
      subsets_n = "sites_n",
      subsets_p = "p_2",
      level_suffix = c(set = "allsites")
    )

    expect_true("p_1_x_trial" %in% names(result$node_list))

    reset_mctable()
  })

  test_that("trial_totals handles scenario aggregation", {
    # Create a test module with mock data
    test_module <- setup_test_mcmodule()

    # Test with custom suffix and scenario_id aggregation
    result <- trial_totals(
      test_module,
      mc_names = c("p_1_x"),
      trials_n = "times_n",
      subsets_n = "sites_n",
      subsets_p = "p_2",
      agg_keys = "scenario_id",
      agg_suffix = "site"
    )
    expect_true("p_1_x_site_set" %in% names(result$node_list))

    # Test aggregation with multiple probability nodes
    result <- trial_totals(
      test_module,
      mc_names = c("p_1_x", "p_1_y"),
      trials_n = "times_n",
      subsets_n = "sites_n",
      subsets_p = "p_2",
      agg_keys = "scenario_id",
    )

    expect_true("p_1_all_hag_set" %in% names(result$node_list))
    expect_equal(dim(result$node_list$p_1_all$mcnode), c(1001, 1, 4))
    expect_equal(dim(result$node_list$p_1_all_hag_set$mcnode), c(1001, 1, 2))

    reset_mctable()
  })

  test_that("trial_totals works with custom name parameter", {
    # Create a test module with mock data
    test_module <- setup_test_mcmodule()

    # Test aggregation with multiple probability nodes when name is provided
    result <- trial_totals(
      test_module,
      mc_names = c("p_1_x", "p_1_y"),
      trials_n = "times_n",
      subsets_n = "sites_n",
      subsets_p = "p_2",
      agg_keys = "scenario_id",
      name = "p_total"
    )

    expect_true("p_1_x_hag_set" %in% names(result$node_list))
    expect_true("p_total_hag" %in% names(result$node_list))
    expect_true("p_total" %in% names(result$node_list))

    expect_equal(dim(result$node_list$p_total$mcnode), c(1001, 1, 4))
    expect_equal(dim(result$node_list$p_total_hag$mcnode), c(1001, 1, 2))

    reset_mctable()
  })

  test_that("trial_totals keeps variates when requested", {
    # Create a test module with mock data
    test_module <- setup_test_mcmodule()

    # Test with keep_variates option
    result <- trial_totals(
      test_module,
      mc_names = c("p_1_x", "p_1_y"),
      trials_n = "times_n",
      subsets_n = "sites_n",
      subsets_p = "p_2",
      agg_keys = "scenario_id",
      keep_variates = TRUE
    )

    expect_equal(dim(result$node_list$p_1_all_hag_set$mcnode), c(1001, 1, 4))
    expect_true(result$node_list$p_2_hag$keep_variates)

    reset_mctable()
  })

  test_that("trial_totals handles error cases", {
    # Create a test module with mock data
    test_module <- setup_test_mcmodule()

    # Expect error: node not found in mcmodule
    expect_error(
      {
        result <- trial_totals(
          test_module,
          mc_names = c("p_1_x", "nonexistent_node"),
          trials_n = "times_n"
        )
      },
      "nonexistent_node not found in test_module"
    )

    # Expect error: node not found in mctable
    expect_error(
      {
        result <- trial_totals(
          test_module,
          mc_names = c("p_1_x", "p_1_y"),
          trials_n = "nonexistent_node"
        )
      },
      "nonexistent_node not found in mctable"
    )

    reset_mctable()
  })

  test_that("trial_totals works with sampling design", {
    # Create a test module with mock data
    X <- mctable_sobol_matrices(imports_mctable, N = 1000)
    sd_module <- eval_module(
      exp = imports_exp,
      data = NULL,
      sample_design = X,
      mctable = imports_mctable
    )
    result <- trial_totals(
      mcmodule = sd_module,
      mc_names = c("no_detect"),
      trials_n = "animals_n",
      subsets_n = "farms_n",
      subsets_p = "h_prev",
      sample_design = X,
      mctable = imports_mctable
    )
    expect_true("no_detect_set" %in% names(result$node_list))
    expect_true(result$node_list$no_detect_subset_n$from_sample_design)
  })

  test_that("at_least_one naming options work", {
    # Setup module
    module <- list(
      node_list = list(
        p_1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2), type = "0", nvariates = 2),
            max = mcdata(c(0.2, 0.3), type = "0", nvariates = 2),
            nvariates = 2
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        p_2 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.5, 0.6), type = "0", nvariates = 2),
            max = mcdata(c(0.6, 0.7), type = "0", nvariates = 2),
            nvariates = 2
          ),
          data_name = "data_x",
          keys = c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B"),
          scenario_id = c("0", "0")
        )
      )
    )

    # Default name
    mod1 <- at_least_one(module, c("p_1", "p_2"))
    expect_true("p_all" %in% names(mod1$node_list))

    # Custom name
    mod2 <- at_least_one(module, c("p_1", "p_2"), name = "custom_comb")
    expect_true("custom_comb" %in% names(mod2$node_list))

    # Custom all_suffix
    mod3 <- at_least_one(module, c("p_1", "p_2"), all_suffix = "combo")
    expect_true("p_combo" %in% names(mod3$node_list))

    # Custom prefix
    mod4 <- at_least_one(module, c("p_1", "p_2"), prefix = "pre")
    expect_equal(mod4$node_list[["pre_p_all"]]$prefix, "pre")

    # Error on missing node
    expect_error(at_least_one(module, c("p_1", "missing")), "not found")
  })

  test_that("generate_all_name works with suffix and errors", {
    expect_equal(generate_all_name(c("test_a", "test_b")), "test_all")
    expect_equal(
      generate_all_name(c("foo_a", "foo_b"), all_suffix = "group"),
      "foo_group"
    )
    expect_error(generate_all_name(c("foo_a", "bar_b")), "common prefix")
    expect_error(
      generate_all_name(c("foo_all", "foo_b")),
      "contains '_all' suffix"
    )
  })

  test_that("agg_totals naming options work", {
    module <- list(
      node_list = list(
        p_1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2), type = "0", nvariates = 2),
            max = mcdata(c(0.2, 0.3), type = "0", nvariates = 2),
            nvariates = 2
          ),
          data_name = "data_x",
          keys = c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B"),
          scenario_id = c("0", "0")
        )
      )
    )

    # Default agg name
    mod1 <- agg_totals(module, "p_1")
    expect_true("p_1_agg" %in% names(mod1$node_list))

    # Custom name
    mod2 <- agg_totals(module, "p_1", name = "custom_agg")
    expect_true("custom_agg" %in% names(mod2$node_list))

    # Custom agg_suffix
    mod3 <- agg_totals(module, "p_1", agg_suffix = "sum")
    expect_true("p_1_sum" %in% names(mod3$node_list))

    # Error for invalid agg_func
    expect_error(
      agg_totals(module, "p_1", agg_func = "invalid"),
      "Aggregation function"
    )

    # Error for missing node
    expect_error(agg_totals(module, "missing"), "not found")
  })

  test_that("trial_totals naming options work", {
    module <- list(
      node_list = list(
        p_1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2), type = "0", nvariates = 2),
            max = mcdata(c(0.2, 0.3), type = "0", nvariates = 2),
            nvariates = 2
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        p_2 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.4, 0.5), type = "0", nvariates = 2),
            max = mcdata(c(0.6, 0.7), type = "0", nvariates = 2),
            nvariates = 2
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        times_n = list(
          mcnode = mcdata(c(3, 4), type = "0", nvariates = 2),
          data_name = "data_x",
          keys = c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B"),
          scenario_id = c("0", "0"),
          times_n = c(3, 4)
        )
      )
    )

    # Default trial_totals
    mod1 <- trial_totals(module, mc_names = "p_1", trials_n = "times_n")
    expect_true("p_1_set" %in% names(mod1$node_list))

    # Custom name
    mod2 <- trial_totals(
      module,
      mc_names = "p_1",
      trials_n = "times_n",
      name = "custom_trial"
    )
    expect_true("custom_trial_set" %in% names(mod2$node_list))

    # Custom agg_suffix - no agg_keys
    mod3 <- trial_totals(
      module,
      mc_names = "p_1",
      trials_n = "times_n",
      agg_suffix = "aggx"
    )
    expect_true("p_1_set" %in% names(mod3$node_list))

    # Custom agg_suffix - agg_keys
    mod4 <- trial_totals(
      module,
      mc_names = "p_1",
      trials_n = "times_n",
      agg_suffix = "aggx",
      agg_keys = "category"
    )
    expect_true("p_1_aggx_set" %in% names(mod4$node_list))

    # Custom all_suffix
    mod5 <- trial_totals(
      module,
      mc_names = "p_1",
      trials_n = "times_n",
      all_suffix = "tot"
    )
    expect_true("p_1_set" %in% names(mod5$node_list))

    mod5 <- trial_totals(
      module,
      mc_names = c("p_1", "p_2"),
      trials_n = "times_n",
      all_suffix = "tot"
    )
    expect_true("p_tot_set" %in% names(mod5$node_list))

    # Custom prefix
    mod6 <- trial_totals(
      module,
      mc_names = "p_1",
      trials_n = "times_n",
      prefix = "pre"
    )
    expect_true("pre_p_1_set" %in% names(mod6$node_list))

    # Custom all options
    mod7 <- trial_totals(
      module,
      mc_names = "p_1",
      trials_n = "times_n",
      name = "custom",
      agg_suffix = "aggx",
      all_suffix = "tot",
      prefix = "pre"
    )
    expect_true("pre_custom_set_n" %in% names(mod7$node_list))

    # Custom name with agg_keys
    mod8 <- trial_totals(
      module,
      mc_names = "p_1",
      trials_n = "times_n",
      name = "custom_trial",
      agg_suffix = "hag",
      agg_keys = "category"
    )

    expect_true("custom_trial_hag_set" %in% names(mod8$node_list))

    # Custom name with agg_keys but no agg_suffix
    mod9 <- trial_totals(
      module,
      mc_names = "p_1",
      trials_n = "times_n",
      name = "custom_trial",
      agg_suffix = "",
      agg_keys = "category"
    )

    expect_true("custom_trial_set" %in% names(mod9$node_list))

    # Error for missing node
    expect_error(
      trial_totals(module, mc_names = "missing", trials_n = "times_n"),
      "not found"
    )
  })

  test_that("at_least_one match works with agg mcnodes", {
    # Create a test module with mock data
    test_module <- setup_test_mcmodule()
    test_module <- agg_totals(
      test_module,
      c("p_1_x"),
      agg_keys = c("scenario_id", "category")
    )
    test_module <- agg_totals(
      test_module,
      c("p_2"),
      agg_keys = c("scenario_id", "category")
    )

    # At least one with agg mcmodules
    result <- at_least_one(
      test_module,
      c("p_1_x_agg", "p_2_agg"),
      name = "p_combined"
    )

    # Check aggregated keys
    expect_equal(
      result$node_list$p_combined$agg_keys,
      c("scenario_id", "category")
    )
  })

  test_that("totals work with mcnodes with multiple data_name", {
    # Create a test modules with mock data
    test_module_1 <- list(
      node_list = list(
        p_a = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2), type = "0", nvariates = 2),
            max = mcdata(c(0.2, 0.3), type = "0", nvariates = 2),
            nvariates = 2
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        p_b = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.4, 0.5), type = "0", nvariates = 2),
            max = mcdata(c(0.6, 0.7), type = "0", nvariates = 2),
            nvariates = 2
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        times_n = list(
          mcnode = mcdata(c(3, 4), type = "0", nvariates = 2),
          data_name = "data_x",
          keys = c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B"),
          scenario_id = c("0", "0"),
          times_n = c(3, 4)
        )
      )
    )

    test_module_2 <- setup_test_mcmodule()

    test_module <- combine_modules(test_module_1, test_module_2)

    # At least one with agg mcmodules
    test_module <- at_least_one(
      test_module,
      c("p_a", "p_2"),
      name = "p_combined_1"
    )

    # Check both data_names are stored
    expect_equal(
      test_module$node_list$p_combined_1$data_name,
      c("data_x", "test_data")
    )
    expect_equal(
      test_module$node_list$p_combined_1$summary$category,
      c("A", "B", "A", "B")
    )

    # Combine in at_least_one
    test_module <- at_least_one(
      mcmodule = test_module,
      mc_names = c("p_combined_1", "p_b"),
      name = "p_combined_2"
    )

    # Check both data_names are stored and dimensions
    expect_equal(
      test_module$node_list$p_combined_2$data_name,
      c("data_x", "test_data")
    )
    expect_equal(
      test_module$node_list$p_combined_2$summary$category,
      c("A", "B", "A", "B")
    )

    # Combine in trial_totals
    test_module <- trial_totals(
      mcmodule = test_module,
      mc_names = c("p_combined_1", "p_b"),
      trials_n = "times_n",
      name = "p_combined_3"
    )

    # Check data_names and dimensions
    expect_equal(
      test_module$node_list$p_combined_3_set$data_name,
      c("data_x", "test_data")
    )
    expect_equal(
      test_module$node_list$p_combined_3_set$summary$category,
      c("A", "B", "A", "B")
    )
    expect_equal(dim(test_module$node_list$p_b$mcnode)[3], 2)
    expect_equal(dim(test_module$node_list$p_b_set$mcnode)[3], 4)
  })

  test_that("trial_totals works with explicit data_name argument", {
    # Reuse module from previous test
    test_module <- list(
      node_list = list(
        p_1 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2), type = "0", nvariates = 2),
            max = mcdata(c(0.2, 0.3), type = "0", nvariates = 2),
            nvariates = 2
          ),
          data_name = "data_a",
          keys = c("category")
        ),
        p_2 = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.4, 0.5), type = "0", nvariates = 2),
            max = mcdata(c(0.6, 0.7), type = "0", nvariates = 2),
            nvariates = 2
          ),
          data_name = "data_b",
          keys = c("category")
        ),
        times_n = list(
          mcnode = mcdata(c(3, 4), type = "0", nvariates = 2),
          data_name = "data_a",
          keys = c("category")
        )
      ),
      data = list(
        data_a = data.frame(
          category = c("A", "B"),
          scenario_id = c("0", "0"),
          times_n = c(3, 4)
        ),
        data_b = data.frame(category = c("B", "A"), scenario_id = c("0", "0"))
      )
    )

    expect_message(
      result <- trial_totals(
        mcmodule = test_module,
        mc_names = c("p_1", "p_2"),
        trials_n = "times_n",
        data_name = "data_a"
      ),
      "Using data_name 'data_a' for node creation"
    )

    expect_equal(result$node_list$p_all_set$data_name, c("data_a", "data_b"))
  })

  test_that("agg_totals works with filtered mcnodes (mc_filter integration)", {
    # Create test module with grouped data
    test_module <- list(
      node_list = list(
        contact = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(
              c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
              type = "0",
              nvariates = 6
            ),
            max = mcdata(
              c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7),
              type = "0",
              nvariates = 6
            ),
            nvariates = 6
          ),
          data_name = "contact_data",
          keys = c("visit_type", "scenario_id")
        )
      ),
      data = list(
        contact_data = data.frame(
          visit_type = c(
            "vehicle",
            "vehicle",
            "vehicle",
            "person",
            "person",
            "person"
          ),
          scenario_id = c("0", "0", "0", "0", "0", "0"),
          fomite_id = c(
            "wheels_1",
            "wheels_2",
            "boots",
            "boots",
            "equipment",
            "gloves"
          )
        )
      )
    )

    # Filter to vehicle visits only
    filtered_module <- mc_filter(
      test_module,
      "contact",
      visit_type == "vehicle",
      suffix = "veh"
    )

    # Verify filtered node has correct dimensions
    expect_equal(dim(filtered_module$node_list$contact_veh$mcnode)[3], 3)

    # Now aggregate the filtered node
    agg_module <- agg_totals(
      filtered_module,
      "contact_veh",
      agg_keys = "scenario_id"
    )

    # Check that aggregated node was created successfully
    expect_true("contact_veh_agg" %in% names(agg_module$node_list))
    expect_true(is.mcnode(agg_module$node_list$contact_veh_agg$mcnode))

    # Check metadata
    expect_equal(agg_module$node_list$contact_veh_agg$type, "agg_total")
  })

  test_that("agg_totals handles single-variate groups correctly (edge case)", {
    # Create test module where some groups will have only 1 variate
    test_module <- list(
      node_list = list(
        risk = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(0.1, 0.2, 0.3, 0.4, 0.5), type = "0", nvariates = 5),
            max = mcdata(c(0.2, 0.3, 0.4, 0.5, 0.6), type = "0", nvariates = 5),
            nvariates = 5
          ),
          data_name = "test_data",
          keys = c("category", "scenario_id")
        )
      ),
      data = list(
        test_data = data.frame(
          category = c("A", "B", "C", "D", "E"),
          scenario_id = c("0", "0", "0", "1", "1"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Aggregate by scenario_id
    # Group 1 (scenario "0"): 3 variates (A, B, C)
    # Group 2 (scenario "1"): 2 variates (D, E) - different from first group size
    result <- agg_totals(
      test_module,
      "risk",
      agg_keys = "scenario_id"
    )

    # Check that aggregation succeeded
    expect_true("risk_agg" %in% names(result$node_list))
    expect_true(is.mcnode(result$node_list$risk_agg$mcnode))

    # Check that result has correct dimensions (2 groups)
    expect_equal(dim(result$node_list$risk_agg$mcnode)[3], 2)

    # Check summary
    expect_equal(nrow(result$node_list$risk_agg$summary), 2)
  })
  test_that("agg_totals handles from_sample_design nodes correctly", {
    # Create test module with a node from sample design
    sample_design <- data.frame(
      input_a = c(0.1, 0.2, 0.3),
      input_b = c(0.4, 0.5, 0.6)
    )
    sample_module <- eval_module(
      exp = list(
        sample_exp = quote({
          result_a <- input_a + 2
          result_b <- input_b + result_a
        })
      ),
      data = NULL,
      sample_design = sample_design
    )

    # Aggregate the results
    result <- agg_totals(
      sample_module,
      mc_name = c("result_b")
    )

    # Check that the aggregated node is created and has correct metadata
    expect_true("result_b_agg" %in% names(result$node_list))
    expect_true(is.mcnode(result$node_list$result_b_agg$mcnode))
    expect_equal(result$node_list$result_b_agg$from_sample_design, TRUE)
  })

  test_that("agg_totals handles from_sample_design nodes with data and keys correctly", {
    # Create test module with a node from sample design
    sample_design <- data.frame(
      input_a = c(0.1, 0.2, 0.3),
      input_b = c(0.4, 0.5, 0.6)
    )

    sample_data <- data.frame(
      input_a = c(0.1, 0.2, 0.3, 0.1, 0.2, 0.3),
      input_b = c(0.4, 0.5, 0.6, 0.4, 0.5, 0.6),
      category = c("A", "B", "C", "A", "B", "C"),
      group = c("G1", "G1", "G1", "G2", "G2", "G2")
    )

    sample_module <- eval_module(
      exp = list(
        sample_exp = quote({
          result_a <- input_a + 2
          result_b <- input_b + result_a
        })
      ),
      data = sample_data,
      sample_design = sample_design
    )

    # Aggregate the results
    result <- agg_totals(
      sample_module,
      mc_name = c("result_b"),
      agg_keys = c("category")
    )
    # Check that the aggregated node is created and has correct metadata
    expect_true("result_b_agg" %in% names(result$node_list))
    expect_true(is.mcnode(result$node_list$result_b_agg$mcnode))
    expect_equal(result$node_list$result_b_agg$from_sample_design, TRUE)
  })
})
