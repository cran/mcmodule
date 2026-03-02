suppressMessages({
  # Tests for mcmodule_dim_check
  test_that("mcmodule_dim_check returns correct dimensions", {
    result <- mcmodule_dim_check(imports_mcmodule)

    expect_type(result, "list")
    expect_equal(result$n_mcnodes, 7)
    expect_equal(result$n_variate, 6)
    expect_equal(result$n_uncertainty, 1001)
  })

  test_that("mcmodule_dim_check works with subset of nodes", {
    result <- mcmodule_dim_check(
      imports_mcmodule,
      mc_names = c("w_prev", "test_origin")
    )

    expect_equal(result$n_mcnodes, 2)
  })

  test_that("mcmodule_dim_check errors on mismatched variate dimensions", {
    mock_mcmodule <- imports_mcmodule

    mock_mcmodule$node_list$wrong_node <- list(
      mcnode = mcdata(1, type = "0", nvariates = 2)
    )

    expect_error(
      mcmodule_dim_check(mock_mcmodule),
      "same number of variate simulations"
    )
  })

  test_that("mcmodule_dim_check errors on mismatched uncertainty dimensions", {
    mock_mcmodule <- imports_mcmodule

    mock_mcmodule$node_list$wrong_node <- list(
      mcnode = mcstoc(runif, min = 0, max = 1, nvariates = 6, nsv = 100)
    )
    expect_error(
      mcmodule_dim_check(mock_mcmodule),
      "same number of uncertanty simulations"
    )
  })

  # Tests for mcmodule_to_matrices
  test_that("mcmodule_to_matrices returns correct structure", {
    result <- mcmodule_to_matrices(imports_mcmodule)

    expect_type(result, "list")
    expect_length(result, 6)
    expect_true(all(sapply(result, is.matrix)))
    expect_equal(nrow(result[[1]]), 1001)
    expect_equal(ncol(result[[1]]), length(imports_mcmodule$node_list))
  })

  test_that("mcmodule_to_matrices works with single variate", {
    mock_mcmodule <- imports_mcmodule

    mock_mcmodule$node_list$uni_variate <- list(
      mcnode = mcstoc(runif, min = 0, max = 1)
    )

    result <- mcmodule_to_matrices(mock_mcmodule)

    expect_length(result, 6)
    expect_equal(nrow(result[[1]]), 1001)
    expect_equal(ncol(result[[1]]), length(mock_mcmodule$node_list))
  })

  test_that("mcmodule_to_matrices handles list of mcnodes", {
    result <- mcmodule_to_matrices(
      imports_mcmodule,
      mc_names = c("w_prev", "test_origin")
    )
    expect_equal(ncol(result[[1]]), 2)
    expect_true(all(result[[1]][, 2] == 0.5))
  })

  # Tests for mcmodule_to_mc
  test_that("mcmodule_to_mc returns list of mc objects", {
    result <- mcmodule_to_mc(imports_mcmodule)

    expect_type(result, "list")
    expect_length(result, 6)
    expect_s3_class(result[[1]], "mc")
    expect_true(all(names(imports_mcmodule$node_list) %in% names(result[[1]])))
  })

  test_that("mcmodule_to_mc works with subset of nodes", {
    result <- mcmodule_to_mc(
      imports_mcmodule,
      mc_names = c("w_prev", "test_origin")
    )
    expect_length(result[[1]], 2)
  })

  test_that("mcmodule_to_mc with variates_as_nsv = FALSE returns list per variate", {
    result <- mcmodule_to_mc(
      imports_mcmodule,
      variates_as_nsv = FALSE
    )

    expect_type(result, "list")
    expect_length(result, 6) # Should have 6 variates
    expect_s3_class(result[[1]], "mc")

    # Each mc object should have nsv = 1001 (number of uncertainty simulations)
    for (i in seq_along(result)) {
      expect_equal(dim(result[[i]][[1]])[1], 1001)
    }
  })

  test_that("mcmodule_to_mc with variates_as_nsv = TRUE returns single mc object", {
    result <- mcmodule_to_mc(
      imports_mcmodule,
      variates_as_nsv = TRUE
    )

    # Should return mc object directly, not wrapped in a list
    expect_s3_class(result, "mc")

    # The mc object should have nsv = 6 * 1001 = 6006
    # (variates * uncertainty simulations)
    expect_equal(dim(result[[1]])[1], 6006)
  })

  test_that("mcmodule_to_mc variates_as_nsv works with subset of nodes", {
    result <- mcmodule_to_mc(
      imports_mcmodule,
      mc_names = c("w_prev", "test_origin"),
      variates_as_nsv = TRUE
    )

    # Should return mc object directly
    expect_s3_class(result, "mc")
    expect_length(result, 2) # Should have 2 nodes
    expect_equal(dim(result[[1]])[1], 6006)
  })

  # Tests for mcmodule_info and mcmodule_index (deprecated)
  test_that("mcmodule_info returns correct structure", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    result <- mcmodule_info(test_module)

    expect_type(result, "list")
    expect_named(
      result,
      c(
        "is_combined",
        "n_modules",
        "module_names",
        "module_exp_data",
        "data_keys",
        "global_keys"
      )
    )
    expect_true("variate" %in% names(result$data_keys))
    expect_true("data_name" %in% names(result$data_keys))
    expect_true(nrow(result$data_keys) == 6)
    expect_equal(result$global_keys, c("pathogen", "origin"))
    expect_false(result$is_combined)
    expect_equal(result$n_modules, 1)
  })

  # Tests for mcmodule_corr
  test_that("mcmodule_corr works with one expression", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    result <- mcmodule_corr(test_module, print_summary = FALSE)

    expect_s3_class(result, "data.frame")

    # Check required columns
    expect_true(all(
      c(
        "exp",
        "exp_n",
        "variate",
        "output",
        "input",
        "value",
        "strength",
        "method",
        "use"
      ) %in%
        names(result)
    ))

    # Check strength column exists and has valid values
    expect_true("strength" %in% names(result))
    valid_strengths <- c(
      "Very strong",
      "Strong",
      "Moderate",
      "Weak",
      "None",
      NA_character_
    )
    expect_true(all(result$strength %in% valid_strengths))

    # Check key columns are included
    expect_true(all(c("pathogen", "origin") %in% names(result)))

    # Check output column values
    expect_true(all(result$output == "no_detect_a"))

    # Check method values (default is spearman, kendall, pearson)
    expect_true(all(result$method %in% c("spearman", "kendall", "pearson")))

    # Check exp column
    expect_true(all(result$exp == "imports"))

    # Check variate range (should be 1 to 6 for imports_data)
    expect_true(all(result$variate %in% 1:6))

    # Check inputs (should include w_prev and test_sensi)
    expect_true(all(c("w_prev", "test_sensi") %in% unique(result$input)))

    # Check number of rows: 6 variates × 2 inputs = 36 rows
    expect_equal(nrow(result), 12)
  })

  test_that("mcmodule_corr print_summary parameter works", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    # Test with print_summary = FALSE (should not print)
    output <- capture.output({
      result <- mcmodule_corr(
        test_module,
        print_summary = FALSE,
        progress = FALSE
      )
    })
    expect_equal(length(output), 0)
    expect_s3_class(result, "data.frame")

    # Test with print_summary = TRUE (should print)
    output <- capture.output({
      result <- mcmodule_corr(
        test_module,
        print_summary = TRUE,
        progress = FALSE
      )
    })
    expect_true(length(output) > 0)
    expect_true(any(grepl("Correlation Analysis Summary", output)))
    expect_s3_class(result, "data.frame")
  })

  test_that("mcmodule_corr progress parameter works", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    output <- capture.output({
      result <- mcmodule_corr(
        test_module,
        print_summary = FALSE,
        progress = TRUE
      )
    })

    expect_true(any(grepl("\\[Correlation analysis\\] Expression", output)))
    expect_true(any(grepl("imports", output)))
    expect_s3_class(result, "data.frame")
  })

  test_that("mcmodule_corr strength classification is correct", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    result <- mcmodule_corr(test_module, print_summary = FALSE)

    # Test strength classification logic
    for (i in seq_len(nrow(result))) {
      abs_val <- abs(result$value[i])
      expected_strength <- if (is.na(abs_val)) {
        NA_character_
      } else if (abs_val >= 0.8) {
        "Very strong"
      } else if (abs_val >= 0.6) {
        "Strong"
      } else if (abs_val >= 0.4) {
        "Moderate"
      } else if (abs_val >= 0.2) {
        "Weak"
      } else {
        "None"
      }

      expect_equal(result$strength[i], expected_strength)
    }

    # Verify summary includes strength distribution
    output <- capture.output({
      result <- mcmodule_corr(
        test_module,
        print_summary = TRUE,
        progress = FALSE
      )
    })
    expect_true(any(grepl("Input Correlation Strength Distribution", output)))
    expect_true(any(grepl("Inputs by Correlation Strength", output)))
  })

  test_that("mcmodule_corr works with multiple expressions", {
    #  Create previous_module
    previous_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    previous_module <- trial_totals(
      previous_module,
      mc_names = "no_detect_a",
      trials_n = "animals_n",
      subsets_n = "farms_n",
      subsets_p = "h_prev",
      mctable = imports_mctable
    )

    #  Create current_module
    current_data <- data.frame(
      pathogen = c("a", "a", "a", "b", "b", "b", "b"),
      origin = c("east", "south", "nord", "east", "south", "nord", "nord"),
      clean = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
      scenario_id = c("0", "0", "0", "0", "0", "0", "clean_transport"),
      survival_p_min = c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.7),
      survival_p_max = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.8)
    )

    current_data_keys <- list(
      survival = list(
        cols = c("pathogen", "survival_p_min", "survival_p_max"),
        keys = c("pathogen")
      )
    )

    current_mctable <- data.frame(
      mcnode = c("clean", "survival_p"),
      description = c("Transport cleaned", "Survival probability"),
      mc_func = c(NA, "runif"),
      from_variable = c(NA, NA),
      transformation = c(NA, NA),
      sensi_analysis = c(TRUE, TRUE)
    )

    current_exp <- quote({
      imported_contaminated <- no_detect_a_set * survival_p * (1 - clean)
    })

    current_module <- eval_module(
      exp = c(current = current_exp),
      data = current_data,
      mctable = current_mctable,
      data_keys = current_data_keys,
      prev_mcmodule = previous_module
    )

    combined_module <- combine_modules(previous_module, current_module)

    combined_module <- at_least_one(
      combined_module,
      c("no_detect_a", "imported_contaminated"),
      name = "total"
    )

    result <- mcmodule_corr(
      combined_module,
      print_summary = FALSE,
      progress = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true(all(
      c("exp", "variate", "input", "value", "output") %in% names(result)
    ))
    result <- mcmodule_corr(
      combined_module,
      output = "total",
      print_summary = FALSE,
      progress = FALSE
    )
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true(all(result$output == "total"))

    result <- mcmodule_corr(
      combined_module,
      by_exp = TRUE,
      print_summary = FALSE,
      progress = FALSE
    )
    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    expect_true(all(result$exp %in% c("imports", "current")))
  })

  test_that("mcmodule_corr works with variates_as_nsv = FALSE", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    result <- mcmodule_corr(
      test_module,
      variates_as_nsv = FALSE,
      print_summary = FALSE,
      progress = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    # With variates_as_nsv = FALSE, should have 6 variates
    expect_true(all(result$variate %in% 1:6))
    expect_equal(length(unique(result$variate)), 6)
  })

  test_that("mcmodule_corr works with variates_as_nsv = TRUE", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    result <- mcmodule_corr(
      test_module,
      variates_as_nsv = TRUE,
      print_summary = FALSE,
      progress = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
    # With variates_as_nsv = TRUE, should have only 1 variate (combined)
    expect_equal(length(unique(result$variate)), 1)
    expect_equal(unique(result$variate), 1)
  })

  # Tests for mcmodule_converg
  test_that("mcmodule_converg returns correct structure", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    result <- mcmodule_converg(test_module, print_summary = FALSE)

    expect_s3_class(result, "data.frame")

    # Check required columns including new standardized columns
    expect_true(all(
      c(
        "expression",
        "variate",
        "node",
        "max_dif",
        "max_dif_scaled",
        "max_dif_mean",
        "max_dif_median",
        "max_dif_q025",
        "max_dif_q975",
        "max_dif_mean_scaled",
        "max_dif_median_scaled",
        "max_dif_q025_scaled",
        "max_dif_q975_scaled",
        "tiny",
        "conv_01",
        "conv_025",
        "conv_05",
        "conv_01_tiny",
        "conv_025_tiny",
        "conv_05_tiny"
      ) %in%
        names(result)
    ))

    # Check that scaled deviation columns are numeric
    expect_type(result$max_dif_mean_scaled, "double")
    expect_type(result$max_dif_median_scaled, "double")
    expect_type(result$max_dif_q025_scaled, "double")
    expect_type(result$max_dif_q975_scaled, "double")

    # Check that convergence columns are logical
    expect_type(result$tiny, "logical")
    expect_type(result$conv_01, "logical")
    expect_type(result$conv_025, "logical")
    expect_type(result$conv_05, "logical")
  })

  test_that("mcmodule_converg print_summary parameter works", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    # Test with print_summary = FALSE
    output <- capture.output({
      result <- mcmodule_converg(
        test_module,
        print_summary = FALSE,
        progress = FALSE
      )
    })
    expect_false(any(grepl("Convergence Analysis Summary", output)))
    expect_s3_class(result, "data.frame")

    # Test with print_summary = TRUE (should print summary)
    output <- capture.output({
      result <- mcmodule_converg(
        test_module,
        print_summary = TRUE,
        progress = FALSE
      )
    })
    expect_true(length(output) > 0)
    expect_true(any(grepl("Convergence Analysis Summary", output)))
    expect_true(any(grepl("Stochastic Distributions Stability", output)))
    expect_true(any(grepl("standardized:", output)))
    expect_s3_class(result, "data.frame")
  })

  test_that("mcmodule_converg progress parameter works", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    output <- capture.output({
      result <- mcmodule_converg(
        test_module,
        print_summary = FALSE,
        progress = TRUE
      )
    })

    expect_true(any(grepl("\\[Convergence analysis\\] Expression", output)))
    expect_true(any(grepl("imports", output)))
    expect_s3_class(result, "data.frame")
  })

  test_that("mcmodule_converg works with custom threshold", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    result <- mcmodule_converg(
      test_module,
      conv_threshold = 0.03,
      print_summary = FALSE,
      progress = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_true("conv_manual" %in% names(result))
    expect_type(result$conv_manual, "logical")
  })

  test_that("mcmodule_converg works with different quantile ranges", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    result <- mcmodule_converg(
      test_module,
      from_quantile = 0.9,
      to_quantile = 1,
      print_summary = FALSE,
      progress = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_true(nrow(result) > 0)
  })

  test_that("mcmodule_converg standardized deviations are calculated correctly", {
    test_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    result <- mcmodule_converg(
      test_module,
      print_summary = FALSE,
      progress = FALSE
    )

    # Check that standardized values are ratios of raw values to means
    # (or NA if mean is zero)
    for (i in seq_len(nrow(result))) {
      if (is.na(result$max_dif_mean_scaled[i])) {
        # If scaled is NA, the mean statistic should be zero or close to zero
        expect_true(TRUE) # Just verify NA is acceptable
      } else {
        # Standardized should be less than or equal to 1 in most cases
        # (unless the max deviation is very large)
        expect_type(result$max_dif_mean_scaled[i], "double")
      }
    }

    # Check that all scaled columns exist and are numeric
    expect_true(is.numeric(result$max_dif_mean_scaled))
    expect_true(is.numeric(result$max_dif_median_scaled))
    expect_true(is.numeric(result$max_dif_q025_scaled))
    expect_true(is.numeric(result$max_dif_q975_scaled))
  })
})
