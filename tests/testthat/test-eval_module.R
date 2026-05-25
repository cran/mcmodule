suppressMessages({
  test_that("eval_module works", {
    # Test basic functionality
    result <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    # Check class and structure
    expect_equal(class(result), "mcmodule")
    expect_true(all(
      c("data", "exp", "node_list") %in% names(result)
    ))

    # Test error handling for missing prev_mcmodule
    test_exp_prev <- quote({
      result <- prev_value * 2
    })
    expect_error(
      eval_module(
        exp = test_exp_prev,
        data = imports_data,
        mctable = imports_mctable,
        data_keys = imports_data_keys
      ),
      "nodes are not present in data or in prev_mcmodule"
    )

    # Test with multiple expressions
    exp_list <- list(
      imports = imports_exp,
      additional = quote({
        final_result <- no_detect * 2
      })
    )

    multi_result <- eval_module(
      exp = exp_list,
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    # Verify multiple expressions were evaluated
    expect_equal(names(multi_result$exp), c("imports", "additional"))

    # Check that variables from first module are available in second
    expect_true("no_detect" %in% names(multi_result$node_list))
    expect_true("final_result" %in% names(multi_result$node_list))

    # Check that inputs have the right metadata
    expect_equal(multi_result$node_list$test_sensi$keys, c("pathogen"))
    expect_equal(
      multi_result$node_list$test_sensi$input_dataset,
      c("test_sensitivity")
    )

    expect_equal(multi_result$node_list$w_prev$keys, c("pathogen", "origin"))
    expect_equal(
      multi_result$node_list$w_prev$input_dataset,
      c("prevalence_region")
    )

    # Check that outputs have the right metadata
    expect_equal(
      multi_result$node_list$no_detect$keys,
      c("pathogen", "origin")
    )
    expect_equal(
      multi_result$node_list$no_detect$inputs,
      c("false_neg", "no_test")
    )

    expect_equal(
      multi_result$node_list$final_result$keys,
      c("pathogen", "origin")
    )
    expect_equal(multi_result$node_list$final_result$inputs, c("no_detect"))
  })

  test_that("eval_module gets previous nodes", {
    #  Create previous_module
    previous_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    previous_module <- trial_totals(
      previous_module,
      mc_names = "no_detect",
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
      survival_p_min = c(0.7, 0.7, 0.7, 0.7, 0.7, 0.7, 0.1),
      survival_p_max = c(0.8, 0.8, 0.8, 0.8, 0.8, 0.8, 0.15)
    )

    current_data_keys <- list(
      survival = list(
        cols = c("pathogen", "clean", "survival_p_min", "survival_p_max"),
        keys = c("pathogen", "clean")
      )
    )

    current_mctable <- data.frame(
      mcnode = c("survival_p"),
      description = c("Survival probability"),
      mc_func = c("runif"),
      from_variable = c(NA),
      transformation = c(NA),
      sensi_variation = c(NA_character_)
    )
    current_exp <- quote({
      imported_contaminated <- no_detect_set * survival_p
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
      c("no_detect", "imported_contaminated"),
      name = "total"
    )

    expect_equal(
      combined_module$node_list$no_detect$keys,
      c("pathogen", "origin")
    )
    summary1 <- mc_summary(combined_module, "no_detect_set")
    expect_equal(summary1$pathogen, c("a", "a", "a", "b", "b", "b"))

    expect_equal(
      combined_module$node_list$survival_p$keys,
      c("pathogen", "clean")
    )
    expect_equal(
      combined_module$node_list$survival_p$input_dataset,
      c("survival")
    )
    summary2 <- mc_summary(combined_module, "survival_p")
    expect_equal(summary2$pathogen, c("a", "a", "a", "b", "b", "b", "b"))

    expect_equal(
      combined_module$node_list$imported_contaminated$keys,
      c("pathogen", "origin", "clean")
    ) # union
    summary3 <- mc_summary(combined_module, "imported_contaminated")
    expect_equal(summary3$scenario_id, c(0, 0, 0, 0, 0, 0, "clean_transport"))

    expect_equal(
      combined_module$node_list$total$keys,
      c("scenario_id", "pathogen", "origin")
    ) # intersection
    expect_message(
      mc_summary(combined_module, "total"),
      "Too many data names. Using existing summary."
    )
  })

  test_that("get_mcmodule_nodes works", {
    # Create test data
    test_node <- list(mcnode = "test_value")
    test_node_list <- list(
      node1 = list(mcnode = "value1"),
      node2 = list(mcnode = "value2")
    )
    class(test_node_list) <- "mcnode_list"

    test_mcmodule <- list(node_list = test_node_list)
    class(test_mcmodule) <- "mcmodule"

    # Test with mcmodule object
    result1 <- get_mcmodule_nodes(test_mcmodule, c("node1"))
    expect_equal(length(result1), 1)
    expect_equal(result1$node1$mcnode, "value1")

    # Test with mcnode_list object
    result2 <- get_mcmodule_nodes(test_node_list, c("node2"))
    expect_equal(length(result2), 1)
    expect_equal(result2$node2$mcnode, "value2")

    # Test with invalid input
    expect_error(get_mcmodule_nodes("invalid"))

    # Test with non-existent nodes
    result3 <- get_mcmodule_nodes(test_mcmodule, c("non_existent"))
    expect_equal(length(result3), 0)

    # Test with no nodes specified
    result4 <- get_mcmodule_nodes(test_mcmodule)
    expect_equal(length(result4), 0)
  })

  test_that("eval_mcmodule dim match works", {
    #  Create pathogen data table
    transmission_data <- data.frame(
      pathogen = c("a", "b"),
      inf_dc_min = c(0.05, 0.3),
      inf_dc_max = c(0.08, 0.4)
    )

    transmission_data_keys <- list(
      transmission_data = list(
        cols = c("pathogen", "inf_dc_min", "inf_dc_max"),
        keys = c("pathogen")
      )
    )

    transmission_mctable <- data.frame(
      mcnode = c("inf_dc"),
      description = c("Probability of infection via direct contact"),
      mc_func = c("runif"),
      from_variable = c(NA),
      transformation = c(NA),
      sensi_variation = c(NA_character_)
    )

    # Test expression
    transmission_exp <- quote({
      infection_risk <- no_detect * inf_dc
    })

    # Evaluate module with previous module
    result_module <- eval_module(
      exp = c(transmission = transmission_exp),
      data = transmission_data,
      mctable = transmission_mctable,
      data_keys = transmission_data_keys,
      prev_mcmodule = imports_mcmodule
    )

    # Verify dimensions
    expect_equal(dim(result_module$node_list$no_detect$mcnode)[3], 6)
    expect_equal(dim(result_module$node_list$infection_risk$mcnode)[3], 6)

    # Verify keys are correctly combined
    expect_equal(
      result_module$node_list$infection_risk$keys,
      c("pathogen", "origin")
    )

    # Verify input tracking
    expect_equal(
      result_module$node_list$infection_risk$inputs,
      c("no_detect", "inf_dc")
    )

    # Verify no null matches in the dimension matching process
    summary <- mc_summary(result_module, "infection_risk")
    expect_equal(nrow(summary), 6)
    expect_true(all(c("pathogen", "origin") %in% names(summary)))
  })

  test_that("eval_mcmodule dim match works with agg keys", {
    #  Create pathogen data table
    transmission_data <- data.frame(
      pathogen = c("a", "b", "c"),
      inf_dc_min = c(0.05, 0.3, 0.5),
      inf_dc_max = c(0.08, 0.4, 0.6)
    )

    transmission_data_keys <- list(
      transmission_data = list(
        cols = c("pathogen", "inf_dc_min", "inf_dc_max"),
        keys = c("pathogen")
      )
    )

    transmission_mctable <- data.frame(
      mcnode = c("inf_dc"),
      description = c("Probability of infection via direct contact"),
      mc_func = c("runif"),
      from_variable = c(NA),
      transformation = c(NA),
      sensi_variation = c(NA_character_)
    )
    # Get previous module
    imports_mcmodule <- agg_totals(
      imports_mcmodule,
      "no_detect",
      agg_keys = "pathogen"
    )

    # Test expression
    transmission_exp <- quote({
      infection_risk <- no_detect_agg * inf_dc
    })

    # Evaluate module with previous module
    result_module <- eval_module(
      exp = c(transmission = transmission_exp),
      data = transmission_data,
      mctable = transmission_mctable,
      data_keys = transmission_data_keys,
      prev_mcmodule = imports_mcmodule,
      match_keys = c("pathogen", "origin"),
    )

    # Verify dimensions
    expect_equal(dim(imports_mcmodule$node_list$no_detect$mcnode)[3], 6)
    expect_equal(dim(result_module$node_list$no_detect_agg$mcnode)[3], 2)
    expect_equal(dim(result_module$node_list$infection_risk$mcnode)[3], 3)

    # Verify keys are correctly combined
    expect_equal(result_module$node_list$infection_risk$keys, c("pathogen"))

    # Verify input tracking
    expect_equal(
      result_module$node_list$infection_risk$inputs,
      c("no_detect_agg", "inf_dc")
    )

    # Verify no null matches in the dimension matching process
    summary <- mc_summary(result_module, "infection_risk")
    expect_equal(nrow(summary), 3)
    expect_true(all(c("pathogen") %in% names(summary)))
  })

  test_that("eval_mcmodule dim match works with custom match_keys", {
    #  Create test data
    contamination_data <- data.frame(
      pathogen = c("a", "b", "a", "b"),
      origin = c("nord", "nord", "nord", "nord"),
      scenario_id = c("0", "0", "heat_treatment", "heat_treatment"),
      contaminated = c(0.1, 0.5, 0.01, 0.05)
    )

    contamination_data_keys <- list(
      contamination_data = list(
        cols = c("pathogen", "origin", "contaminated"),
        keys = c("pathogen", "origin")
      )
    )

    contamination_mctable <- data.frame(
      mcnode = c("contaminated"),
      description = c("Probability of being contaminated"),
      mc_func = c(NA),
      from_variable = c(NA),
      transformation = c(NA),
      sensi_variation = c(NA_character_)
    )

    # Test expression
    contamination_exp <- quote({
      introduction_risk <- no_detect * contaminated
    })

    # Evaluate module with previous module
    result_default <- eval_module(
      exp = c(contamination = contamination_exp),
      data = contamination_data,
      mctable = contamination_mctable,
      data_keys = contamination_data_keys,
      prev_mcmodule = imports_mcmodule
    )

    # Evaluate module with previous module with CUSTOM MATCH KEYS
    result_custom <- eval_module(
      exp = c(contamination = contamination_exp),
      data = contamination_data,
      mctable = contamination_mctable,
      data_keys = contamination_data_keys,
      prev_mcmodule = imports_mcmodule,
      match_keys = c("pathogen")
    )

    # Verify dimensions
    expect_equal(dim(result_default$node_list$introduction_risk$mcnode)[3], 8)
    expect_equal(dim(result_custom$node_list$introduction_risk$mcnode)[3], 12)

    # Verify keys are correctly combined
    expect_equal(
      result_custom$node_list$introduction_risk$keys,
      c("pathogen", "origin")
    )

    # Verify input tracking
    expect_equal(
      result_custom$node_list$introduction_risk$inputs,
      c("no_detect", "contaminated")
    )

    # Verify no null matches in the dimension matching process
    summary <- mc_summary(result_custom, "introduction_risk")
    expect_equal(nrow(summary), 12)
    expect_true(all(c("pathogen", "origin") %in% names(summary)))
  })

  test_that("eval_module uses explicit keys and overwrite_keys default", {
    # data_keys provided -> overwrite_keys should default to FALSE -> keys are merged with data_keys
    test_data <- data.frame(
      pathogen = c("a", "b"),
      origin = c("o1", "o2"),
      inf_dc_min = c(0.1, 0.2),
      inf_dc_max = c(0.2, 0.3)
    )
    test_keys <- list(
      transmission = list(cols = names(test_data), keys = c("pathogen"))
    )

    test_mctable <- data.frame(
      mcnode = c("inf_dc"),
      description = c("inf"),
      mc_func = c("runif"),
      from_variable = c(NA),
      transformation = c(NA),
      sensi_variation = c(NA_character_)
    )

    res1 <- eval_module(
      exp = c(
        transmission = quote({
          infection_risk <- inf_dc
        })
      ),
      data = test_data,
      mctable = test_mctable,
      data_keys = test_keys,
      keys = c("pathogen", "origin")
    )

    expect_equal(res1$node_list$inf_dc$keys, c("pathogen", "origin"))
    expect_equal(res1$node_list$infection_risk$keys, c("pathogen", "origin"))

    # data_keys empty -> overwrite_keys should default to TRUE -> provided keys become local data_keys
    res2 <- eval_module(
      exp = c(
        transmission = quote({
          infection_risk <- inf_dc
        })
      ),
      data = test_data,
      mctable = test_mctable,
      data_keys = list(), # empty -> triggers overwrite_keys = TRUE
      keys = c("pathogen", "origin")
    )

    expect_equal(
      sort(res2$node_list$inf_dc$keys),
      sort(c("pathogen", "origin"))
    )
    expect_equal(
      sort(res2$node_list$infection_risk$keys),
      sort(c("pathogen", "origin"))
    )
  })

  test_that("eval_module handles prev_nodes with multiple data_names needing matching", {
    # Mock previous modules with different data_names
    prev_data_x <- data.frame(id = 1:3, value_x = c(0.1, 0.2, 0.3))
    prev_data_y <- data.frame(id = 1:3, value_y = c(0.2, 0.4, 0.5))
    names(prev_data_x) <- c("id", "value_x")
    names(prev_data_y) <- c("id", "value_y")

    prev_node_list <- list(
      prev_value_x = list(
        mcnode = mcdata(prev_data_x$value_x, type = "0", nvariates = 3),
        data_name = "prev_data_x",
        agg_keys = NULL,
        keep_variates = TRUE,
        type = "prev_node",
        keys = "id"
      ),
      prev_value_y = list(
        mcnode = mcdata(prev_data_y$value_y, type = "0", nvariates = 3),
        data_name = c("prev_data_x", "prev_data_y"),
        agg_keys = NULL,
        keep_variates = TRUE,
        type = "prev_node",
        keys = "id"
      )
    )

    class(prev_node_list) <- "mcnode_list"

    prev_mcmodule <- list(
      data = list(
        prev_data_x = prev_data_x,
        prev_data_y = prev_data_y
      ),
      node_list = prev_node_list
    )

    class(prev_mcmodule) <- "mcmodule"

    # Current data
    current_data <- data.frame(id = 1:3, value = c(0.01, 0.02, 0.03))
    current_exp <- quote({
      result <- prev_value_x * prev_value_y * value
    })

    mctable <- data.frame(
      mcnode = "value",
      description = "value node",
      mc_func = NA,
      from_variable = NA,
      transformation = NA,
      sensi_variation = NA_character_
    )

    # Run eval_module with both previous modules
    expect_error(
      eval_module(
        exp = c(current = current_exp),
        data = current_data,
        mctable = mctable,
        prev_mcmodule = prev_mcmodule
      ),
      "summary is needed for mcnodes with multiple data_names"
    )

    # Add Summary
    prev_mcmodule$node_list$prev_value_y$summary <- mc_summary(
      mcnode = prev_mcmodule$node_list$prev_value_y$mcnode,
      data = prev_data_y,
      keys_names = "id"
    )

    expect_message(
      result_mcmodule <- eval_module(
        exp = c(current = current_exp),
        data = current_data,
        mctable = mctable,
        prev_mcmodule = prev_mcmodule
      ),
      "Using summary to match dimensions"
    )

    expect_true("value" %in% names(result_mcmodule$node_list))
    expect_true("result" %in% names(result_mcmodule$node_list))
    expect_equal(
      result_mcmodule$node_list$result$inputs,
      c("prev_value_x", "prev_value_y", "value")
    )
    expect_equal(result_mcmodule$node_list$result$data_name, c("current_data"))
  })

  test_that("eval_module creates input nodes from data without mctable", {
    test_data <- data.frame(external_input = c(0.1, 0.2, 0.3))
    test_exp <- quote({
      result <- external_input * 2
    })

    reset_mctable()

    # When mctable is not provided, eval_module reports that inputs are created
    # from data.
    expect_message(
      result_mcmodule <- eval_module(
        exp = c(test = test_exp),
        data = test_data
      ),
      "Creating mcnodes from data"
    )

    expect_true("external_input" %in% names(result_mcmodule$node_list))
    expect_true("result" %in% names(result_mcmodule$node_list))
    expect_true(is.mcnode(result_mcmodule$node_list$external_input$mcnode))
  })

  test_that("eval_module warns when inputs are in data but not in provided mctable (non-empty mctable)", {
    test_data <- data.frame(external_input = c(0.1, 0.2, 0.3))
    test_exp <- quote({
      result <- external_input * 2
    })

    # mctable contains a different node, so external_input is missing from it
    some_mctable <- data.frame(
      mcnode = c("other_node"),
      description = c("other"),
      mc_func = c(NA),
      from_variable = c(NA),
      transformation = c(NA),
      sensi_variation = c(NA_character_),
      stringsAsFactors = FALSE
    )

    expect_message(
      result_mcmodule <- eval_module(
        exp = c(test = test_exp),
        data = test_data,
        mctable = some_mctable,
        data_keys = list()
      ),
      "The following nodes are present in data but not in the mctable: external_input"
    )

    expect_true("external_input" %in% names(result_mcmodule$node_list))
    expect_true("result" %in% names(result_mcmodule$node_list))
    expect_true(is.mcnode(result_mcmodule$node_list$external_input$mcnode))
  })

  test_that("eval_module creates input nodes from sample_design without mctable and allows empty data", {
    test_exp <- quote({
      result <- input_a + input_b
    })

    reset_mctable()

    X <- data.frame(
      input_a = c(0.1, 0.2, 0.3, 0.4),
      input_b = c(1, 2, 3, 4)
    )

    result_mcmodule <- eval_module(
      exp = c(test = test_exp),
      data = data.frame(),
      sample_design = X
    )

    expect_equal(class(result_mcmodule), "mcmodule")
    expect_true(result_mcmodule$node_list$input_a$from_sample_design)
    expect_true(result_mcmodule$node_list$input_b$from_sample_design)
    expect_null(result_mcmodule$node_list$input_a$data_name)
    expect_null(result_mcmodule$node_list$input_b$data_name)
    expect_equal(
      dim(result_mcmodule$node_list$input_a$mcnode),
      c(nrow(X), 1, 1)
    )
    expect_equal(
      dim(result_mcmodule$node_list$input_b$mcnode),
      c(nrow(X), 1, 1)
    )
    expect_equal(
      as.numeric(result_mcmodule$node_list$input_a$mcnode[, 1, 1]),
      X$input_a
    )
    expect_equal(
      as.numeric(result_mcmodule$node_list$input_b$mcnode[, 1, 1]),
      X$input_b
    )

    expect_no_error(
      eval_module(
        exp = c(test = test_exp),
        data = NULL,
        sample_design = X
      )
    )
  })

  test_that("eval_module uses global sample_design by default", {
    reset_sample_design()
    reset_mctable()

    test_exp <- quote({
      result <- input_a + input_b
    })

    X <- data.frame(
      input_a = c(0.1, 0.2, 0.3),
      input_b = c(1, 2, 3)
    )

    set_sample_design(X)

    result_mcmodule <- eval_module(
      exp = c(test = test_exp),
      data = data.frame()
    )

    expect_true(result_mcmodule$node_list$input_a$from_sample_design)
    expect_true(result_mcmodule$node_list$input_b$from_sample_design)

    reset_sample_design()
  })

  test_that("eval_module enforces input ndvar compatibility when sample_design is used", {
    test_data <- data.frame(
      other_min = 0.2,
      other_max = 0.4
    )

    test_mctable <- data.frame(
      mcnode = c("input_a", "other"),
      mc_func = c(NA, "runif"),
      description = c("A", "Other"),
      from_variable = c(NA, NA),
      sample_space = c(NA_character_, NA_character_),
      transformation = c(NA, NA),
      sensi_variation = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE
    )

    test_exp <- quote({
      result <- input_a * other
    })

    X <- data.frame(
      input_a = c(0.1, 0.2, 0.3, 0.4, 0.5),
      other = c(0.2, 0.3, 0.4, 0.5, 0.6)
    )

    result_mcmodule <- eval_module(
      exp = c(test = test_exp),
      data = test_data,
      mctable = test_mctable,
      sample_design = X
    )

    expect_equal(
      dim(result_mcmodule$node_list$input_a$mcnode),
      c(nrow(X), 1, 1)
    )
    expect_equal(dim(result_mcmodule$node_list$other$mcnode)[1], nrow(X))
    # Because 'other' is provided as a column in sample_design, it is treated
    # as coming from sample_design.
    expect_true(isTRUE(result_mcmodule$node_list$other$from_sample_design))
    expect_null(result_mcmodule$node_list$other$data_name)
  })

  test_that("eval_module keeps type 0 nodes at original dimension with sample_design", {
    test_data <- data.frame(
      fixed_value = 2
    )

    test_mctable <- data.frame(
      mcnode = c("input_a", "fixed_value"),
      mc_func = c(NA, NA),
      description = c("A", "Fixed"),
      from_variable = c(NA, NA),
      sample_space = c(NA_character_, NA_character_),
      transformation = c(NA, NA),
      sensi_variation = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE
    )

    test_exp <- quote({
      result <- input_a * fixed_value
    })

    X <- data.frame(input_a = c(0.1, 0.2, 0.3, 0.4))

    # When sample_design is provided, inputs not present as columns must be
    # either provided in sample_design or have numeric bounds in sample_space.
    expect_error(
      eval_module(
        exp = c(test = test_exp),
        data = test_data,
        mctable = test_mctable,
        sample_design = X
      ),
      "Input 'fixed_value' is missing from sample_design and has no numeric bounds in mctable\\$sample_space"
    )
  })

  test_that("eval_module errors when sample_design misses required inputs and data is empty", {
    test_exp <- quote({
      result <- input_a + input_b
    })

    test_mctable <- data.frame(
      mcnode = c("input_a", "input_b"),
      mc_func = c(NA, NA),
      description = c("A", "B"),
      from_variable = c(NA, NA),
      sample_space = c(NA_character_, NA_character_),
      transformation = c(NA, NA),
      sensi_variation = c(NA_character_, NA_character_),
      stringsAsFactors = FALSE
    )

    X <- data.frame(input_a = c(0.1, 0.2, 0.3))

    expect_error(
      eval_module(
        exp = c(test = test_exp),
        data = data.frame(),
        mctable = test_mctable,
        sample_design = X
      ),
      "Input 'input_b' is missing from sample_design and has no numeric bounds in mctable\\$sample_space"
    )
  })

  test_that("eval_module deals with mcdata() and mcstoc() functions", {
    test_data <- data.frame(
      category = c("a", "b"),
      input_a_min = c(0.1, 0.2),
      input_a_max = c(0.2, 0.3)
    )

    test_exp1 <- quote({
      input_b <- mcdata(data = c(0.5, 1.5), type = "0")
      input_c <- mcstoc(runif, min = 0, max = 1)
      ouput_ab <- mcstoc(rnorm, mean = input_b, sd = input_a)
      result <- ouput_ab + input_c
    })

    test_mctable <- data.frame(
      mcnode = c("input_a"),
      mc_func = c("runif"),
      description = c("Test input A"),
      stringsAsFactors = FALSE,
      from_variable = c(NA),
      transformation = c(NA),
      sensi_variation = c(NA_character_)
    )

    result_mcmodule <- eval_module(
      exp = c(test = test_exp1),
      data = test_data,
      mctable = test_mctable
    )

    expect_true(result_mcmodule$node_list$ouput_ab$function_call)
    expect_true(result_mcmodule$node_list$ouput_ab$created_in_exp)
    expect_equal(
      dim(result_mcmodule$node_list$ouput_ab$mcnode),
      dim(result_mcmodule$node_list$result$mcnode)
    )
    expect_equal(
      result_mcmodule$node_list$result$inputs,
      c("ouput_ab", "input_c")
    )
  })

  test_that("eval_module removes inline nvariates when sample_design is provided", {
    test_exp <- quote({
      input_b <- mcdata(data = 0.5, type = "0")
      input_c <- mcstoc(runif, min = 0, max = 1)
      input_d <- 5
      result <- input_b + input_c + sample_a + input_d
    })

    sample_design <- data.frame(sample_a = c(0.1, 0.2, 0.3))

    result_mcmodule <- eval_module(
      exp = c(test = test_exp),
      data = data.frame(),
      sample_design = sample_design
    )

    expect_equal(dim(result_mcmodule$node_list$input_b$mcnode), c(1, 1, 1))
    expect_equal(
      dim(result_mcmodule$node_list$input_c$mcnode),
      c(nrow(sample_design), 1, 1)
    )
    expect_equal(
      dim(result_mcmodule$node_list$result$mcnode),
      c(nrow(sample_design), 1, 1)
    )
    expect_true(result_mcmodule$node_list$input_b$created_in_exp)
    expect_true(is.null(result_mcmodule$node_list$input_b$from_sample_design))
    expect_true(result_mcmodule$node_list$result$from_sample_design)
  })

  test_that("eval_module works with use_variation parameter", {
    # Test basic use_variation functionality
    result_variation <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys,
      use_variation = c("h_prev", "test_sensi")
    )

    # Verify it created an mcmodule
    expect_equal(class(result_variation), "mcmodule")
    expect_true("infected" %in% names(result_variation$node_list))

    # Verify mcnodes were created with variation applied
    expect_true(is.mcnode(result_variation$node_list$infected$mcnode))
    expect_true(is.mcnode(result_variation$node_list$test_sensi$mcnode))
  })

  test_that("eval_module handles NULL defaults for OAT parameters", {
    # Test that NULL defaults work (no errors)
    result_null_defaults <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys,
      use_variation = NULL
    )

    expect_equal(class(result_null_defaults), "mcmodule")
    expect_true("infected" %in% names(result_null_defaults$node_list))
    expect_true(is.mcnode(result_null_defaults$node_list$infected$mcnode))
  })

  test_that("eval_module works with mcnode_na_rm() in expressions", {
    # Create test data with values that could produce NA or Inf
    test_data <- data.frame(
      category = c("a", "b", "c"),
      input_min = c(0.1, 0.2, 0.3),
      input_max = c(0.2, 0.3, 0.4),
      divisor = c(1, 0, 2) # zero will create Inf
    )

    test_mctable <- data.frame(
      mcnode = c("input"),
      mc_func = c("runif"),
      description = c("Test input"),
      from_variable = c(NA),
      transformation = c(NA),
      sensi_variation = c(NA_character_),
      stringsAsFactors = FALSE
    )

    test_data_keys <- list(
      test_data = list(
        cols = c("category", "input_min", "input_max", "divisor"),
        keys = c("category")
      )
    )

    # Expression that uses mcnode_na_rm to handle potential NA/Inf
    test_exp <- quote({
      ratio <- input / divisor
      # Clean the ratio by replacing NA/Inf with 0
      clean_ratio <- mcnode_na_rm(ratio)
      # Use cleaned ratio in further calculations
      result <- clean_ratio * 10
    })

    result_module <- eval_module(
      exp = c(test = test_exp),
      data = test_data,
      mctable = test_mctable,
      data_keys = test_data_keys
    )

    # Verify module was created
    expect_equal(class(result_module), "mcmodule")
    expect_true("clean_ratio" %in% names(result_module$node_list))
    expect_true("result" %in% names(result_module$node_list))

    # Verify clean_ratio has no NA or Inf values
    clean_ratio_mcnode <- result_module$node_list$clean_ratio$mcnode
    expect_false(any(is.na(clean_ratio_mcnode)))
    expect_false(any(is.infinite(clean_ratio_mcnode)))

    # Verify result also has no NA or Inf
    result_mcnode <- result_module$node_list$result$mcnode
    expect_false(any(is.na(result_mcnode)))
    expect_false(any(is.infinite(result_mcnode)))
  })

  test_that("eval_module works with mcnode_na_rm() with custom na_value", {
    # Create test data
    test_data <- data.frame(
      id = c("x", "y"),
      value_a = c(1, 2),
      value_b = c(0, 3) # zero will create Inf when used as divisor
    )

    # Expression using mcnode_na_rm with custom replacement value
    test_exp <- quote({
      ratio <- value_a / value_b
      # Replace Inf with -999 instead of default 0
      clean_ratio <- mcnode_na_rm(ratio, na_value = -999)
    })

    result_module <- eval_module(
      exp = c(test = test_exp),
      data = test_data
    )

    # Verify the custom replacement value was used
    clean_ratio_mcnode <- result_module$node_list$clean_ratio$mcnode
    expect_false(any(is.infinite(clean_ratio_mcnode)))

    # Check that Inf was replaced with -999
    expect_true(any(clean_ratio_mcnode == -999))
  })

  test_that("eval_module works with mcnode_null_rm() in expressions", {
    # Create test data
    test_data <- data.frame(
      category = c("a", "b"),
      base_value = c(0.5, 0.8)
    )

    # Expression that uses mcnode_null_rm to handle potentially missing nodes
    test_exp <- quote({
      result <- base_value * mcnode_null_rm(optional_node, null_value = 1)
    })

    result_module <- eval_module(
      exp = c(test = test_exp),
      data = test_data
    )

    # Verify module was created
    expect_equal(class(result_module), "mcmodule")
    expect_true("result" %in% names(result_module$node_list))

    # Verify result uses the default value (1)
    result_mcnode <- result_module$node_list$result$mcnode
    expect_equal(as.numeric(result_mcnode), test_data$base_value)
  })

  test_that("eval_module ignores missing prev_nodes flagged with null_rm", {
    test_data <- data.frame(
      category = c("a", "b"),
      base_value_min = c(2, 3),
      base_value_max = c(3, 4)
    )

    test_mctable <- data.frame(
      mcnode = c("base_value"),
      mc_func = c("runif"),
      description = c("Base value"),
      from_variable = c(NA),
      transformation = c(NA),
      sensi_variation = c(NA_character_),
      stringsAsFactors = FALSE
    )

    test_data_keys <- list(
      test_data = list(
        cols = c("category", "base_value_min", "base_value_max", "base_value"),
        keys = c("category")
      )
    )

    test_exp <- quote({
      result <- base_value * mcnode_null_rm(missing_prev_node, null_value = 1)
    })

    expect_no_error(
      result_module <- eval_module(
        exp = c(test = test_exp),
        data = test_data,
        mctable = test_mctable,
        data_keys = test_data_keys,
        prev_mcmodule = list(
          list(
            data = list(),
            node_list = structure(list(), class = "mcnode_list")
          ) |>
            structure(class = "mcmodule")
        )
      )
    )

    expect_true("result" %in% names(result_module$node_list))
    expect_false("missing_prev_node" %in% names(result_module$node_list))
    expect_equal(
      "missing_prev_node",
      result_module$node_list$result$null_rm_inputs
    )
  })

  test_that("eval_module works with mcnode_null_rm() returning existing node", {
    # Create test data
    test_data <- data.frame(
      category = c("x", "y", "z"),
      factor_value = c(2, 3, 4)
    )

    # Expression where mcnode_null_rm returns the existing node
    test_exp <- quote({
      existing_node <- mcdata(c(0.1, 0.2, 0.3), type = "0")
      # mcnode_null_rm should return the existing node unchanged
      safe_node <- mcnode_null_rm(existing_node)
      # Use it in calculations
      result <- factor_value * safe_node
    })

    result_module <- eval_module(
      exp = c(test = test_exp),
      data = test_data
    )

    # Verify both nodes exist
    expect_true("existing_node" %in% names(result_module$node_list))
    expect_true("safe_node" %in% names(result_module$node_list))
    expect_true("result" %in% names(result_module$node_list))

    # Verify safe_node equals existing_node
    existing_mcnode <- result_module$node_list$existing_node$mcnode
    safe_mcnode <- result_module$node_list$safe_node$mcnode
    expect_equal(safe_mcnode, existing_mcnode)
  })
})
