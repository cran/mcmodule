suppressMessages({
  test_that("mc_keys for keys works", {
    # Create mock module
    test_module <- list(
      node_list = list(
        test_node = list(
          data_name = "test_data",
          keys = c("key1", "key2")
        )
      ),
      data = list(
        test_data = data.frame(
          key1 = c("A", "B"),
          key2 = c(1, 2),
          value = c(10, 20)
        )
      )
    )

    # Test with specified keys
    result <- mc_keys(test_module, "test_node", c("key1", "key2"))
    expect_equal(ncol(result), 3) # scenario_id + 2 keys
    expect_true(all(c("scenario_id", "key1", "key2") %in% names(result)))

    # Test default scenario_id
    expect_true(all(result$scenario_id == "0"))

    # Test with missing keys
    expect_error(
      mc_keys(test_module, "test_node", c("key1", "nonexistent")),
      "Columns nonexistent not found"
    )

    # Test with invalid node name
    expect_error(
      mc_keys(test_module, "nonexistent_node"),
      "not found"
    )
  })

  test_that("mc_keys for agg_keys works", {
    # Create mock module with aggregated node
    mock_agg_module <- list(
      node_list = list(
        agg_node = list(
          agg_keys = c("group"),
          summary = data.frame(
            group = c("X", "Y"),
            count = c(5, 10)
          ),
          keep_variates = FALSE
        )
      )
    )

    result <- mc_keys(mock_agg_module, "agg_node")
    expect_equal(ncol(result), 2) # scenario_id + group
    expect_true(all(c("scenario_id", "group") %in% names(result)))
  })

  test_that("mc_keys with multiple variates per group exist works", {
    # Create mock module
    test_module <- list(
      node_list = list(
        test_node = list(
          data_name = "test_data",
          keys = c("key1", "key2", "key3")
        )
      ),
      data = list(
        test_data = data.frame(
          key1 = c("A", "B", "A", "B", "A", "B"),
          key2 = c(1, 2, 1, 2, 1, 2),
          key3 = c("red", "green", "yellow", "blue", "orange", "black"),
          value = c(10, 20)
        )
      )
    )

    # Test with specified keys
    expect_message(
      mc_keys(test_module, "test_node", c("key1", "key2")),
      "3 variates per group"
    )
  })

  test_that("mc_keys with NA key works", {
    # Create mock module
    test_module <- list(
      node_list = list(
        test_node = list(
          data_name = "test_data",
          keys = c("key1", "key2", "key3")
        )
      ),
      data = list(
        test_data = data.frame(
          key1 = c("A", "B", "A", "B", "A", "B"),
          key2 = c(NA, NA, NA, NA, NA, NA),
          key3 = c("red", "green", "yellow", "blue", "orange", "black"),
          value = c(10, 20)
        )
      )
    )

    # Test with specified keys
    expect_message(
      mc_keys(test_module, "test_node", c("key1", "key2")),
      "3 variates per group"
    )
  })
  test_that("mc_keys works for sample design nodes with no keys", {
    # Create mock module with sample design node
    sample_module <- list(
      node_list = list(
        sample_node = list(
          from_sample_design = TRUE
        )
      )
    )

    result <- mc_keys(sample_module, "sample_node")
    expect_equal(ncol(result), 1) # Only scenario_id
    expect_true(all(result$scenario_id == "0"))
  })

  test_that("mc_keys works for sample design nodes with keys", {
    # Create mock module with sample design node that has keys
    sample_module <- list(
      node_list = list(
        sample_node = list(
          from_sample_design = TRUE,
          data_name = "sample_data",
          keys = c("key1", "key2")
        )
      ),
      data = list(
        sample_data = data.frame(
          key1 = c("A", "B"),
          key2 = c(1, 2)
        )
      )
    )

    result <- mc_keys(sample_module, "sample_node")
    expect_equal(ncol(result), 1) # Only scenario_id
    expect_true(all(result$scenario_id == "0"))
  })
  test_that("mc_keys works for output nodes created with input nodes from sample design", {
    # Evaluate module with sample design node and output node
    reset_mctable()
    sample_design <- data.frame(
      input_a = c(1, 2),
      input_b = c(3, 4),
      stringsAsFactors = FALSE
    )

    test_data <- data.frame(
      input_a = c(1, 2),
      input_b = c(3, 4),
      category = c("A", "B"),
      stringsAsFactors = FALSE
    )

    test_data_keys <- list(
      sample_data = list(
        cols = names(test_data),
        keys = c("category")
      )
    )

    test_module <- eval_module(
      exp = c(
        sample = quote({
          output_node <- input_a + input_b
        })
      ),
      sample_design = sample_design,
      data = test_data,
      data_keys = test_data_keys
    )

    result <- mc_keys(test_module, "output_node")
    expect_true(test_module$node_list$output_node$from_sample_design)
    expect_equal(nrow(result), 1) # Only one variate
    expect_equal(ncol(result), 1) # Only scenario_id
    expect_true(all(result$scenario_id == "0"))
  })

  test_that("mc_match group matching works", {
    test_module <- list(
      node_list = list(
        node_x = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(1, 2, 3), type = "0", nvariates = 3),
            max = mcdata(c(2, 3, 4), type = "0", nvariates = 3),
            nvariates = 3
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        node_y = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(5, 6, 7), type = "0", nvariates = 3),
            max = mcdata(c(6, 7, 8), type = "0", nvariates = 3),
            nvariates = 3
          ),
          data_name = "data_y",
          keys = c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B", "C")
        ),
        data_y = data.frame(
          category = c("B", "C", "A")
        )
      )
    )

    result <- mc_match(test_module, "node_x", "node_y")

    # Test dimensions
    expect_equal(
      dim(result$node_x_match),
      dim(test_module$node_list$node_x$mcnode)
    )
    expect_equal(
      dim(result$node_y_match),
      dim(test_module$node_list$node_y$mcnode)
    )

    # Test that categories are matched correctly
    expect_equal(result$keys_xy$category, test_module$data$data_x$category)

    # Verify expected keys_xy
    expect_equal(result$keys_xy$category, c("A", "B", "C"))
    expect_equal(result$keys_xy$scenario_id, c("0", "0", "0"))
  })

  test_that("mc_match scenario matching works", {
    test_module <- list(
      node_list = list(
        node_x = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(1, 2, 3, 4), type = "0", nvariates = 4),
            max = mcdata(c(2, 3, 4, 5), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        node_y = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(5, 6, 7, 8), type = "0", nvariates = 4),
            max = mcdata(c(6, 7, 8, 9), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "data_y",
          keys = c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "1", "1")
        ),
        data_y = data.frame(
          category = c("A", "B", "A", "B"),
          scenario_id = c("0", "0", "2", "2")
        )
      )
    )

    result <- mc_match(test_module, "node_x", "node_y")

    # Check scenario matching logic
    expect_equal(nrow(result$keys_xy), 6)

    # Verify dimensions of matched nodes
    expect_equal(dim(result$node_x_match)[2], dim(result$node_y_match)[2])

    # Verify expected keys_xy
    expect_equal(result$keys_xy$category, c("A", "B", "A", "B", "A", "B"))
    expect_equal(result$keys_xy$scenario_id, c("0", "0", "1", "1", "2", "2"))
  })

  test_that("mc_match null matching works", {
    test_module <- list(
      node_list = list(
        node_x = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(1, 2, 3), type = "0", nvariates = 3),
            max = mcdata(c(2, 3, 4), type = "0", nvariates = 3),
            nvariates = 3
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        node_y = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(5, 6, 7), type = "0", nvariates = 3),
            max = mcdata(c(6, 7, 8), type = "0", nvariates = 3),
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

    result <- mc_match(test_module, "node_x", "node_y")

    # Verify dimensions of matched nodes
    expect_equal(dim(result$node_x_match)[2], dim(result$node_y_match)[2])

    # Verify expected keys_xy
    expect_equal(result$keys_xy$category, c("A", "B", "C", "B", "B"))
    expect_equal(result$keys_xy$scenario_id, c("0", "0", "0", "1", "2"))
  })

  test_that("wif_match works", {
    # Test data
    x <- data.frame(
      category = c("a", "b", "a", "b"),
      scenario_id = c(0, 0, 1, 1),
      value = 1:4
    )

    y <- data.frame(
      category = c("a", "b", "a", "b"),
      scenario_id = c(0, 0, 2, 2),
      value = 5:8
    )

    # Automatic matching
    result <- wif_match(x, y)
    expect_equal(result$x$scenario_id, result$y$scenario_id)
    expect_equal(result$x$scenario_id, c(0, 0, 1, 1, 2, 2))
    expect_equal(result$x$value, c(1, 2, 3, 4, 1, 2))
    expect_equal(result$y$value, c(5, 6, 5, 6, 7, 8))

    # Match by type
    result_by <- wif_match(x, y, "category")
    expect_equal(result_by$x$scenario_id, result_by$y$scenario_id)

    # Test error on unmatched groups
    y_bad <- data.frame(
      category = c("a", "c", "a", "c"),
      scenario_id = c(0, 0, 2, 2),
      value = 5:8
    )
    expect_error(wif_match(x, y_bad), "Groups not found")
    expect_error(wif_match(x, y_bad, "category"), "Groups not found")
  })

  test_that("mc_match of agg_nodes works", {
    #  Create previous_module
    previous_module <- eval_module(
      exp = c(imports = imports_exp),
      data = imports_data,
      mctable = imports_mctable,
      data_keys = imports_data_keys
    )

    #  Create current_module
    current_data <- data.frame(
      pathogen = c("a", "b", "a", "b"),
      origin = c("nord", "nord", "nord", "nord"),
      scenario_id = c("0", "0", "no_product_imports", "no_product_imports"),
      contaminated = c(0.1, 0.5, 0.1, 0.5),
      imported = c(1, 1, 0.1, 0.1),
      products_n = c(1500, 1500, 0, 0)
    )

    current_data_keys <- list(
      current_data = list(
        cols = names(current_data),
        keys = c("pathogen", "origin", "scenario_id")
      )
    )

    current_mctable <- data.frame(
      mcnode = c("contaminated", "imported", "products_n"),
      description = c(
        "Probability a product is contaminated",
        "Probability a product is imported",
        "Number of products"
      ),
      mc_func = c(NA, NA, NA),
      from_variable = c(NA, NA, NA),
      transformation = c(NA, NA, NA),
      sensi_analysis = c(FALSE, FALSE, FALSE)
    )
    current_exp <- quote({
      imported_contaminated <- contaminated * imported
    })

    current_module <- eval_module(
      exp = c(current = current_exp),
      data = current_data,
      mctable = current_mctable,
      data_keys = current_data_keys
    )

    # Combine modules
    module <- combine_modules(previous_module, current_module)

    # Match output nodes in both modules
    no_detect_keys <- mc_keys(mcmodule = module, mc_name = "no_detect")
    expect_equal(
      names(no_detect_keys),
      c("scenario_id", "pathogen", "origin")
    )
    expect_equal(dim(no_detect_keys), c(6, 3))

    imported_contaminated_keys <- mc_keys(
      mcmodule = module,
      mc_name = "imported_contaminated"
    )
    expect_equal(
      names(imported_contaminated_keys),
      c("scenario_id", "pathogen", "origin")
    )
    expect_equal(dim(imported_contaminated_keys), c(4, 3))

    result <- mc_match(module, "no_detect", "imported_contaminated")
    expect_equal(result$keys_xy$g_row.y, c(1, NA, NA, 2, NA, NA, 3, 4))

    # Aggregate imported_contaminated
    module <- agg_totals(module, "imported_contaminated")
    imported_contaminated_agg_keys <- mc_keys(
      mcmodule = module,
      mc_name = "imported_contaminated_agg"
    )

    expect_equal(names(imported_contaminated_agg_keys), c("scenario_id"))
    expect_equal(dim(imported_contaminated_agg_keys), c(2, 1))

    result <- mc_match(module, "no_detect", "imported_contaminated_agg")
    expect_equal(result$keys_xy$g_row.x, c(1, 2, 3, 4, 5, 6, 1, 2, 3, 4, 5, 6))
    expect_equal(result$keys_xy$g_row.y, c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2))

    # Aggregate no_detect
    module <- agg_totals(module, "no_detect")
    no_detect_agg_keys <- mc_keys(
      mcmodule = module,
      mc_name = "no_detect_agg"
    )

    expect_equal(names(no_detect_agg_keys), c("scenario_id"))
    expect_equal(dim(no_detect_agg_keys), c(1, 1))

    result <- mc_match(module, "no_detect_agg", "imported_contaminated_agg")
    expect_equal(result$keys_xy$g_row.y, c(1, 2))

    # Match mcnodes already matching
    test_sensi_keys <- mc_keys(mcmodule = module, mc_name = "test_sensi")

    result <- mc_match(module, "no_detect", "test_sensi")
    expect_equal(result$keys_xy$g_row.y, c(1, 2, 3, 4, 5, 6))
  })

  test_that("mc_match_data works", {
    # Create test data
    test_data <- data.frame(
      pathogen = c("a", "b"),
      inf_dc_min = c(0.05, 0.3),
      inf_dc_max = c(0.08, 0.4)
    )

    result <- mc_match_data(imports_mcmodule, "no_detect", test_data)

    # Check dimensions
    expect_equal(dim(result$test_data_match), c(6, 4))

    # Check new keys column names are included in the result
    expect_true(all(c("pathogen", "origin") %in% names(result$test_data_match)))
    expect_true(all(c("pathogen", "origin") %in% names(result$keys_xy)))

    # Check row number
    expect_equal(result$keys_xy$g_row.y, c(1, 1, 1, 2, 2, 2))
  })

  test_that("mc_match_data works with custom keys_names", {
    # Create test data
    test_data <- data.frame(
      pathogen = c("a", "b", "a", "b"),
      origin = c("nord", "nord", "nord", "nord"),
      scenario_id = c("0", "0", "no_product_imports", "no_product_imports"),
      contaminated = c(0.1, 0.5, 0.1, 0.5),
      imported = c(1, 1, 0.1, 0.1),
      products_n = c(1500, 1500, 0, 0)
    )

    result_default <- mc_match_data(imports_mcmodule, "no_detect", test_data)
    result_custom <- mc_match_data(
      imports_mcmodule,
      "no_detect",
      test_data,
      keys_names = c("pathogen")
    )

    # Check dimensions
    expect_equal(dim(result_default$test_data_match), c(8, 6))
    expect_equal(dim(result_custom$test_data_match), c(12, 6))

    # Check new keys column names are included in the result
    expect_true(all(c("pathogen", "origin") %in% names(result_default$keys_xy)))
    expect_true(!"origin" %in% names(result_custom$keys_xy))

    # Check row number
    expect_equal(result_default$keys_xy$g_row.y, c(1, NA, NA, 2, NA, NA, 3, 4))
    expect_equal(
      result_custom$keys_xy$g_row.y,
      c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4)
    )
  })

  test_that("mc_match handles sample_design nodes without keys or data_name", {
    reset_mctable()

    sample_design <- data.frame(
      input_a = c(0.1, 0.2, 0.3),
      input_b = c(1, 2, 3),
      stringsAsFactors = FALSE
    )

    sample_module <- eval_module(
      exp = c(
        sample = quote({
          result <- input_a + input_b
        })
      ),
      data = data.frame(),
      sample_design = sample_design
    )

    expect_true(isTRUE(sample_module$node_list$input_a$from_sample_design))
    expect_true(isTRUE(sample_module$node_list$input_b$from_sample_design))
    expect_null(sample_module$node_list$input_a$data_name)
    expect_null(sample_module$node_list$input_b$data_name)
    expect_null(sample_module$node_list$input_a$keys)
    expect_null(sample_module$node_list$input_b$keys)

    result <- mc_match(sample_module, "input_a", "input_b")

    expect_length(result, 3)
    expect_equal(dim(result[[1]]), c(3, 1, 1))
    expect_equal(dim(result[[2]]), c(3, 1, 1))
  })

  test_that("mc_match recycles sample_design nodes to higher variate nodes", {
    reset_mctable()

    sample_design <- data.frame(
      input_a = c(0.1, 0.2, 0.3),
      stringsAsFactors = FALSE
    )

    sample_module <- eval_module(
      exp = c(
        sample = quote({
          result <- input_a
        })
      ),
      data = data.frame(),
      sample_design = sample_design
    )

    regular_module <- eval_module(
      exp = c(
        regular = quote({
          ref_result <- ref_input
        })
      ),
      data = data.frame(ref_input = c(10, 20, 30)),
      data_keys = list()
    )

    module <- combine_modules(sample_module, regular_module)

    result <- mc_match(module, "input_a", "ref_input")

    expect_length(result, 3)
    expect_equal(dim(result[[1]])[3], 3)
    expect_equal(dim(result[[2]])[3], 3)
  })

  test_that("mc_match_data handles sample_design nodes without keys", {
    reset_mctable()

    sample_design <- data.frame(
      input_a = c(0.1, 0.2, 0.3),
      stringsAsFactors = FALSE
    )

    sample_module <- eval_module(
      exp = list(
        sample = quote({
          result <- input_a
        })
      ),
      data = data.frame(),
      sample_design = sample_design
    )

    test_data <- data.frame(
      measure = c(10, 20, 30),
      category = c("A", "B", "C"),
      stringsAsFactors = FALSE
    )

    result <- mc_match_data(sample_module, "input_a", test_data)

    expect_length(result, 3)
    expect_equal(dim(result[[1]])[3], 3)
    expect_equal(nrow(result[[2]]), 3)
  })

  test_that("mc_match_data handles sample_design nodes with keys", {
    sample_design <- data.frame(
      input_a = c(0.1, 0.2, 0.3),
      stringsAsFactors = FALSE
    )

    data_test <- data.frame(
      input_a = c(0.1, 0.2, 0.3),
      category = c("A", "B", "C"),
      stringsAsFactors = FALSE
    )

    mctable_test <- data.frame(
      mcnode = "input_a",
      description = "Sample design input",
      mc_func = NA,
      from_variable = NA,
      transformation = NA,
      sensi_analysis = FALSE
    )

    data_keys_test <- list(
      data_test = list(
        cols = names(data_test),
        keys = c("category")
      )
    )

    sample_module <- eval_module(
      exp = list(
        sample = quote({
          result <- input_a + 1
        })
      ),
      data = data_test,
      data_keys = data_keys_test,
      sample_design = sample_design,
      mctable = mctable_test
    )

    data_test_b <- data.frame(
      count = c(100, 200, 300),
      stringsAsFactors = FALSE
    )

    result <- mc_match_data(sample_module, "input_a", data_test_b)

    expect_length(result, 3)
    expect_equal(dim(result[[1]])[3], 3)
    expect_equal(nrow(result[[2]]), 3)
  })

  test_that("mc_match errors when baseline scenario '0' is missing key combinations", {
    bad_module <- list(
      node_list = list(
        node_x = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(1, 2), type = "0", nvariates = 2),
            max = mcdata(c(2, 3), type = "0", nvariates = 2),
            nvariates = 2
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        node_y = list(
          mcnode = mcstoc(
            runif,
            min = mcdata(c(5, 6), type = "0", nvariates = 2),
            max = mcdata(c(6, 7), type = "0", nvariates = 2),
            nvariates = 2
          ),
          data_name = "data_y",
          keys = c("category")
        )
      ),
      data = list(
        # baseline has only A in scenario "0"
        data_x = data.frame(category = c("A", "B"), scenario_id = c("0", "0")),
        # B appears only in scenario "1" (missing from baseline)
        data_y = data.frame(category = c("A", "B"), scenario_id = c("0", "1"))
      )
    )
    expect_error(
      mc_match(bad_module, "node_x", "node_y"),
      "Baseline scenario '0' missing key combinations"
    )
  })

  test_that("mc_keys returns filtered dimensions for filter nodes", {
    test_module <- list(
      node_list = list(
        p_1 = list(
          mcnode = mc2d::mcstoc(
            runif,
            min = mc2d::mcdata(
              c(0.1, 0.2, 0.3, 0.4),
              type = "0",
              nvariates = 4
            ),
            max = mc2d::mcdata(
              c(0.2, 0.3, 0.4, 0.5),
              type = "0",
              nvariates = 4
            ),
            nvariates = 4
          ),
          data_name = "test_data",
          keys = c("group", "scenario_id")
        )
      ),
      data = list(
        test_data = data.frame(
          group = c("A", "A", "B", "B"),
          scenario_id = c("0", "0", "0", "0")
        )
      )
    )

    filtered <- mc_filter(
      mcmodule = test_module,
      mc_name = "p_1",
      group == "A",
      suffix = "flt"
    )

    keys_df <- mc_keys(filtered, "p_1_flt")

    expect_equal(nrow(keys_df), dim(filtered$node_list$p_1_flt$mcnode)[3])
    expect_equal(nrow(keys_df), 2)
    expect_equal(colnames(keys_df), c("scenario_id", "group"))
  })

  test_that("mc_match with match_scenario=TRUE (default) maintains default behavior", {
    test_module <- list(
      node_list = list(
        node_x = list(
          mcnode = mcdata(c(10, 20, 30), type = "0", nvariates = 3),
          data_name = "data_x",
          keys = c("category")
        ),
        node_y = list(
          mcnode = mcdata(c(15, 25, 35), type = "0", nvariates = 3),
          data_name = "data_y",
          keys = c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B", "C"),
          scenario_id = c("0", "0", "0"),
          stringsAsFactors = FALSE
        ),
        data_y = data.frame(
          category = c("A", "B", "C"),
          scenario_id = c("0", "0", "0"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Default behavior - scenario_id is excluded from matching keys
    result <- mc_match(test_module, "node_x", "node_y", match_scenario = TRUE)

    # Result returns matched mcnodes, not full module
    expect_equal(length(result), 3) # mcnode_x_match, mcnode_y_match, keys_xy
    expect_equal(dim(result[[1]])[3], 3) # node_x_match
    expect_equal(dim(result[[2]])[3], 3) # node_y_match

    # Verify scenario_id is in keys_xy
    expect_true("scenario_id" %in% names(result$keys_xy))
  })

  test_that("mc_match with match_scenario=FALSE enables cross-scenario matching", {
    # Create baseline node (scenario 0 only)
    baseline_module <- list(
      node_list = list(
        baseline_node = list(
          mcnode = mcdata(c(10, 20), type = "0", nvariates = 2),
          data_name = "baseline_data",
          keys = c("category")
        )
      ),
      data = list(
        baseline_data = data.frame(
          category = c("A", "B"),
          scenario_id = c("0", "0"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Create what-if nodes (scenarios 1 and 2 only)
    whatif_module <- list(
      node_list = list(
        whatif_node = list(
          mcnode = mcdata(c(15, 25, 12, 22), type = "0", nvariates = 4),
          data_name = "whatif_data",
          keys = c("category")
        )
      ),
      data = list(
        whatif_data = data.frame(
          category = c("A", "B", "A", "B"),
          scenario_id = c("1", "1", "2", "2"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Add whatif_node to baseline_module for mc_match
    baseline_module$node_list$whatif_node <- whatif_module$node_list$whatif_node
    baseline_module$data$whatif_data <- whatif_module$data$whatif_data

    # Cross-scenario matching - baseline should match with scenarios 1 and 2
    result <- mc_match(
      baseline_module,
      "baseline_node",
      "whatif_node",
      match_scenario = FALSE
    )

    # Baseline node should be expanded to match all what-if combinations
    expect_equal(dim(result[[1]])[3], 4) # 2 baseline categories × 2 what-if scenarios = 4
    expect_equal(dim(result[[2]])[3], 4)

    # Check that keys_xy shows the cross-scenario matching
    expect_true("scenario_id" %in% names(result$keys_xy))
    expect_true(all(c("1", "2") %in% result$keys_xy$scenario_id))
  })

  test_that("mc_match scenario_id becomes matching key when match_scenario=FALSE", {
    test_module <- list(
      node_list = list(
        node_x = list(
          mcnode = mcdata(c(10, 20, 15, 25), type = "0", nvariates = 4),
          data_name = "data_x",
          keys = c("group")
        ),
        node_y = list(
          mcnode = mcdata(c(100, 200, 150, 250), type = "0", nvariates = 4),
          data_name = "data_y",
          keys = c("group")
        )
      ),
      data = list(
        data_x = data.frame(
          group = c("A", "B", "A", "B"),
          scenario_id = c("1", "1", "2", "2"),
          stringsAsFactors = FALSE
        ),
        data_y = data.frame(
          group = c("A", "B", "A", "B"),
          scenario_id = c("1", "1", "2", "2"),
          stringsAsFactors = FALSE
        )
      )
    )

    # With match_scenario=FALSE, nodes should match by both group AND scenario_id
    result <- mc_match(test_module, "node_x", "node_y", match_scenario = FALSE)

    # Should maintain 4 variates (exact match on both keys)
    expect_equal(dim(result[[1]])[3], 4) # node_x_match
    expect_equal(dim(result[[2]])[3], 4) # node_y_match

    # Verify scenario_id appears in the keys_xy
    expect_true("scenario_id" %in% names(result$keys_xy))

    # Check that scenario_id values are preserved correctly
    expect_true(all(c("1", "2") %in% result$keys_xy$scenario_id))
  })

  test_that("mc_match maintains backward compatibility with default parameters", {
    test_module <- list(
      node_list = list(
        node_x = list(
          mcnode = mcdata(c(10, 20), type = "0", nvariates = 2),
          data_name = "data_x",
          keys = c("category")
        ),
        node_y = list(
          mcnode = mcdata(c(30, 40), type = "0", nvariates = 2),
          data_name = "data_y",
          keys = c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B"),
          scenario_id = c("0", "0"),
          stringsAsFactors = FALSE
        ),
        data_y = data.frame(
          category = c("A", "B"),
          scenario_id = c("0", "0"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Call without match_scenario parameter (should use default TRUE)
    result_default <- mc_match(test_module, "node_x", "node_y")
    result_explicit <- mc_match(
      test_module,
      "node_x",
      "node_y",
      match_scenario = TRUE
    )

    # Results should be identical
    expect_equal(
      dim(result_default[[1]]),
      dim(result_explicit[[1]])
    )
    expect_equal(
      dim(result_default[[2]]),
      dim(result_explicit[[2]])
    )
    expect_equal(nrow(result_default$keys_xy), nrow(result_explicit$keys_xy))
  })

  test_that("mc_match_data with match_scenario=TRUE maintains default behavior", {
    test_module <- list(
      node_list = list(
        node_x = list(
          mcnode = mcdata(c(10, 20), type = "0", nvariates = 2),
          data_name = "data_x",
          keys = c("category")
        )
      ),
      data = list(
        data_x = data.frame(
          category = c("A", "B"),
          scenario_id = c("0", "0"),
          stringsAsFactors = FALSE
        )
      )
    )

    new_data <- data.frame(
      category = c("A", "B"),
      value = c(100, 200),
      scenario_id = c("0", "0"),
      stringsAsFactors = FALSE
    )

    result <- mc_match_data(
      test_module,
      "node_x",
      new_data,
      match_scenario = TRUE
    )

    expect_equal(length(result), 3) # mcnode_match, new_data_match, keys_xy
    expect_equal(dim(result[[1]])[3], 2)
  })

  test_that("mc_match_data with match_scenario=FALSE enables cross-scenario matching", {
    test_module <- list(
      node_list = list(
        node_baseline = list(
          mcnode = mcdata(c(10, 20), type = "0", nvariates = 2),
          data_name = "data_baseline",
          keys = c("category")
        )
      ),
      data = list(
        data_baseline = data.frame(
          category = c("A", "B"),
          scenario_id = c("0", "0"),
          stringsAsFactors = FALSE
        )
      )
    )

    # Data with different scenarios
    new_data <- data.frame(
      category = c("A", "B", "A", "B"),
      value = c(100, 200, 150, 250),
      scenario_id = c("1", "1", "2", "2"),
      stringsAsFactors = FALSE
    )

    result <- mc_match_data(
      test_module,
      "node_baseline",
      new_data,
      match_scenario = FALSE
    )

    # Baseline should be expanded to match all scenario combinations
    expect_equal(dim(result[[1]])[3], 4) # mcnode_match
    expect_equal(nrow(result$new_data_match), 4)

    # Verify cross-scenario matching in keys_xy
    expect_true("scenario_id" %in% names(result$keys_xy))
    expect_true(all(c("1", "2") %in% result$keys_xy$scenario_id))
  })
})
