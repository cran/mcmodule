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
          keep_variates=FALSE
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
          key1 = c("A", "B","A", "B","A", "B"),
          key2 = c(1, 2,1, 2,1,2),
          key3 = c("red", "green","yellow","blue","orange","black"),
          value = c(10, 20)
        )
      )
    )

    # Test with specified keys
    expect_message(mc_keys(test_module, "test_node", c("key1", "key2")),"3 variates per group")

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
          key1 = c("A", "B","A", "B","A", "B"),
          key2 = c(NA, NA,NA, NA,NA,NA),
          key3 = c("red", "green","yellow","blue","orange","black"),
          value = c(10, 20)
        )
      )
    )

    # Test with specified keys
    expect_message(mc_keys(test_module, "test_node", c("key1", "key2")),"3 variates per group")

  })

  test_that("mc_match group matching works", {
    test_module <- list(
      node_list = list(
        node_x = list(
          mcnode = mcstoc(runif,
            min = mcdata(c(1, 2, 3), type = "0", nvariates = 3),
            max = mcdata(c(2, 3, 4), type = "0", nvariates = 3),
            nvariates = 3
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        node_y = list(
          mcnode = mcstoc(runif,
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
    expect_equal(dim(result$node_x_match), dim(test_module$node_list$node_x$mcnode))
    expect_equal(dim(result$node_y_match), dim(test_module$node_list$node_y$mcnode))

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
          mcnode = mcstoc(runif,
            min = mcdata(c(1, 2, 3, 4), type = "0", nvariates = 4),
            max = mcdata(c(2, 3, 4, 5), type = "0", nvariates = 4),
            nvariates = 4
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        node_y = list(
          mcnode = mcstoc(runif,
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
          mcnode = mcstoc(runif,
            min = mcdata(c(1, 2, 3), type = "0", nvariates = 3),
            max = mcdata(c(2, 3, 4), type = "0", nvariates = 3),
            nvariates = 3
          ),
          data_name = "data_x",
          keys = c("category")
        ),
        node_y = list(
          mcnode = mcstoc(runif,
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
    current_data  <-data.frame(pathogen=c("a","b","a","b"),
                               origin=c("nord","nord","nord","nord"),
                               scenario_id=c("0","0","no_product_imports","no_product_imports"),
                               contaminated=c(0.1,0.5,0.1,0.5),
                               imported=c(1,1,0.1,0.1),
                               products_n=c(1500,1500,0,0))

    current_data_keys <-list(current_data = list(cols=names(current_data), keys=c("pathogen","origin","scenario_id")))

    current_mctable  <- data.frame(mcnode = c("contaminated", "imported", "products_n"),
                                   description = c("Probability a product is contaminated", "Probability a product is imported", "Number of products"),
                                   mc_func = c(NA, NA, NA),
                                   from_variable = c(NA, NA, NA),
                                   transformation = c(NA, NA, NA),
                                   sensi_analysis = c(FALSE, FALSE, FALSE))
    current_exp<-quote({
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
    no_detect_a_keys<-mc_keys(mcmodule=module,mc_name="no_detect_a")
    expect_equal(names(no_detect_a_keys),c("scenario_id","pathogen","origin"))
    expect_equal(dim(no_detect_a_keys),c(6,3))

    imported_contaminated_keys<-mc_keys(mcmodule=module,mc_name="imported_contaminated")
    expect_equal(names(imported_contaminated_keys),c("scenario_id","pathogen","origin"))
    expect_equal(dim(imported_contaminated_keys),c(4,3))

    result<-mc_match(module, "no_detect_a", "imported_contaminated")
    expect_equal(result$keys_xy$g_row.y,c(1,NA,NA,2,NA,NA,3,4))

    # Aggregate imported_contaminated
    module<-agg_totals(module,"imported_contaminated")
    imported_contaminated_agg_keys<-mc_keys(mcmodule=module,mc_name="imported_contaminated_agg")

    expect_equal(names(imported_contaminated_agg_keys),c("scenario_id"))
    expect_equal(dim(imported_contaminated_agg_keys), c(2,1))

    result<-mc_match(module, "no_detect_a", "imported_contaminated_agg")
    expect_equal(result$keys_xy$g_row.x,c(1,2,3,4,5,6,1,2,3,4,5,6))
    expect_equal(result$keys_xy$g_row.y,c(1,1,1,1,1,1,2,2,2,2,2,2))

    # Aggregate no_detect_a
    module<-agg_totals(module,"no_detect_a")
    no_detect_a_agg_keys<-mc_keys(mcmodule=module,mc_name="no_detect_a_agg")

    expect_equal(names(no_detect_a_agg_keys),c("scenario_id"))
    expect_equal(dim(no_detect_a_agg_keys), c(1,1))

    result<-mc_match(module, "no_detect_a_agg", "imported_contaminated_agg")
    expect_equal(result$keys_xy$g_row.y,c(1,2))

    # Match mcnodes already matching
    test_sensi_keys<-mc_keys(mcmodule=module,mc_name="test_sensi")

    result<-mc_match(module, "no_detect_a", "test_sensi")
    expect_equal(result$keys_xy$g_row.y,c(1,2,3,4,5,6))

  })

  test_that("mc_match_data works", {
    # Create test data
    test_data  <- data.frame(pathogen=c("a","b"),
                             inf_dc_min=c(0.05,0.3),
                             inf_dc_max=c(0.08,0.4))

    result<-mc_match_data(imports_mcmodule,"no_detect_a", test_data)

    # Check dimensions
    expect_equal(dim(result$test_data_match),c(6,4))

    # Check new keys column names are included in the result
    expect_true(all(c("pathogen","origin")%in%names(result$test_data_match)))
    expect_true(all(c("pathogen","origin")%in%names(result$keys_xy)))

    # Check row number
    expect_equal(result$keys_xy$g_row.y,c(1,1,1,2,2,2))
  })

  test_that("mc_match_data works with custom keys_names", {
    # Create test data
    test_data  <-data.frame(pathogen=c("a","b","a","b"),
                            origin=c("nord","nord","nord","nord"),
                            scenario_id=c("0","0","no_product_imports","no_product_imports"),
                            contaminated=c(0.1,0.5,0.1,0.5),
                            imported=c(1,1,0.1,0.1),
                            products_n=c(1500,1500,0,0))

    result_default<-mc_match_data(imports_mcmodule,"no_detect_a", test_data)
    result_custom<-mc_match_data(imports_mcmodule,"no_detect_a", test_data, keys_names = c("pathogen"))

    # Check dimensions
    expect_equal(dim(result_default$test_data_match),c(8,6))
    expect_equal(dim(result_custom$test_data_match),c(12,6))

    # Check new keys column names are included in the result
    expect_true(all(c("pathogen","origin")%in%names(result_default$keys_xy)))
    expect_true(!"origin"%in%names(result_custom$keys_xy))

    # Check row number
    expect_equal(result_default$keys_xy$g_row.y,c(1,NA,NA,2,NA,NA,3,4))
    expect_equal(result_custom$keys_xy$g_row.y,c(1,1,1,2,2,2,3,3,3,4,4,4))

  })
})

