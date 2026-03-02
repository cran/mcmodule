suppressMessages({
  test_that("get_edge_table works", {
    # Create test mcmodule
    edges <- get_edge_table(imports_mcmodule)
    expect_true(all(
      names(imports_mcmodule$node_list) %in% c(edges$node_to, edges$node_from)
    ))
    expect_true(all(c("node_from", "node_to") %in% colnames(edges)))
  })

  test_that("get_node_table works", {
    nodes <- get_node_table(imports_mcmodule)
    expect_true(all(names(imports_mcmodule$node_list) %in% nodes$name))
  })

  test_that("mc_network works", {
    nodes <- visNetwork_nodes(imports_mcmodule)
    edges <- visNetwork_edges(imports_mcmodule)

    expect_true(all(
      c("id", "color", "grouping", "expression", "title") %in% colnames(nodes)
    ))
    expect_true(all(c("from", "to", "id") %in% colnames(edges)))

    # With mcnode inptus
    imports_network_1 <- mc_network(imports_mcmodule)
    expect_true(all(
      c("visNetwork", "htmlwidget") %in% class(imports_network_1)
    ))
    expect_equal(imports_network_1$x$nodes[names(nodes)], nodes)
    expect_equal(imports_network_1$x$edges[names(edges)], edges)

    # Without mcnode inptus
    imports_network_2 <- mc_network(imports_mcmodule)
    expect_true(all(imports_network_2$x$nodes$module %in% "imports"))

    # With legend + with mcnode inputs
    imports_network_3 <- mc_network(
      imports_mcmodule,
      inputs = TRUE,
      legend = TRUE
    )
    expect_equal(
      imports_network_3$x$legend$nodes$label,
      c("inputs", "in_node", "out_node")
    )

    # With legend + without mcnode inputs
    imports_network_4 <- mc_network(imports_mcmodule, legend = TRUE)
    expect_equal(
      imports_network_4$x$legend$nodes$label,
      c("in_node", "out_node")
    )

    # With custom colour_by + legend + without mcnode inputs
    imports_network_5 <- mc_network(
      imports_mcmodule,
      inputs = TRUE,
      legend = TRUE,
      color_by = "exp_name"
    )
    expect_equal(imports_network_5$x$legend$nodes$label, c("imports"))

    # With custom palette
    imports_network_6 <- mc_network(
      imports_mcmodule,
      color_pal = c("red", "green", "blue", "yellow", "orange")
    )
    expect_true(all(
      imports_network_6$x$nodes$color %in%
        c("red", "red", "red", "green", "green", "green", "green")
    ))

    # With custom palette + legend
    imports_network_7 <- mc_network(
      imports_mcmodule,
      legend = TRUE,
      color_pal = c("red", "green", "blue", "yellow", "orange")
    )
    expect_equal(
      imports_network_7$x$legend$nodes$label,
      c("in_node", "out_node")
    )

    # With custom palette + with mcnode inputs
    imports_network_8 <- mc_network(
      imports_mcmodule,
      inputs = TRUE,
      color_pal = c("red", "green", "blue", "yellow", "orange")
    )
    expect_equal(
      imports_network_8$x$nodes$color[1:11],
      c(
        in_node = "red",
        out_node = "orange",
        rep(c(in_node = "red"), 2),
        rep(c(out_node = "orange"), 3),
        rep(c(inputs_col = "yellow"), 2),
        rep(c(input_data = "green"), 1),
        rep(c(input_dataset = "blue"), 1)
      )
    )

    # With custom palette + with mcnode inputs + legend
    imports_network_9 <- mc_network(
      imports_mcmodule,
      inputs = TRUE,
      legend = TRUE,
      color_pal = c("red", "green", "blue", "yellow", "orange")
    )
    expect_equal(
      imports_network_9$x$legend$nodes$label,
      c("in_node", "input_data", "input_dataset", "inputs_col", "out_node")
    )
  })

  test_that("combined nodes mc_network works", {
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
      imported = c(1, 1, 0, 0),
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
    combined_module <- combine_modules(previous_module, current_module)

    combined_module <- at_least_one(
      combined_module,
      c("no_detect_a", "imported_contaminated"),
      name = "total"
    )

    expect_no_error(mc_network(combined_module))
  })
})
