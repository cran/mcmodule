suppressMessages({
  test_that("get_node_list works (basic use)", {
    # Create test data
    test_exp <- quote({
      result <- input_a * input_b
      final <- result + prev_value
    })

    test_mctable <- data.frame(
      mcnode = c("input_a", "input_b"),
      mc_func = c("runif", "rnorm"),
      description = c("Test input A", "Test input B")
    )

    test_data_keys <- list(
      test_data = list(
        cols = c(
          "x",
          "input_a_min",
          "input_a_max",
          "input_b_mean",
          "input_b_sd"
        ),
        keys = c("x")
      )
    )

    # Run function
    node_list <- get_node_list(
      exp = test_exp,
      mctable = test_mctable,
      data_keys = test_data_keys
    )

    # Test structure
    expect_s3_class(node_list, "mcnode_list")

    # Test input nodes
    expect_true("input_a" %in% names(node_list))
    expect_true("input_b" %in% names(node_list))
    expect_equal(node_list$input_a$type, "in_node")
    expect_equal(node_list$input_b$type, "in_node")

    # Test output nodes
    expect_true("result" %in% names(node_list))
    expect_true("final" %in% names(node_list))
    expect_equal(node_list$result$type, "out_node")
    expect_equal(node_list$final$type, "out_node")

    # Test relationships
    expect_equal(sort(node_list$result$inputs), sort(c("input_a", "input_b")))
    expect_equal(sort(node_list$final$inputs), sort(c("result", "prev_value")))

    # Test keys
    expect_equal(node_list$result$keys, "x")

    # Test error cases
    expect_error(get_node_list("not an expression"))
    expect_error(get_node_list(quote(not_valid <- 1)))
  })

  test_that("get_node_list works for transformed inputs", {
    # Create test data
    test_exp <- quote({
      result <- input_a * input_b_yes
      final <- result + 1
    })

    test_mctable <- data.frame(
      mcnode = c("input_a", "input_b_yes"),
      mc_func = c("runif", NA),
      from_variable = c(NA, "input_b"),
      transformation = c(NA, "value=='yes'"),
      description = c("Test input A", "Test input B")
    )

    test_data_keys <- list(
      test_data = list(
        cols = c("x", "input_a_min", "input_a_max", "input_b"),
        keys = c("x")
      )
    )

    # Run function
    node_list <- get_node_list(
      exp = test_exp,
      mctable = test_mctable,
      data_keys = test_data_keys
    )

    expect_equal(node_list$input_b_yes$type, "in_node")
    expect_equal(node_list$input_b_yes$keys, "x")
    expect_equal(node_list$input_b_yes$inputs_col, "input_b")
    expect_equal(node_list$input_b_yes$input_dataset, "test_data")
  })

  test_that("get_node_list works with several data tables", {
    # Create test data
    test_exp <- quote({
      result <- input_a * input_b * input_c
      final <- result + prev_value
    })

    test_mctable <- data.frame(
      mcnode = c("input_a", "input_b", "input_c"),
      mc_func = c("runif", "rnorm", NA),
      description = c("Test input A", "Test input B", "Test input C")
    )

    test_data_keys <- list(
      test_data_x = list(
        cols = c(
          "x",
          "input_a_min",
          "input_a_max",
          "input_b_mean",
          "input_b_sd"
        ),
        keys = c("x")
      ),
      test_data_y = list(
        cols = c("y", "input_c"),
        keys = c("y")
      )
    )

    # Run function
    node_list <- get_node_list(
      exp = test_exp,
      mctable = test_mctable,
      data_keys = test_data_keys
    )

    # Test keys
    expect_equal(node_list$result$keys, c("x", "y"))
  })

  test_that("get_node_list works with functions and scalars", {
    # Create test data
    test_exp <- quote({
      output_1 <- (input_a * input_b) / output_1
      output_2 <- mcnode_na_rm(output_1, 0)
      level <- 2
      output_3 <- (1 + output_2) * prev_value
      output_4 <- output_3 * level
    })

    test_mctable <- data.frame(
      mcnode = c("input_a", "input_b", "input_c"),
      mc_func = c("runif", "rnorm", NA),
      description = c("Test input A", "Test input B", "Test input C")
    )

    test_data_keys <- list(
      test_data_x = list(
        cols = c(
          "x",
          "input_a_min",
          "input_a_max",
          "input_b_mean",
          "input_b_sd"
        ),
        keys = c("x")
      ),
      test_data_y = list(
        cols = c("y", "input_c"),
        keys = c("y")
      )
    )

    # Run function
    node_list <- get_node_list(
      exp = test_exp,
      mctable = test_mctable,
      data_keys = test_data_keys
    )

    expect_true(node_list$output_2$function_call)
    expect_true(node_list$output_2$na_rm)
    expect_equal(node_list$level$type, c("scalar"))
    expect_equal(node_list$output_3$inputs, c("output_2", "prev_value"))
    expect_equal(node_list$output_3$node_exp, c("(1 + output_2) * prev_value"))
    expect_equal(node_list$output_4$inputs, c("output_3", "level"))
  })

  test_that("get_node_list sets null_rm on in_nodes wrapped by mcnode_null_rm", {
    test_exp <- quote({
      output_1 <- mcnode_null_rm(input_a, null_value = 0)
      output_2 <- mcnode_na_rm(output_1, 0)
    })

    test_mctable <- data.frame(
      mcnode = c("input_a"),
      mc_func = c("runif"),
      description = c("Test input A"),
      stringsAsFactors = FALSE
    )

    test_data_keys <- list(
      test_data = list(
        cols = c("x", "input_a_min", "input_a_max"),
        keys = c("x")
      )
    )

    node_list <- get_node_list(
      exp = test_exp,
      mctable = test_mctable,
      data_keys = test_data_keys
    )

    expect_true("input_a" %in% names(node_list))
    expect_equal(node_list$input_a$type, "in_node")
    expect_true(isTRUE(node_list$input_a$null_rm))

    expect_true("output_2" %in% names(node_list))
    expect_equal(node_list$output_2$type, "out_node")
    expect_true(isTRUE(node_list$output_2$na_rm))
  })

  test_that("get_node_list works with functions with naming conflicts", {
    exp <- c("something", "called", "exp", "not", "a", "function")

    # Create test data
    test_exp <- quote({
      output_1 <- (input_a * input_b) / output_1
      output_2 <- base::exp(output_1) #Exp being a funciont
      output_3 <- (1 + output_2) * prev_value
      output_4 <- output_3 * level
    })

    test_mctable <- data.frame(
      mcnode = c("input_a", "input_b", "input_c"),
      mc_func = c("runif", "rnorm", NA),
      description = c("Test input A", "Test input B", "Test input C")
    )

    test_data_keys <- list(
      test_data_x = list(
        cols = c(
          "x",
          "input_a_min",
          "input_a_max",
          "input_b_mean",
          "input_b_sd"
        ),
        keys = c("x")
      ),
      test_data_y = list(
        cols = c("y", "input_c"),
        keys = c("y")
      )
    )

    # Run function
    node_list <- get_node_list(
      exp = test_exp,
      mctable = test_mctable,
      data_keys = test_data_keys
    )

    expect_true(node_list$output_2$function_call)
    expect_equal(node_list$output_2$inputs, c("output_1"))
    expect_equal(node_list$output_3$inputs, c("output_2", "prev_value"))
    expect_equal(node_list$output_3$node_exp, c("(1 + output_2) * prev_value"))
    expect_equal(node_list$output_4$inputs, c("output_3", "level"))
  })

  test_that("get_node_list treats pkg::fn() like fn() for inputs", {
    test_exp <- quote({
      output_1 <- input_a + 1
      output_2 <- stats::pnorm(output_1)
      output_3 <- base::exp(output_2)
      output_4 <- output_3 + prev_value
    })

    test_mctable <- data.frame(
      mcnode = c("input_a"),
      mc_func = c("runif"),
      description = c("Test input A"),
      stringsAsFactors = FALSE
    )

    node_list <- get_node_list(
      exp = test_exp,
      mctable = test_mctable,
      data_keys = list(
        test_data = list(
          cols = c("input_a_min", "input_a_max"),
          keys = character()
        )
      )
    )

    expect_true(node_list$output_2$function_call)
    expect_true(node_list$output_3$function_call)
    expect_equal(node_list$output_2$inputs, c("output_1"))
    expect_equal(node_list$output_3$inputs, c("output_2"))
    expect_equal(node_list$output_4$inputs, c("output_3", "prev_value"))
    expect_false("stats" %in% node_list$output_2$inputs)
    expect_false("base" %in% node_list$output_3$inputs)
  })

  test_that("get_node_list uses explicit keys and merges them with data_keys when present", {
    test_exp <- quote({
      result <- input_a * 2
    })

    test_mctable <- data.frame(
      mcnode = c("input_a"),
      mc_func = c("runif"),
      description = c("Test input A"),
      stringsAsFactors = FALSE
    )

    # Case 1: data_keys present -> merged with provided keys
    test_data_keys <- list(
      test_data = list(
        cols = c("x", "input_a_min", "input_a_max"),
        keys = c("x")
      )
    )

    node_list1 <- get_node_list(
      exp = test_exp,
      mctable = test_mctable,
      data_keys = test_data_keys,
      keys = c("y")
    )

    expect_equal(node_list1$input_a$keys, c("x", "y"))

    # Case 2: no data_keys (empty) -> provided keys applied as-is
    node_list2 <- get_node_list(
      exp = test_exp,
      mctable = test_mctable,
      data_keys = list(),
      keys = c("x", "y")
    )

    expect_equal(sort(node_list2$input_a$keys), sort(c("x", "y")))
  })

  test_that("get_node_list deals with mcdata() and mcstoc() functions", {
    test_exp1 <- quote({
      input_b <- mcdata(data = c(0.5, 1.5, 2.5))
      result <- mcstoc(rnorm, mean = input_b, sd = input_a)
    })

    test_mctable <- data.frame(
      mcnode = c("input_a"),
      mc_func = c("runif"),
      description = c("Test input A"),
      stringsAsFactors = FALSE
    )

    node_list1 <- get_node_list(
      exp = test_exp1,
      mctable = test_mctable
    )

    expect_true(node_list1$result$function_call)
    expect_true(node_list1$result$created_in_exp)
    expect_equal(node_list1$result$inputs, c("input_b", "input_a"))
    expect_equal(node_list1$input_b$type, c("out_node"))

    # Test error when nvariate is provided
    test_exp2 <- quote({
      input_b <- mcdata(data = c(0.5, 1.5, 2.5), type = "0", nvariates = 3)
      result <- mcstoc(
        rnorm,
        mean = input_b,
        sd = input_a,
        type = "V",
        nvariates = 3
      )
    })

    expect_error(
      {
        node_list2 <- get_node_list(
          exp = test_exp2,
          mctable = test_mctable
        )
      },
      "Remove 'nvariates' argument"
    )
  })

  test_that("get_node_list requires a quoted { } expression", {
    expect_error(
      get_node_list("not an expression"),
      "exp must be a quoted expression"
    )
    expect_error(
      get_node_list(123),
      "exp must be a quoted expression"
    )
  })

  test_that("get_node_list warns for unsupported mcnode types (U, VU)", {
    test_exp_u <- quote({
      input_b <- mcdata(data = c(0.5, 1.5), type = "U")
      result <- mcstoc(rnorm, mean = input_b, sd = input_a)
    })

    test_exp_vu <- quote({
      input_c <- mcdata(data = c(0.2, 0.8), type = "VU")
      final <- mcstoc(rnorm, mean = input_c, sd = input_a)
    })

    test_mctable <- data.frame(
      mcnode = c("input_a"),
      mc_func = c("runif"),
      description = c("Test input A"),
      stringsAsFactors = FALSE,
      from_variable = c(NA),
      transformation = c(NA),
      sensi_analysis = c(FALSE)
    )

    expect_warning(
      node_list_u <- get_node_list(
        exp = test_exp_u,
        mctable = test_mctable
      ),
      "not fully supported by this mcmodule"
    )
    expect_true(node_list_u$result$created_in_exp)
    expect_true("input_b" %in% names(node_list_u))

    expect_warning(
      node_list_vu <- get_node_list(
        exp = test_exp_vu,
        mctable = test_mctable
      ),
      "not fully supported by this mcmodule"
    )
    expect_true(node_list_vu$final$created_in_exp)
    expect_true("input_c" %in% names(node_list_vu))
  })
})
