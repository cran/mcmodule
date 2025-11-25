suppressMessages({
  test_that("add_group_id single dataframe works", {
    # Create test data
    test_df <- data.frame(
      category = rep(c("A", "B"), 5),
      value = rnorm(10),
      type = rep(c("X", "Y"), each = 5)
    )

    # Test with one key
    result <- add_group_id(test_df, by = "category")
    expect_equal(max(result$g_id), 2) # Should have 2 groups

    # Test with two keys
    result <- add_group_id(test_df, by = c("category", "type"))
    expect_equal(max(result$g_id), 4) # Should have 4 groups
  })

  test_that("add_group_id two dataframes works", {
    # Create test data
    df1 <- data.frame(
      category = rep(c("A", "B"), 5),
      value = rnorm(10),
      type = rep(c("X", "Y"), each = 5)
    )

    df2 <- data.frame(
      category = rep(c("A", "B"), 5),
      value = rnorm(10),
      type = rep(c("Y", "X"), each = 5)
    )

    # Test with two dataframes
    result <- add_group_id(df1, df2, by = c("category", "type"))
    expect_equal(names(result), c("x", "y"))
    expect_equal(
      unique(result$x[order(result$x$g_id), c("g_id", "category", "type")]),
      unique(result$y[order(result$y$g_id), c("g_id", "category", "type")])
    )

    # Test automatic categorical variable detection
    result_auto <- add_group_id(df1, df2)
    expect_equal(
      unique(result_auto$x[
        order(result_auto$x$g_id),
        c("g_id", "category", "type")
      ]),
      unique(result_auto$y[
        order(result_auto$y$g_id),
        c("g_id", "category", "type")
      ])
    )

    # Test error handling
    expect_error(add_group_id(df1, df2, by = "nonexistent"))
  })

  test_that("keys_match works", {
    # Test data
    x <- data.frame(
      type = c("1", "2"),
      category = c("a", "b"),
      scenario_id = c(0, 1)
    )

    y <- data.frame(
      type = c("1", "2"),
      category = c("c", "d"),
      scenario_id = c(0, 2)
    )

    # Automatic matching
    expect_message(
      result <- keys_match(x, y),
      "Group by: type, category"
    )

    expect_equal(result$xy$scenario_id, c(0, 1, 0, 2))

    # Match by type
    result_type <- keys_match(x, y, "type")
    expect_equal(result_type$xy$g_id, c(1, 2, 2))
  })

  test_that("keys_match returns keys_x when keys_x and keys_y identical", {
    x <- data.frame(category = c("a", "b"), val_x = 1:2)
    y <- data.frame(category = c("a", "b"), val_y = 3:4)
    res <- keys_match(x, y, keys_names = "category")
    expect_equal(res$xy$g_row.x, c(1, 2))
    expect_equal(res$xy$g_row.y, c(1, 2))
  })

  test_that("keys_match handles non-matching nodes (extra group in y)", {
    x <- data.frame(category = c("a", "b"), val_x = 1:2)
    y <- data.frame(category = c("a", "b", "c"), val_y = 3:5)
    res <- keys_match(x, y, keys_names = "category")
    expect_equal(res$xy$g_row.x, c(1, 2, NA))
    expect_equal(res$xy$g_row.y, c(1, 2, 3))
  })
})
