suppressMessages({
  # Unit test for Monte Carlo table setting and resetting
  test_that("set_mctable and reset_mctable work", {
    # Verify mctable starts empty
    expect_equal(nrow(set_mctable()), 0)

    # Test that function fails with wrong input
    expect_error(set_mctable(imports_data))

    # Set mctable and verify it matches expected value
    set_mctable(imports_mctable)
    expect_equal(set_mctable(), imports_mctable)

    # Verify mctable reset
    reset_mctable()
    expect_equal(nrow(set_mctable()), 0)
  })

  test_that("check_mctable works", {
    example_mctable <- data.frame(
      mcnode = c("x", "y"),
      description = c("Probability x", "Probability y"),
      mc_func = c("runif", NA)
    )

    expect_no_warning(
      checked_example_mctable <- check_mctable(example_mctable)
    )

    expect_equal(names(checked_example_mctable), names(set_mctable()))
  })

  test_that("check_mctable works for incomplete mctable within functions", {
    example_data <- data.frame(
      category_1 = c("a", "b", "a", "b"),
      category_2 = c("blue", "blue", "red", "red"),
      x_min = c(0.07, 0.3, 0.2, 0.5),
      x_max = c(0.08, 0.4, 0.3, 0.6),
      y = c(0.01, 0.02, 0.03, 0.04)
    )

    example_data_keys <- list(
      example_data = list(
        cols = names(example_data),
        keys = c("category_1", "category_2")
      )
    )

    example_mctable <- data.frame(
      mcnode = c("x", "y"),
      description = c("Probability x", "Probability y"),
      mc_func = c("runif", NA)
    )

    example_exp <- quote({
      result <- x * y
    })

    expect_no_warning(
      example_mcmodule <- eval_module(
        exp = c(example = example_exp),
        data = example_data,
        mctable = example_mctable,
        data_keys = example_data_keys
      )
    )
  })
})
