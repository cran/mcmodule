suppressMessages({
  test_that("set_data_keys works with new format", {
    # Verify data model starts empty
    expect_equal(length(set_data_keys()), 0)

    # Test that function fails with wrong input
    expect_error(set_data_keys("not a list"))
    expect_error(set_data_keys(list(bad_element = "not proper structure")))

    # Test validation that keys must be in cols
    invalid_data_keys <- list(
      test_data = list(
        cols = c("x", "y"),
        keys = c("x", "z")  # z is not in cols
      )
    )
    expect_error(set_data_keys(invalid_data_keys), "Keys must be a subset of column names")

    # Set data model and verify it matches expected value
    test_data_keys <- list(
      test_data = list(
        cols = c("x", "y", "z"),
        keys = c("x")
      ),
      another_data = list(
        cols = c("id", "name", "value"),
        keys = c("id", "name")
      )
    )
    expect_message(set_data_keys(test_data_keys), "data_keys set to test_data_keys")
    expect_equal(set_data_keys(), test_data_keys)

    # Test with NULL elements (conditional datasets)
    test_data_keys_with_null <- list(
      test_data = list(
        cols = c("x", "y", "z"),
        keys = c("x")
      ),
      conditional_data = NULL
    )
    expect_message(set_data_keys(test_data_keys_with_null))
    expect_equal(length(set_data_keys()), 2)

    # Reset and verify it's empty
    reset_data_keys()
    expect_equal(length(set_data_keys()), 0)
    expect_type(set_data_keys(), "list")
  })
})
