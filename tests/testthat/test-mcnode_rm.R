suppressMessages({
  test_that("mcnode_na_rm replaces NA values with default 0", {
    # Create mcnode with NA values
    test_mcnode <- mcdata(c(0.1, NA, 0.3, NA, 0.5), type = "0", nvariates = 5)

    # Apply mcnode_na_rm
    result <- mcnode_na_rm(test_mcnode)

    # Check that NAs are replaced with 0
    expect_false(any(is.na(result)))
    expect_equal(result[2], 0)
    expect_equal(result[4], 0)
    expect_equal(result[1], 0.1)
    expect_equal(result[3], 0.3)
    expect_equal(result[5], 0.5)
  })

  test_that("mcnode_na_rm replaces Inf values with default 0", {
    # Create mcnode with Inf values
    test_mcnode <- mcdata(
      c(0.1, Inf, 0.3, -Inf, 0.5),
      type = "0",
      nvariates = 5
    )

    # Apply mcnode_na_rm
    result <- mcnode_na_rm(test_mcnode)

    # Check that Inf values are replaced with 0
    expect_false(any(is.infinite(result)))
    expect_equal(result[2], 0)
    expect_equal(result[4], 0)
    expect_equal(result[1], 0.1)
    expect_equal(result[3], 0.3)
    expect_equal(result[5], 0.5)
  })

  test_that("mcnode_na_rm replaces both NA and Inf values", {
    # Create mcnode with both NA and Inf values
    test_mcnode <- mcdata(
      c(0.1, NA, Inf, -Inf, 0.5, NA),
      type = "0",
      nvariates = 6
    )

    # Apply mcnode_na_rm
    result <- mcnode_na_rm(test_mcnode)

    # Check that both NA and Inf are replaced
    expect_false(any(is.na(result)))
    expect_false(any(is.infinite(result)))
    expect_equal(result[2], 0)
    expect_equal(result[3], 0)
    expect_equal(result[4], 0)
    expect_equal(result[6], 0)
    expect_equal(result[1], 0.1)
    expect_equal(result[5], 0.5)
  })

  test_that("mcnode_na_rm uses custom na_value", {
    # Create mcnode with NA and Inf values
    test_mcnode <- mcdata(c(0.1, NA, Inf, 0.4), type = "0", nvariates = 4)

    # Apply mcnode_na_rm with custom value
    result <- mcnode_na_rm(test_mcnode, na_value = -999)

    # Check that NAs and Inf are replaced with -999
    expect_equal(result[2], -999)
    expect_equal(result[3], -999)
    expect_equal(result[1], 0.1)
    expect_equal(result[4], 0.4)
  })

  test_that("mcnode_na_rm works with all NA mcnode", {
    # Create mcnode with all NAs
    test_mcnode <- mcdata(c(NA, NA, NA), type = "0", nvariates = 3)

    # Apply mcnode_na_rm
    result <- mcnode_na_rm(test_mcnode)

    # Check that all values are replaced with 0
    expect_false(any(is.na(result)))
    expect_true(all(result == 0))
    expect_equal(length(result), 3)
  })

  test_that("mcnode_na_rm works with clean mcnode (no NAs or Inf)", {
    # Create mcnode without NA or Inf
    test_mcnode <- mcdata(c(0.1, 0.2, 0.3, 0.4), type = "0", nvariates = 4)

    # Apply mcnode_na_rm
    result <- mcnode_na_rm(test_mcnode)

    # Check that values remain unchanged
    expect_equal(result, test_mcnode)
  })

  test_that("mcnode_na_rm preserves mcnode class and attributes", {
    # Create mcnode with NA
    test_mcnode <- mcdata(c(0.1, NA, 0.3), type = "0", nvariates = 3)

    # Apply mcnode_na_rm
    result <- mcnode_na_rm(test_mcnode)

    # Check that class is preserved
    expect_s3_class(result, "mcnode")
    expect_equal(class(result), class(test_mcnode))
  })

  test_that("mcnode_null_rm returns mcnode if it exists and is not NULL", {
    # Create a valid mcnode
    test_mcnode <- mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3)

    # Apply mcnode_null_rm
    result <- mcnode_null_rm(test_mcnode)

    # Check that the mcnode is returned unchanged
    expect_equal(result, test_mcnode)
  })

  test_that("mcnode_null_rm returns null_value if mcnode is NULL", {
    # Create a NULL mcnode
    test_mcnode <- NULL

    # Apply mcnode_null_rm
    result <- mcnode_null_rm(test_mcnode)

    # Check that null_value (default 0) is returned
    expect_equal(result, 0)
  })

  test_that("mcnode_null_rm uses custom null_value", {
    # Create a NULL mcnode
    test_mcnode <- NULL

    # Apply mcnode_null_rm with custom null_value
    result <- mcnode_null_rm(test_mcnode, null_value = -999)

    # Check that custom null_value is returned
    expect_equal(result, -999)
  })

  test_that("mcnode_null_rm works with numeric vectors", {
    # Test with a numeric vector (not strictly an mcnode)
    test_vector <- c(1, 2, 3)

    # Apply mcnode_null_rm
    result <- mcnode_null_rm(test_vector)

    # Should return the vector unchanged
    expect_equal(result, test_vector)
  })
})
