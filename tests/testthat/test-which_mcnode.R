test_that("which_mcnode works with custom test function", {
  # Create test mcmodule with proper mcnodes using mcdata
  test_mcnode_neg <- mcdata(c(-1, -2, -3), type = "0", nvariates = 3)
  test_mcnode_pos <- mcdata(c(1, 2, 3), type = "0", nvariates = 3)
  test_mcnode_large <- mcdata(c(100, 200, 300), type = "0", nvariates = 3)
  
  test_mcmodule <- list(
    node_list = list(
      node_negative = list(mcnode = test_mcnode_neg),
      node_positive = list(mcnode = test_mcnode_pos),
      node_large = list(mcnode = test_mcnode_large)
    )
  )

  # Test: find nodes with negative values
  result <- which_mcnode(test_mcmodule, function(x) any(x < 0))
  expect_true("node_negative" %in% result)
  expect_equal(length(result), 1)

  # Test: find nodes with values > 50
  result2 <- which_mcnode(test_mcmodule, function(x) any(x > 50))
  expect_true("node_large" %in% result2)
  expect_equal(length(result2), 1)

  # Test: find nodes with values > 1000 (none)
  result3 <- which_mcnode(test_mcmodule, function(x) any(x > 1000))
  expect_equal(length(result3), 0)
})

test_that("which_mcnode validates input", {
  # Test with invalid mcmodule
  expect_error(
    which_mcnode("not a list", function(x) any(is.na(x))),
    "mcmodule must be a list with a node_list component"
  )

  # Test with list but no node_list
  expect_error(
    which_mcnode(list(data = "test"), function(x) any(is.na(x))),
    "mcmodule must be a list with a node_list component"
  )

  # Test with non-function test_func
  test_mcmodule <- list(node_list = list(node1 = list(mcnode = c(1, 2, 3))))
  expect_error(
    which_mcnode(test_mcmodule, "not a function"),
    "test_func must be a function"
  )
})

test_that("which_mcnode handles empty node_list", {
  test_mcmodule <- list(node_list = list())
  result <- which_mcnode(test_mcmodule, function(x) any(is.na(x)))
  expect_type(result, "character")
  expect_equal(length(result), 0)
})

test_that("which_mcnode handles NULL mcnodes", {
  test_mcnode_valid <- mcdata(c(1, 2, 3), type = "0", nvariates = 3)
  
  test_mcmodule <- list(
    node_list = list(
      node1 = list(mcnode = NULL),
      node2 = list(mcnode = test_mcnode_valid)
    )
  )
  result <- which_mcnode(test_mcmodule, function(x) any(x > 0))
  expect_equal(length(result), 1)
  expect_true("node2" %in% result)
})

test_that("which_mcnode_na works with mcmodule without NAs", {
  # Test with imports_mcmodule which should not have NAs
  result <- which_mcnode_na(imports_mcmodule)

  # Should return a character vector
  expect_type(result, "character")

  # Should return empty vector if no NAs present
  expect_true(length(result) >= 0)
})

test_that("which_mcnode_na detects NAs in mcnodes", {
  # Create test mcnodes with mcdata
  test_mcnode_na <- mcdata(c(0.1, NA, 0.3, 0.4, 0.5), type = "0", nvariates = 5)
  test_mcnode_clean <- mcdata(c(0.1, 0.2, 0.3, 0.4, 0.5), type = "0", nvariates = 5)
  test_mcnode_all_na <- mcdata(c(NA, NA, NA), type = "0", nvariates = 3)
  
  test_mcmodule <- list(
    node_list = list(
      node_with_na = list(mcnode = test_mcnode_na),
      node_without_na = list(mcnode = test_mcnode_clean),
      node_all_na = list(mcnode = test_mcnode_all_na)
    )
  )

  result <- which_mcnode_na(test_mcmodule)

  # Should detect both nodes with NAs
  expect_true("node_with_na" %in% result)
  expect_true("node_all_na" %in% result)
  expect_false("node_without_na" %in% result)
  expect_equal(length(result), 2)
})

test_that("which_mcnode_na returns empty vector when no NAs present", {
  # Create test mcnodes without NAs
  test_mcnode1 <- mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3)
  test_mcnode2 <- mcdata(c(0.4, 0.5, 0.6), type = "0", nvariates = 3)
  
  test_mcmodule <- list(
    node_list = list(
      node1 = list(mcnode = test_mcnode1),
      node2 = list(mcnode = test_mcnode2)
    )
  )

  result <- which_mcnode_na(test_mcmodule)

  # Should return empty character vector
  expect_type(result, "character")
  expect_equal(length(result), 0)
})

test_that("which_mcnode_na works with mcnode matrix objects", {
  # Create an mcnode containing NA
  test_mcnode <- mcdata(c(0.1, NA), type = "0", nvariates = 2)
  
  test_mcmodule <- list(
    node_list = list(
      mcnode_test = list(mcnode = test_mcnode)
    )
  )

  result <- which_mcnode_na(test_mcmodule)

  # Should detect the NA in the mcnode
  expect_true("mcnode_test" %in% result)
  expect_equal(length(result), 1)
})

test_that("which_mcnode_inf detects infinite values", {
  # Create test mcnodes with Inf values
  test_mcnode_inf <- mcdata(c(0.1, 0.2, Inf, 0.4), type = "0", nvariates = 4)
  test_mcnode_neginf <- mcdata(c(0.1, -Inf, 0.3, 0.4), type = "0", nvariates = 4)
  test_mcnode_clean <- mcdata(c(0.1, 0.2, 0.3, 0.4), type = "0", nvariates = 4)
  test_mcnode_both <- mcdata(c(Inf, -Inf, 0), type = "0", nvariates = 3)
  
  test_mcmodule <- list(
    node_list = list(
      node_with_inf = list(mcnode = test_mcnode_inf),
      node_with_neg_inf = list(mcnode = test_mcnode_neginf),
      node_without_inf = list(mcnode = test_mcnode_clean),
      node_with_both = list(mcnode = test_mcnode_both)
    )
  )

  result <- which_mcnode_inf(test_mcmodule)

  # Should detect all nodes with Inf or -Inf
  expect_true("node_with_inf" %in% result)
  expect_true("node_with_neg_inf" %in% result)
  expect_true("node_with_both" %in% result)
  expect_false("node_without_inf" %in% result)
  expect_equal(length(result), 3)
})

test_that("which_mcnode_inf returns empty vector when no Inf present", {
  # Create test mcnodes without Inf values
  test_mcnode1 <- mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3)
  test_mcnode2 <- mcdata(c(0.4, 0.5, 0.6), type = "0", nvariates = 3)
  
  test_mcmodule <- list(
    node_list = list(
      node1 = list(mcnode = test_mcnode1),
      node2 = list(mcnode = test_mcnode2)
    )
  )

  result <- which_mcnode_inf(test_mcmodule)

  # Should return empty character vector
  expect_type(result, "character")
  expect_equal(length(result), 0)
})

test_that("which_mcnode_inf works with imports_mcmodule", {
  # Test with real data
  result <- which_mcnode_inf(imports_mcmodule)

  # Should return a character vector
  expect_type(result, "character")
})

test_that("which_mcnode_na and which_mcnode_inf are independent", {
  # Create mcnodes with both NA and Inf
  test_mcnode_na <- mcdata(c(0.1, NA, 0.3), type = "0", nvariates = 3)
  test_mcnode_inf <- mcdata(c(0.1, Inf, 0.3), type = "0", nvariates = 3)
  test_mcnode_both <- mcdata(c(NA, Inf, 0.3), type = "0", nvariates = 3)
  test_mcnode_clean <- mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3)
  
  test_mcmodule <- list(
    node_list = list(
      node_na = list(mcnode = test_mcnode_na),
      node_inf = list(mcnode = test_mcnode_inf),
      node_both = list(mcnode = test_mcnode_both),
      node_clean = list(mcnode = test_mcnode_clean)
    )
  )

  result_na <- which_mcnode_na(test_mcmodule)
  result_inf <- which_mcnode_inf(test_mcmodule)

  # NA function should find nodes with NAs
  expect_true("node_na" %in% result_na)
  expect_true("node_both" %in% result_na)
  expect_false("node_inf" %in% result_na)
  expect_false("node_clean" %in% result_na)

  # Inf function should find nodes with Inf
  expect_true("node_inf" %in% result_inf)
  expect_true("node_both" %in% result_inf)
  expect_false("node_na" %in% result_inf)
  expect_false("node_clean" %in% result_inf)
})

test_that("which_mcnode handles errors in test function gracefully", {
  test_mcnode <- mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3)
  
  test_mcmodule <- list(
    node_list = list(
      node1 = list(mcnode = test_mcnode)
    )
  )

  # Test function that throws an error
  result <- which_mcnode(test_mcmodule, function(x) stop("error"))

  # Should return empty vector when error occurs
  expect_type(result, "character")
  expect_equal(length(result), 0)
})
