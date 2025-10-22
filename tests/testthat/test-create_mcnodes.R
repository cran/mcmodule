suppressMessages({
  # Unit test for Monte Carlo nodes creation
  test_that("create_mcnodes works", {
    # Test that function fails without mctable parameter
    expect_error(create_mcnodes(data = imports_data))

    # Create nodes with both required parameters
    create_mcnodes(data = imports_data, mctable = imports_mctable)

    # Verify dimensions of h_prev match expected values
    expect_equal(dim(h_prev), c(ndvar(), 1, nrow(imports_data)))

    # Compare automatic vs manual node creation
    automatic_node <- round(mean(extractvar(h_prev)), 2)
    manual_node <- round(mean(mcstoc(runif, min = imports_data$h_prev_min[1], max = imports_data$h_prev_max[1])), 2)

    # Check if both methods produce same result
    expect_equal(manual_node, automatic_node)

    # Test node creation after setting mctable
    set_mctable(imports_mctable)
    expect_no_error(create_mcnodes(data = imports_data))
    reset_mctable()
  })
})
