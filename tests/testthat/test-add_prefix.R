suppressMessages({
  test_that("add_prefix works with basic mcmodule", {
    old_names <- names(imports_mcmodule$node_list)
    imports_mcmodule_new <- add_prefix(imports_mcmodule)
    new_names <- names(imports_mcmodule_new$node_list)
    expect_equal(paste0("imports_mcmodule_", old_names), new_names)
  })

  test_that("add_prefix works with custom prefix", {
    old_names <- names(imports_mcmodule$node_list)
    imports_mcmodule_new <- add_prefix(imports_mcmodule, prefix = "custom")
    new_names <- names(imports_mcmodule_new$node_list)
    expect_equal(paste0("custom_", old_names), new_names)
  })

  test_that("add_prefix does not duplicate prefix when already prefixed", {
    # Apply prefix twice
    imports_mcmodule_once <- add_prefix(imports_mcmodule, prefix = "test")
    imports_mcmodule_twice <- add_prefix(imports_mcmodule_once, prefix = "test")

    # Names should be the same
    expect_equal(
      names(imports_mcmodule_once$node_list),
      names(imports_mcmodule_twice$node_list)
    )

    # Verify no double prefix exists
    expect_false(any(grepl(
      "test_test_",
      names(imports_mcmodule_twice$node_list)
    )))
  })

  test_that("add_prefix works with total nodes", {
    test_mcmodule <- imports_mcmodule
    # Create mcmodule with total nodes
    test_mcmodule <- trial_totals(
      mcmodule = test_mcmodule,
      mc_names = "no_detect_a",
      trials_n = "animals_n",
      mctable = imports_mctable
    )

    old_names <- names(test_mcmodule$node_list)

    # Apply prefix
    test_mcmodule <- add_prefix(test_mcmodule, prefix = "prefixed")
    new_names <- names(test_mcmodule$node_list)

    # Check that total nodes are prefixed (note: trial_totals creates nodes with "_a_set" suffix)
    expect_true(any(grepl("prefixed_no_detect_a_set", new_names)))
    expect_true(any(grepl("prefixed_no_detect_a_set_n", new_names)))

    # Verify all nodes from module and its inputs are prefixed (animals_n is input to totals)
    expect_true(all(startsWith(new_names, "prefixed_")))
  })

  test_that("add_prefix preserves node inputs correctly", {
    mcmodule_prefixed <- add_prefix(imports_mcmodule, prefix = "test")

    # Check that inputs are also prefixed
    for (node_name in names(mcmodule_prefixed$node_list)) {
      inputs <- mcmodule_prefixed$node_list[[node_name]][["inputs"]]
      if (!is.null(inputs) && length(inputs) > 0) {
        # All inputs should either be prefixed or be from previous modules
        expect_true(all(
          startsWith(inputs, "test_") |
            !inputs %in% names(imports_mcmodule$node_list)
        ))
      }
    }
  })

  test_that("add_prefix works with combined modules", {
    # Create a second simple module
    transmission_data <- data.frame(
      pathogen = c("a", "b"),
      inf_dc_min = c(0.05, 0.3),
      inf_dc_max = c(0.08, 0.4)
    )

    transmission_mctable <- data.frame(
      mcnode = "inf_dc",
      mc_func = "runif"
    )

    transmission <- eval_module(
      exp = list(
        transmission = quote({
          result <- inf_dc
        })
      ),
      data = transmission_data,
      mctable = transmission_mctable,
      prev_mcmodule = imports_mcmodule
    )
    # Apply prefix
    prefixed_a <- add_prefix(imports_mcmodule)
    # Apply prefix
    prefixed_b <- add_prefix(transmission)

    # Combine modules
    combined <- combine_modules(prefixed_a, prefixed_b)
    combined <- at_least_one(
      combined,
      mc_names = c("imports_mcmodule_no_detect_a", "transmission_result"),
      name = "combined_result"
    )
    # Check that combined module has prefixed nodes from both modules
    expect_true(any(grepl(
      "imports_mcmodule_no_detect_a",
      names(combined$node_list)
    )))
    expect_true(any(grepl("transmission_result", names(combined$node_list))))
  })

  test_that("add_prefix handles nodes without module assignment", {
    # Create a copy and remove module info from some nodes
    test_mcmodule <- imports_mcmodule
    test_mcmodule$node_list[[1]][["module"]] <- NULL

    # Should not error and should assign module name
    prefixed <- add_prefix(test_mcmodule, prefix = "test")

    expect_true(all(startsWith(names(prefixed$node_list), "test_")))
  })

  test_that("add_prefix with rewrite_module parameter", {
    # First apply a prefix
    first_prefix <- add_prefix(imports_mcmodule, prefix = "first")

    # Then rewrite with a new prefix
    rewritten <- add_prefix(
      first_prefix,
      prefix = "second",
      rewrite_module = "first"
    )

    # All nodes should now have "second_" prefix, not "first_"
    expect_true(all(startsWith(names(rewritten$node_list), "second_")))
    expect_false(any(grepl("first_", names(rewritten$node_list))))
  })

  test_that("add_prefix does not add redundant module-level prefix", {
    prefixed <- add_prefix(imports_mcmodule, prefix = "test")

    # Check that mcmodule$prefix is not set (redundant with node-level)
    expect_null(prefixed$prefix)

    # But node-level prefixes should exist
    expect_true(all(
      sapply(prefixed$node_list, function(x) !is.null(x[["prefix"]]))
    ))
  })

  test_that("add_prefix works with aggregated totals", {
    # Create mcmodule with aggregated totals
    mcmodule_agg <- imports_mcmodule %>%
      trial_totals(
        mc_names = "no_detect_a",
        trials_n = "animals_n",
        mctable = imports_mctable
      ) %>%
      agg_totals(mc_name = "no_detect_a_set", agg_keys = "pathogen")

    # Apply prefix
    prefixed <- add_prefix(mcmodule_agg, prefix = "prefixed")

    # Check that aggregated nodes are also prefixed
    expect_true(any(grepl(
      "prefixed_no_detect_a_set_agg",
      names(prefixed$node_list)
    )))
  })
})
