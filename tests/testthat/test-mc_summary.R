test_that("mc_summary works", {
  test_module <- list(
    node_list = list(
      p1 = list(
        mcnode = mcstoc(runif,
          min = mcdata(c(0.1, 0.2, 0.3), type = "0", nvariates = 3),
          max = mcdata(c(0.2, 0.3, 0.4), type = "0", nvariates = 3),
          nvariates = 3
        ),
        data_name = "test_data",
        keys = c("category")
      )
    ),
    data = list(
      test_data = data.frame(
        category = c("A", "B", "C"),
        scenario_id = c("0", "0", "0")
      )
    )
  )

  # Test basic summary from mcmodule
  result <- mc_summary(test_module, "p1")
  expect_true(all(c("category", "scenario_id") %in% names(result)))

  # Test basic summary from data
  result_data <- mc_summary(data = test_module$data$test_data, mcnode = test_module$node_list$p1$mcnode)
  expect_true("variate" %in% names(result_data))



  # Test with digits parameter
  result_rounded <- mc_summary(test_module, "p1", digits = 2)
  expect_true(all(sapply(
    result_rounded[sapply(result_rounded, is.numeric)],
    function(x) all(abs(x - round(x, 2)) < 1e-10)
  )))

  # Test keys from mcmodule
  result_keys <- mc_summary(test_module, "p1", keys_names = c("category"))
  expect_true("category" %in% names(result_keys))

  # Test keys from data
  result_data_keys <- mc_summary(
    data = test_module$data$test_data,
    mcnode = test_module$node_list$p1$mcnode,
    keys_names = c("category")
  )
  expect_true("category" %in% names(result_data_keys))

  # Test errors
  expect_error(mc_summary(test_module, "nonexistent_node"))
  expect_error(mc_summary(test_module$data, "p1"))
  expect_error(mc_summary(test_module, "p1", keys_names = c("nonexistent_key")))
})
