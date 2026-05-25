suppressMessages({
  test_that("set_sample_design and reset_sample_design manage the global design", {
    # Start clean
    reset_sample_design()
    expect_null(set_sample_design())

    X <- data.frame(a = c(0.1, 0.2), b = c(1, 2), stringsAsFactors = FALSE)
    expect_message(set_sample_design(X), "sample_design set")

    current <- set_sample_design()
    expect_type(current, "list")
    expect_true(all(c("sa", "X") %in% names(current)))
    expect_null(current$sa)
    expect_true(is.data.frame(current$X))
    expect_equal(current$X, X)

    expect_message(reset_sample_design(), "sample_design reset")
    expect_null(set_sample_design())
    reset_sample_design()
  })

  test_that("set_sample_design accepts a list input with X", {
    X <- data.frame(a = c(1, 2), stringsAsFactors = FALSE)
    obj <- list(sa = "dummy", X = X)

    set_sample_design(obj)
    current <- set_sample_design()
    expect_equal(current$sa, "dummy")
    expect_equal(current$X, X)
    reset_sample_design()
  })

  test_that("mctable_bounds errors when required columns are missing", {
    mctable <- data.frame(
      mcnode = "x",
      stringsAsFactors = FALSE
    )
    expect_error(
      mctable_bounds(mctable),
      "mctable must contain columns 'mcnode' and 'sample_space'"
    )
  })

  test_that("mctable_bounds supports categorical sample_space via numeric transformation", {
    set.seed(456)
    mctable <- data.frame(
      mcnode = "x",
      sample_space = "c('always','sometimes','never')",
      transformation = "ifelse(value == 'always', 1, ifelse(value == 'sometimes', 0.5, 0))",
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable, transformation = TRUE, n_probe = 2000)

    expect_equal(b$factors, "x")
    # Expected bounds after mapping {never, sometimes, always} -> {0, 0.5, 1}.
    expect_equal(b$binf[[1]], 0)
    expect_equal(b$bsup[[1]], 1)
  })

  test_that("mctable_bounds uses n_probe to approximate bounds for non-monotone transformations", {
    set.seed(123)
    mctable <- data.frame(
      mcnode = c("x"),
      sample_space = c("min = 0, max = 1"),
      # Non-monotone on [0, 1], maximum at value = 0.3
      transformation = c("-(value - 0.3)^2"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable, transformation = TRUE, n_probe = 5000)
    expect_equal(b$factors, "x")
    # True bounds are [-0.49, 0]. Probing should get close to 0 for bsup.
    expect_true(b$binf[[1]] <= -0.45)
    expect_true(b$bsup[[1]] > -0.01)
    expect_true(b$bsup[[1]] <= 0.001)
  })

  test_that("mctable_bounds returns numeric bounds and factor names", {
    mctable <- data.frame(
      mcnode = c("x", "y"),
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable)
    expect_type(b, "list")
    expect_true(all(c("binf", "bsup", "factors", "fixed") %in% names(b)))
    expect_equal(b$factors, c("x", "y"))
    expect_type(b$binf, "double")
    expect_type(b$bsup, "double")
    expect_equal(b$binf, c(0, 10))
    expect_equal(b$bsup, c(1, 20))
    expect_equal(length(b$fixed), 0)
  })

  test_that("mctable_bounds supports c(min, max) bounds", {
    mctable <- data.frame(
      mcnode = c("x", "y"),
      sample_space = c("c(0, 1)", "c(10, 20)"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable)
    expect_equal(b$binf, c(0, 10))
    expect_equal(b$bsup, c(1, 20))
  })

  test_that("mctable_bounds filters mc_names and errors on invalid names", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        "min = -5, max = 5"
      ),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable, mc_names = c("a", "c"))
    expect_equal(b$factors, c("a", "c"))
    expect_equal(b$binf, c(0, -5))
    expect_equal(b$bsup, c(1, 5))

    expect_error(
      mctable_bounds(mctable, mc_names = c("a", "nope")),
      "Invalid mc_names"
    )
  })

  test_that("mctable_bounds drops NA/empty sample_space from factors by default", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c", "d"),
      sample_space = c("min = 0, max = 1", NA, "", "NA"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable)
    expect_equal(b$factors, "a")
    expect_equal(b$binf, 0)
    expect_equal(b$bsup, 1)
    expect_equal(length(b$fixed), 0)
  })

  test_that("mctable_bounds sets fixed values for non-sampled nodes", {
    mctable <- data.frame(
      mcnode = c("a", "b", "c", "d"),
      sample_space = c(
        "min = 0, max = 1",
        "min = 10, max = 20",
        NA,
        "NA"
      ),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(
      mctable,
      mc_names = "a",
      if_not_sampled = "median",
      transformation = FALSE
    )

    expect_equal(b$factors, "a")
    expect_equal(b$binf, 0)
    expect_equal(b$bsup, 1)
    expect_true(all(c("b", "c", "d") %in% names(b$fixed)))
    expect_equal(unname(b$fixed[["b"]]), 15)
    expect_equal(unname(b$fixed[["c"]]), 0)
    expect_equal(unname(b$fixed[["d"]]), 0)
  })

  test_that("mctable_bounds applies transformation to bounds when requested", {
    set.seed(1)
    mctable <- data.frame(
      mcnode = c("x"),
      sample_space = c("min = 0, max = 1"),
      transformation = c("value^2"),
      stringsAsFactors = FALSE
    )

    b <- mctable_bounds(mctable, transformation = TRUE, n_probe = 2000)
    expect_equal(b$factors, "x")
    expect_true(b$binf[[1]] >= 0)
    expect_true(b$bsup[[1]] <= 1)
  })

  test_that("mctable_bounds errors for unsupported bounds formats", {
    mctable <- data.frame(
      mcnode = c("cat"),
      sample_space = c("c('a','b')"),
      stringsAsFactors = FALSE
    )

    expect_error(
      mctable_bounds(mctable),
      "Cannot extract numeric bounds"
    )
  })

  test_that("mctable_sobol_matrices returns mapped draws for runif bounds", {
    skip_if_not_installed("sensobol")

    mctable <- data.frame(
      mcnode = c("a", "b"),
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      stringsAsFactors = FALSE
    )

    X <- mctable_sobol_matrices(
      mctable = mctable,
      N = 32,
      order = "first"
    )

    expect_true(is.matrix(X))
    expect_equal(ncol(X), 2)
    expect_true(all(X[, 1] >= 0 & X[, 1] <= 1))
    expect_true(all(X[, 2] >= 10 & X[, 2] <= 20))
  })

  test_that("mctable_sobol_matrices maps rnorm using qnorm", {
    skip_if_not_installed("sensobol")

    mctable <- data.frame(
      mcnode = "x",
      mc_func = "rnorm",
      sample_space = "mean = 0, sd = 1",
      stringsAsFactors = FALSE
    )

    X <- mctable_sobol_matrices(
      mctable = mctable,
      N = 64,
      order = "first"
    )

    expect_true(is.matrix(X))
    expect_equal(ncol(X), 1)
    expect_true(all(is.finite(X[, 1])))
    expect_true(abs(mean(X[, 1])) < 0.25)
    expect_true(sd(X[, 1]) > 0.5)
  })

  test_that("mctable_sobol_matrices supports mc_names", {
    skip_if_not_installed("sensobol")

    mctable <- data.frame(
      mcnode = c("a", "b", "c"),
      sample_space = c("min = 0, max = 1", "min = 10, max = 20", NA),
      stringsAsFactors = FALSE
    )

    X <- mctable_sobol_matrices(
      mctable = mctable,
      N = 16,
      order = "first",
      mc_names = "a"
    )

    expect_true(is.matrix(X))
    expect_equal(ncol(X), 1)
  })

  test_that("eval_module fills missing sample_design inputs from mctable sample_space", {
    # Only 'a' is provided in sample_design; 'b' is required by expression.
    sample_design <- data.frame(a = c(0, 1), stringsAsFactors = FALSE)

    mctable <- data.frame(
      mcnode = c("a", "b"),
      mc_func = NA,
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      stringsAsFactors = FALSE
    )

    expr <- quote({
      out <- a + b
    })

    m <- eval_module(
      exp = expr,
      data = NULL,
      mctable = mctable,
      sample_design = sample_design,
      if_not_sampled = "median"
    )

    expect_true(inherits(m, "mcmodule"))
    expect_true(isTRUE(m$node_list$b$from_sample_design))
    expect_true(isTRUE(m$node_list$b$from_sample_design_fixed))
    # b fixed at mean(10, 20) = 15 for both samples
    expect_equal(as.numeric(m$node_list$b$mcnode[, 1, 1]), c(15, 15))
    # out = a + b
    expect_equal(as.numeric(m$node_list$out$mcnode[, 1, 1]), c(15, 16))
  })

  test_that("eval_module if_not_sampled supports min/max", {
    sample_design <- data.frame(a = c(0, 1), stringsAsFactors = FALSE)
    mctable <- data.frame(
      mcnode = c("a", "b"),
      mc_func = NA,
      sample_space = c("min = 0, max = 1", "min = 10, max = 20"),
      stringsAsFactors = FALSE
    )
    expr <- quote({
      out <- a + b
    })

    m_min <- eval_module(
      exp = expr,
      data = NULL,
      mctable = mctable,
      sample_design = sample_design,
      if_not_sampled = "min"
    )
    expect_equal(as.numeric(m_min$node_list$b$mcnode[, 1, 1]), c(10, 10))

    m_max <- eval_module(
      exp = expr,
      data = NULL,
      mctable = mctable,
      sample_design = sample_design,
      if_not_sampled = "max"
    )
    expect_equal(as.numeric(m_max$node_list$b$mcnode[, 1, 1]), c(20, 20))
  })

  test_that("eval_module errors only when missing from sample_design and cannot be created from mctable", {
    sample_design <- data.frame(a = c(0, 1), stringsAsFactors = FALSE)
    mctable <- data.frame(
      mcnode = c("a"),
      mc_func = NA,
      sample_space = c("min = 0, max = 1"),
      stringsAsFactors = FALSE
    )

    expr <- quote({
      out <- a + b
    })

    expect_error(
      eval_module(
        exp = expr,
        data = NULL,
        mctable = mctable,
        sample_design = sample_design
      ),
      "Input 'b' is missing from sample_design and not found in mctable"
    )
  })
})
