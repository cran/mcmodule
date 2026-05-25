## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----include = FALSE----------------------------------------------------------
library(mcmodule)
set.seed(123)

## -----------------------------------------------------------------------------
# Calculate output (expected number of animals not detected)
imports_mcmodule_corr <- trial_totals(
  imports_mcmodule,
  mc_names = c("no_detect"),
  trials_n = "animals_n",
  subsets_n = "farms_n",
  subsets_p = "h_prev",
  mctable = imports_mctable
)

# Calculate correlations using Spearman's method (default)
corr_results <- mcmodule_corr(imports_mcmodule_corr, output = "no_detect_set_n", method = "spearman")

## -----------------------------------------------------------------------------
# View correlation results
head(corr_results)

## -----------------------------------------------------------------------------
library(ggplot2)

# Plot tornado plot of correlations
mcmodule_tornado(imports_mcmodule_corr, output = "no_detect_set_n", method = "spearman", print_summary = FALSE)

## -----------------------------------------------------------------------------
# Create a sample design as a data frame
X <- data.frame(
  w_prev = runif(10000, 0.15, 0.6),
  h_prev = runif(10000, 0.02, 0.7),
  test_sensi = runif(10000, 0.8, 0.91),
  animals_n = sample(5:10, 10000, replace = TRUE),
  farms_n = sample(82:176, 10000, replace = TRUE),
  test_origin = runif(10000, 0, 1)
)

dim(X)
head(X)

# Set global sample design
set_sample_design(X)

# Retrieve current global sample design
current_design <- set_sample_design()

# Evaluate the module with the global sample design
imports_mcmodule_X <- eval_module(
  exp = imports_exp
)

# Calculate output (expected number of animals not detected)
imports_mcmodule_X <- trial_totals(
  imports_mcmodule_X,
  mc_names = c("no_detect"),
  trials_n = "animals_n",
  subsets_n = "farms_n",
  subsets_p = "h_prev"
)

mcmodule_tornado(imports_mcmodule_X, output = "no_detect_set_n", method = "spearman")

# Reset global sample design
reset_sample_design()

## -----------------------------------------------------------------------------
imports_mctable[,c("mcnode","mc_func", "sample_space")]

## -----------------------------------------------------------------------------
# Get bounds for Morris sampling design
b <- mctable_bounds(imports_mctable, transformation = FALSE)

## -----------------------------------------------------------------------------
library(sensitivity)

# Create Morris design
morris_sa <- sensitivity::morris(
  model = NULL,
  factors = b$factors,
  r = 2000,
  design = list(type = "oat", levels = 4, grid.jump = 2),
  binf = b$binf,
  bsup = b$bsup,
  scale = TRUE
)

# Evaluate the module with that design.
imports_mcmodule_morris <- eval_module(
  exp = imports_exp,
  sample_design = morris_sa,
  mctable = imports_mctable
)

# Calculate output (expected number of animals not detected)
imports_mcmodule_morris <- trial_totals(
  imports_mcmodule_morris,
  mc_names = c("no_detect"),
  trials_n = "animals_n",
  subsets_n = "farms_n",
  subsets_p = "h_prev",
  sample_design = morris_sa,
  mctable = imports_mctable
)

## -----------------------------------------------------------------------------
# Extract the output vector and estimate Morris indices.
y <- unmc(imports_mcmodule_morris$node_list$no_detect_set_n$mcnode)

# Aggregated output used for sensitivity analysis.
sensitivity::tell(morris_sa, y)

# Print Morris indices
morris_sa

# Plot Morris indices (mu.star and sigma)
plot(morris_sa)

## -----------------------------------------------------------------------------
library(sensobol)

N <- 10000
X <- mctable_sobol_matrices(imports_mctable, N = N, order = "second")

## -----------------------------------------------------------------------------
imports_mcmodule_sobol <- eval_module(
  exp = imports_exp,
  data = NULL,
  sample_design = X,
  mctable = imports_mctable
)

imports_mcmodule_sobol <- trial_totals(
  mcmodule = imports_mcmodule_sobol,
  mc_names = c("no_detect"),
  trials_n = "animals_n",
  subsets_n = "farms_n",
  subsets_p = "h_prev",
  sample_design = X,
  mctable = imports_mctable
)

## -----------------------------------------------------------------------------
y <- unmc(imports_mcmodule_sobol$node_list$no_detect_set_n$mcnode)

# Compute Sobol indices
sobol_sa <- sensobol::sobol_indices(Y = y, N = N, params = colnames(X), order = "second", boot = TRUE, R = 1000)

sobol_sa

plot(sobol_sa)
plot(sobol_sa, order = "second")

