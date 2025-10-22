## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(mcmodule)

## ----warning=FALSE, message=FALSE---------------------------------------------
library(mc2d)
set.seed(123)
n_iterations <- 10000

# Within-herd prevalence
w_prev <- mcstoc(runif,
  min = 0.15, max = 0.2,
  nsu = n_iterations, type = "U"
)
# Test sensitivity
test_sensi <- mcstoc(rpert,
  min = 0.89, mode = 0.9, max = 0.91,
  nsu = n_iterations, type = "U"
)
# Probability an animal is tested in origin
test_origin <- mcdata(1, type = "0") # Yes


# EXPRESSIONS
# Probability that an animal in an infected herd is infected (a = animal)
infected <- w_prev
# Probability an animal is tested and is a false negative
# (test specificity assumed to be 100%)
false_neg <- infected * test_origin * (1 - test_sensi)
# Probability an animal is not tested
no_test <- infected * (1 - test_origin)
# Probability an animal is not detected
no_detect <- false_neg + no_test

mc_model <- mc(
  w_prev, infected, test_origin, test_sensi,
  false_neg, no_test, no_detect
)

# RESULT
hist(mc_model)
no_detect

## -----------------------------------------------------------------------------
set.seed(123)
n_iterations <- 10000

# Within-herd prevalence
w_prev_min <- mcdata(c(a = 0.15, b = 0.45), nvariates = 2, type = "0")
w_prev_max <- mcdata(c(a = 0.2, b = 0.6), nvariates = 2, type = "0")

w_prev <- mcstoc(runif,
  min = w_prev_min, max = w_prev_max,
  nsu = n_iterations, nvariates = 2, type = "U"
)

# Test sensitivity
test_sensi_min <- mcdata(c(a = 0.89, b = 0.80), nvariates = 2, type = "0")
test_sensi_mode <- mcdata(c(a = 0.9, b = 0.85), nvariates = 2, type = "0")
test_sensi_max <- mcdata(c(a = 0.91, b = 0.90), nvariates = 2, type = "0")

test_sensi <- mcstoc(rpert,
  min = test_sensi_min,
  mode = test_sensi_mode, max = test_sensi_max,
  nsu = n_iterations, nvariates = 2, type = "U"
)

# Probability an animal is tested in origin
test_origin <- mcdata(c(a = 1, b = 1), nvariates = 2, type = "0")


# EXPRESSIONS
# Probability that an animal in an infected herd is infected (a = animal)
infected <- w_prev
# Probability an animal is tested and is a false negative
# (test specificity assumed to be 100%)
false_neg <- infected * test_origin * (1 - test_sensi)
# Probability an animal is not tested
no_test <- infected * (1 - test_origin)
# Probability an animal is not detected
no_detect <- false_neg + no_test

mc_model <- mc(
  w_prev, infected, test_origin, test_sensi,
  false_neg, no_test, no_detect
)

# RESULT
no_detect

## ----eval=FALSE---------------------------------------------------------------
# install.packages("mcmodule")
# library("mcmodule")

## ----eval=FALSE---------------------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("NataliaCiria/mcmodule")
# library("mcmodule")

## ----warning=FALSE, message=FALSE---------------------------------------------
# install.packages(c("dplyr","ggplot2","igraph","visNetwork"))
library(dplyr) # Data manipulation
library(igraph) # Network analysis
library(visNetwork) # Interactive network visualization

## -----------------------------------------------------------------------------
animal_imports

## -----------------------------------------------------------------------------
prevalence_region

## -----------------------------------------------------------------------------
test_sensitivity

## -----------------------------------------------------------------------------
imports_data <- prevalence_region %>%
  left_join(animal_imports) %>%
  left_join(test_sensitivity) %>%
  relocate(pathogen, origin, test_origin)

## -----------------------------------------------------------------------------
imports_data_keys <- list(
  animal_imports = list(
    cols = names(animal_imports),
    keys = "origin"
  ),
  prevalence_region = list(
    cols = names(prevalence_region),
    keys = c("pathogen", "origin")
  ),
  test_sensitivity = list(
    cols = names(test_sensitivity),
    keys = "pathogen"
  )
)

## ----echo = FALSE-------------------------------------------------------------
# Display the table with horizontal scrolling
imports_mctable %>%
  mutate(transformation = ifelse(mcnode == "test_origin", 'ifelse(value == "always", 1, ifelse(value == "sometimes", 0.5, ifelse(value == "never", 0, NA)))', transformation)) %>%
  knitr::kable()

## -----------------------------------------------------------------------------
imports_exp <- quote({
  # Probability that an animal in an infected herd is infected (a = animal)
  infected <- w_prev
  # Probability an animal is tested and is a false negative
  # (test specificity assumed to be 100%)
  false_neg <- infected * test_origin * (1 - test_sensi)
  # Probability an animal is not tested
  no_test <- infected * (1 - test_origin)
  # Probability an animal is not detected
  no_detect <- false_neg + no_test
})

## -----------------------------------------------------------------------------
imports <- eval_module(
  exp = c(imports = imports_exp),
  data = imports_data,
  mctable = imports_mctable,
  data_keys = imports_data_keys
)

## -----------------------------------------------------------------------------
class(imports)

## -----------------------------------------------------------------------------
names(imports)

## -----------------------------------------------------------------------------
imports$node_list$w_prev
imports$node_list$no_detect

## -----------------------------------------------------------------------------
mc_network(imports, legend = TRUE)

## -----------------------------------------------------------------------------
mc_summary(mcmodule = imports, mc_name = "no_detect")

## -----------------------------------------------------------------------------
# Probability of at least one imported animal from an infected herd is not detected
imports <- trial_totals(
  mcmodule = imports,
  mc_names = "no_detect",
  trials_n = "animals_n",
  mctable = imports_mctable
)

## -----------------------------------------------------------------------------
# Probability of at least one
imports$node_list$no_detect_set$summary

# Expected number of animals
imports$node_list$no_detect_set_n$summary

## -----------------------------------------------------------------------------
# Probability of at least one animal from at least one herd being is not detected (probability of a herd being infected: h_prev)
imports <- trial_totals(
  mcmodule = imports,
  mc_names = "no_detect",
  trials_n = "animals_n",
  subsets_n = "farms_n",
  subsets_p = "h_prev",
  mctable = imports_mctable,
)

# Result
imports$node_list$no_detect_set$summary

## -----------------------------------------------------------------------------
# Probability of at least one in a farm
imports$node_list$no_detect_subset$summary

## -----------------------------------------------------------------------------
imports <- agg_totals(
  mcmodule = imports,
  mc_name = "no_detect_set",
  agg_keys = "pathogen"
)

# Result
imports$node_list$no_detect_set_agg$summary

## -----------------------------------------------------------------------------
mc_network(imports, legend = TRUE)

## -----------------------------------------------------------------------------
imports_data <- imports_data %>%
  mutate(scenario_id = "0")

imports_data_wif <- imports_data %>%
  mutate(
    scenario_id = "always_test",
    test_origin = "always"
  ) %>%
  bind_rows(imports_data) %>%
  relocate(scenario_id)

imports_data_wif[, 1:6]

## -----------------------------------------------------------------------------
imports_wif <- eval_module(
  exp = c(imports = imports_exp),
  data = imports_data_wif,
  mctable = imports_mctable,
  data_keys = imports_data_keys
)

imports_wif <- imports_wif %>%
  trial_totals(
    mc_names = "no_detect",
    trials_n = "animals_n",
    subsets_n = "farms_n",
    subsets_p = "h_prev",
    mctable = imports_mctable,
  ) %>%
  agg_totals(
    mc_name = "no_detect_set",
    agg_keys = c("pathogen", "scenario_id")
  )

# Result
imports_wif$node_list$no_detect_set_agg$summary

## -----------------------------------------------------------------------------
#  Create pathogen data table
transmission_data <- data.frame(
  pathogen = c("a", "b"),
  inf_dc_min = c(0.05, 0.3),
  inf_dc_max = c(0.08, 0.4)
)

transmission_data_keys <- list(transmission_data = list(
  cols = c("pathogen", "inf_dc_min", "inf_dc_max"),
  keys = c("pathogen")
))

transmission_mctable <- data.frame(
  mcnode = c("inf_dc"),
  description = c("Probability of infection via direct contact"),
  mc_func = c("runif"),
  from_variable = c(NA),
  transformation = c(NA),
  sensi_analysis = c(FALSE)
)
dir_contact_exp <- quote({
  dir_contact <- no_detect * inf_dc
})

transmission <- eval_module(
  exp = c(dir_contact = dir_contact_exp),
  data = transmission_data,
  mctable = transmission_mctable,
  data_keys = transmission_data_keys,
  prev_mcmodule = imports_wif
)

mc_network(transmission, legend = TRUE)

## -----------------------------------------------------------------------------
intro <- combine_modules(imports_wif, transmission)
intro <- at_least_one(intro, c("no_detect", "dir_contact"), name = "total")
intro$node_list$total$summary

mc_network(intro, legend = TRUE)

## -----------------------------------------------------------------------------
sample_mcnode <- mcstoc(runif,
  min = mcdata(c(NA, 0.2, -Inf), type = "0", nvariates = 3),
  max = mcdata(c(NA, 0.3, Inf), type = "0", nvariates = 3),
  nvariates = 3
)

# Replace NA and Inf with 0
clean_mcnode <- mcnode_na_rm(sample_mcnode)

## -----------------------------------------------------------------------------
# Custom name for at_least_one()
intro <- at_least_one(intro, c("no_detect", "dir_contact"), name = "custom_total")

# Custom name for agg_totals()
intro <- agg_totals(intro, "no_detect_set", name = "custom_agg")

# Custom suffix for agg_totals()
intro <- agg_totals(intro, "no_detect_set",
  agg_keys = c("scenario_id", "pathogen"),
  agg_suffix = "aggregated"
)

## -----------------------------------------------------------------------------
imports_wif <- add_prefix(imports_wif)

## -----------------------------------------------------------------------------
create_mcnodes(data = prevalence_region, mctable = imports_mctable)
# Nodes are created in the environment
h_prev

## -----------------------------------------------------------------------------
mc_summary(data = prevalence_region, mcnode = h_prev, keys_names = c("pathogen", "origin"))

