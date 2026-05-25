## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(mcmodule)

## -----------------------------------------------------------------------------
# Single-level trial
imports_mcmodule <- trial_totals(
  mcmodule = imports_mcmodule,
  mc_names = "no_detect", # trial_p
  trials_n = "animals_n",
  mctable = imports_mctable
)

# Probability that at least one infected animal from an infected farm is not detected
mc_summary(imports_mcmodule,"no_detect_set")[,-1]

## -----------------------------------------------------------------------------
# Simple multilevel trial
imports_mcmodule <- trial_totals(
  mcmodule = imports_mcmodule,
  mc_names = "no_detect", # trial_p
  trials_n = "animals_n",
  subsets_n = "farms_n",
  subsets_p = "h_prev",
  mctable = imports_mctable
)

# Probability that at least one infected animal from at least one farm is not detected
mc_summary(imports_mcmodule,"no_detect_set", digits=2)[,-1]


