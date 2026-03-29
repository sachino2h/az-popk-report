
################################################################################
## Model Bootstrap Script
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Purpose:
##
## * This script sets up and executes bootstrap runs of bbr model objects 
## * Example summary calls are shown at the bottom
## * Additional summaries and plotting are in scripts/bootstrap-plot.R
##
################################################################################

library(dplyr)
library(bbr)
library(here)
library(ggplot2)
source(here("scripts", "functions", "bbr-helpers.R"))

runno <- "106"  ### Put the name of the model to bootstrap here

# Load the original model 
modDir <- here("models", "pk")
orig_mod <- read_model(here(modDir, runno))

# Check if the model file and data file are up to date before bootstrapping
print(check_up_to_date(orig_mod))

# TO PREVIEW WITH PRE-RUN EXAMPLE MODEL, uncomment this `read_model()` line
#   and skip the `new_bootstrap_run()`, `setup_bootstrap_run()`, 
#   and `submit_model()` calls below
# boot_run <- read_model(file.path(modDir, "106-boot"))

# Create new bootstrap object
boot_run <- new_bootstrap_run(orig_mod)

# Optionally inherit final parameter estimates
# boot_run <- inherit_param_estimates(boot_run)

# Sample data and create `n` model objects
# * Modify `n` to be the number of bootstrap replicates you want
# * Use `strat_cols` argument to stratify the sampling by specified columns
boot_run <- setup_bootstrap_run(boot_run, n = 200, strat_cols = "STUDY")

# Submit bootstrap models
# * By default, this submits the bootstrap models to run on the compute nodes,
#   in batches of 100
# * See ?submit_model.bbi_nmboot_model for other arguments and details
submit_model(boot_run)

# Open bootstrap submission log for monitoring
if (interactive()) {
  open_file_if_exists(boot_run, "OUTPUT")
}

# Check status of submitted bootstrap runs
get_model_status(boot_run)


# Summarize bootstrap run & store results
# * Once all models have finished, read in the results
# * This also stores all parameter estimates and other summary information in a
#   single RDS file, for quick access on subsequent `summarize_bootstrap_run()`
#   calls
if (check_nonmem_finished(boot_run)) {
  boot_sum <- summarize_bootstrap_run(boot_run)  
  
  # Print high-level summary of bootstrap run
  boot_sum %>% print()
}

if (interactive()) {
  # View quantiles of bootstrap parameter estimates
  #   and compare to parameter estimates from original model
  boot_comp_df <- param_estimates_compare(boot_sum)
  View(boot_comp_df)
  
  # Load all parameter estimates to a tibble
  boot_est_df <- bootstrap_estimates(boot_run)
  View(boot_est_df %>% select(-absolute_model_path, -bbi_summary, -termination_code))
}

# Optionally clean up individual model runs **after** running `summarize_bootstrap_run()`
# * Once `summarize_bootstrap_run()` has been called on the completed bootstrap, the
#   summary information from all runs will be stored on disk for later use
# * Set `CLEANUP_BOOT_RUNS <- TRUE` and run the code below to delete all of the raw 
#   NONMEM output from the individual bootstrap runs
# * See ?cleanup_bootstrap_run for more details
CLEANUP_BOOT_RUNS <- FALSE
if (CLEANUP_BOOT_RUNS && check_nonmem_finished(boot_run)) {
  summarize_bootstrap_run(boot_run)
  cleanup_bootstrap_run(boot_run)  
}


