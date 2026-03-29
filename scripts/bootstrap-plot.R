
################################################################################
## Bootstrap Summary & Plotting Script
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Purpose:
##
## * This script reads in and summarizes bootstrap runs that have previously
##   been executed by bbr (in scripts/bootstrap.R)
## * Histograms of parameter estimates are shown at the bottom
##
################################################################################

library(dplyr)
library(bbr)
library(here)
library(ggplot2)
library(tidyr)
library(pmtables)
library(pmparams)
library(forcats)
library(mrggsave)


runno <- "106"  ### Put the name of the original model that was bootstrapped here

modDir <- here("models", "pk")
figDir <- here("results", "figure")
if(!dir.exists(figDir)) dir.create(figDir)

options(
  mrggsave.dir = file.path(figDir, runno),
  mrg.script = "bootstrap-plot.R",
  mrggsave.dev = "png"
)

# Load the finished bootstrap run, as well as the original model
boot_run <- read_model(file.path(modDir, paste0(runno, "-boot")))
orig_mod <- read_model(get_based_on(boot_run))

# Check that the original model file and data file are up to date before comparing to bootstrap results
print(check_up_to_date(orig_mod))

# Parameter names
# * Load the full parameter names from the parameter key YAML file with `pmparams`
key <- orig_mod %>%
  pmparams::define_param_table(.key = "scripts/pk-parameter-key.yaml") %>%
  mutate(abb = forcats::fct_inorder(abb)) %>%
  dplyr::select(parameter_names, abb, desc, panel, trans)


# Summary info
# * Load bootstrap summary
# * Load quantiles of bootstrap estimates, as well as the original model's
#   parameter estimates
# * Preview comparison of original estimates to bootstrap quantiles
boot_sum <- summarize_bootstrap_run(boot_run)
boot_comp <- param_estimates_compare(boot_sum, .orig_mod = orig_mod) 
if (interactive()) {
  print(boot_comp)
}

# Pivot this tibble, for creating vertical lines in histogram plot
sum_data <- boot_comp %>% 
  tidyr::pivot_longer(
    cols = c("original","p50", "p2.5", "p97.5"), 
    names_to = "stat", values_to = "original"
  ) %>% left_join(key)

# Load all parameter estimates for plotting their distribution below
par <- bootstrap_estimates(boot_run, format_long = TRUE) %>% 
  left_join(key, by = "parameter_names")


# Filter to record type (e.g., THETAs)
record_type <- "THETA"
par_pl <- par %>% dplyr::filter(grepl(record_type, parameter_names))
sum_data_pl <- sum_data %>% dplyr::filter(grepl(record_type, parameter_names))

low_hi_sum <- sum_data_pl %>% dplyr::filter(stat != "original")
orig_sum <- sum_data_pl %>% dplyr::filter(stat == "original")


# Plot parameter distributions
p <- ggplot(par_pl, aes(x = estimate)) + facet_wrap(~abb, scales = "free") +
  geom_histogram(color = "white", alpha = 0.7) + 
  geom_vline(data = low_hi_sum, aes(xintercept = original), lwd = 1, color = "blue3") +
  geom_vline(data = orig_sum, aes(xintercept = original), lwd = 1, color = "red3") +
  theme_bw() + labs(x = "Value", y = "Count")

p

mrggsave_last(stem = "{runno}-bootstrap", height = 7.5, width = 7.5)
