################################################################################
## Model Parameter Bootstrap Table Script
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Purpose:
##
## * Creates report-ready bootstrap parameter tables with transformed parameters specified
##  by yaml style parameter key.
## * Automatically generates '.png' and '.csv' files of formatted parameter tables 
##  and saves to 'results/table/model_name'.
################################################################################

### Packages ----------------------------
library(tidyverse)
library(pmtables)
library(here)
library(bbr)
library(magrittr)
library(yaml)
library(pmparams)
library(glue)

### Model run ----------------------------
modelRun <- 106   # Covariate model

### Parameter key yaml file --------------
key <- here("scripts", "pk-parameter-key.yaml")  

### Directories ----------------------------
scriptDir <- here("scripts")
modDir <- here("models", "pk")                # define model directory
tabDir <- here("results", "table", modelRun)  # saves to subfolder with model name
fs::dir_create(tabDir)

### Helper functions -------------------------
source(here("scripts", "functions", "functions-make-gt.R"))

# Set table options -------------------------
options(mrg.script = "parameter-table-boot-final.R",   # name of this script
        pmtables.dir = tabDir,         # location to save tables
        pmtables.path.type = "proj")   # include full filepath in footer

# Non-bootstrap parameters -------------------------
# Extract non-boot PK parameters and generate values for report table
sum <- read_model(here(modDir, modelRun)) %>%   
  model_summary()  # shows quick model summary 

# To see all raw model parameters
sum %>% param_estimates()

# Format parameter estimates
param_df <- sum %>% 
  define_param_table(.key = key) %>% 
  format_param_table(.prse = TRUE) %>% 
  select(-ci_95) 

# Print parameter names, descriptions, etc. to review
param_df

# Read in bootstrap results  ----------------------------
boot_run <- read_model(here(modDir, "106-boot"))

summarize_bootstrap_run(boot_run)
boot_est <- bootstrap_estimates(boot_run)

# Format bootstrap estimates
boot_df <- boot_est %>%
  define_boot_table(.key = key) %>% 
  format_boot_table() %>% 
  mutate(boot_ci_95 = paste0("[", boot_ci_95, "]"))

# Print parameter names, descriptions, etc. to review
boot_df 

# join tables ----------------------------
bootParam = left_join(param_df, boot_df, by = c("abb", "desc"))


# Full bootstrap parameter table ----------------------------

# Generate list of footnotes
footnotes <- c(
  param_notes(.ci = 95),
  boot_notes(.ci = 95, .n_run = nrow(boot_est))
)

# Create table
boot_params_all = bootParam %>% 
  make_boot_pmtable(.pmtype = "full") %>%
  st_notes(footnotes$ci, footnotes$corr,  # add abbreviations
           footnotes$cv, footnotes$rse,
           footnotes$se) %>%   
  st_notes_str() %>%                      # collapse all abbreviations to a string                        
  st_notes(footnotes$logTrans) %>%        # customize other notes
  st_notes(footnotes$boot_ci) %>% 
  st_notes(footnotes$cvOmegaEq, footnotes$cvSigmaEq) %>%
  st_files(output = glue("{modelRun}-pk-params-boot.docx"))   # specify source file name to be printed in footnotes

# Preview pmtables output
boot_params_all %>% st_as_image(border = "0.2cm 0.7cm 6.2cm 0.7cm")

# Convert and preview gt output
gt_table <- st_as_gt(boot_params_all)
gt_table

# Save
save_gt_docx(gt_table)
