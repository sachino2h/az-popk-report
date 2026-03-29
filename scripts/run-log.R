### Run Log Script
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Purpose:
##
## * This file creates a run log in table format for evaluation of 
## key models of interest
##
################################################################################

### Packages -------------------------
library(bbr)
library(here)
library(magrittr)
library(tidyverse)
library(pmtables)
library(glue)
library(yspec)

### Helper functions -------------------------
source(here("scripts", "functions", "functions-run-log.R"))
source(here("scripts", "functions", "functions-make-gt.R"))

### Directories -------------------------
modDir <- here("models", "pk")
dataDir <- here("data", "derived")
tabDir <- here("results", "table")
fs::dir_create(tabDir)

### Set table options -------------------------
options(mrg.script = "run-log.R", 
        pmtables.dir = tabDir,        
        pmtables.path.type = "proj")

### glossary details -------------------------
glo <- read_glossary(here("report/glossary.yaml"))


##################################
# View ALL runs in designated model
# directory (modDir)
##################################

current_runs <- run_log(modDir) 
current_runs %>% View # pops open window with basic bbr run log summary

# Note: 
# This script includes only starred models in the run log table. 
# Stars can be added to additional models using the code below
# mod <- read_model(modDir, 102)
# mod <- mod %>% add_star()
#
# The "description" in the model object is used as the description of the 
# model structure in the run log. Descriptions can be added or modified using 
# the code below
# mod <- read_model(modDir, 102)
# If there is no description:
# mod <- mod %>% add_description("Two-compartment model.")
# To modify/replace existing description:
# mod <- mod %>% replace_description("One-compartment model.")


### Filter to "starred" runs ---------------
key_runs <- run_log(modDir) %>% 
  filter(star == T) 

# View key run numbers to make sure all desired runs are included
key_runs$run

# Note:
# The objective function value, condition number, and model structure are 
# taken from the bbr model object. The description of covariates, error model,
# and IIV are described in a yaml file.

## Use helper function to format run log
run_log_inputs <- run_log_report(key_runs$run)

# Manually add covariates, error structure, and IIV details into yaml file called below.
# Read in yaml model specification as a dataframe ----------------------------
mod_details <- yaml_as_df(here("scripts", "run-log-key.yaml")) %>% 
  rename("Run" = ".row")


## Join model details onto formatted bbr run log output
runlog_tab <- left_join(run_log_inputs,
                        mod_details,
                        by = "Run") 


##################################
# Generate run log table
##################################

# Create run log with pmtables
tab <- runlog_tab %>% 
  st_new() %>% 
  st_align(Run = col_ragged(1), 
           Structure = col_ragged(3.25),
           Covariates = col_ragged(3),
           Error = col_ragged(1.75),
           IIV = col_ragged(2.5),
           OFV = col_ragged(1.25)) %>% 
  st_rename("Cond. no." = "CondNo") %>% 
  st_notes_glo(glo, "IIV", "OFV", "CondNo") %>% 
  st_notes_detach(width = 1) %>% 
  st_files(output = "model-run-log.docx")

# Preview pmtables output
tab %>% st_as_image()

# Convert to gt output and preview 
gt_table <- st_as_gt(tab)
gt_table

# Save
save_gt_docx(gt_table)
