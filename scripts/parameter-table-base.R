################################################################################
## Model Parameter Table Script
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Purpose:
##
## * Creates report-ready parameter tables with transformed parameters specified
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
modelRun = 102 # Base model

### Parameter key yaml file --------------
key <- here("scripts", "pk-parameter-key.yaml")  

### Directories ----------------------------
scriptDir <- here("scripts")
modDir <- here("models", "pk")                # define model directory
tabDir <- here("results", "table", modelRun)  # saves to subfolder with model name
fs::dir_create(tabDir)

### Helper functions -------------------------
source(here("scripts", "functions", "functions-make-gt.R"))

# Set table options ----------------------------
options(mrg.script = "parameter-table-base.R",  # name of this script
        pmtables.dir = tabDir,         # location to save tables
        pmtables.path.type = "proj")   # include full filepath in footer

### Glossary details ----------------------------
glo <- read_glossary(here("report/glossary.yaml"))

# Read in base model and format output in dataframe ----------------------------
sum <- read_model(here(modDir, modelRun)) %>%   
  model_summary()  # shows quick model summary 

# To see all raw model parameters
sum %>% param_estimates()

# Format parameter estimates
param_df <- sum %>% 
  define_param_table(.key = key) %>% 
  format_param_table(.prse = TRUE) %>% 
  mutate(ci_95 = paste0("[", ci_95, "]")) %>%
  # As of pmparams 0.2.1, the shrinkage column is placed between the estimate
  # column and the CI column. The next pmparams release will restore the
  # original order (i.e. put the CI column right after the estimate). Manually
  # restore the order in the meantime.
  #
  # https://github.com/metrumresearchgroup/pmparams/pull/112
  relocate(shrinkage, .after = ci_95)

# Print parameter names, descriptions, etc. to review
param_df 


## Full parameter table -------------------------------------

params_all = param_df %>% 
  make_pmtable(.pmtype = "full")  %>% 
  st_left(desc = col_ragged(4.5), 
          value = col_ragged(2.5, center = T, ragged = "no")) %>%
  st_span("Base model", value:pRSE) %>% 
  # st_select(-greek) %>%           # optionally remove greek column
  st_notes_glo(glo, CI95) %>% 
  st_notes(param_notes()$corr,    # add abbreviations
           param_notes()$cv, param_notes()$rse,
           param_notes()$se) %>%   
  st_notes_str() %>%                # collapse all abbreviations to a string 
  st_notes(param_notes()$logTrans,  # customize other notes
           param_notes()$ciEq,      
           param_notes()$cvOmegaEq, 
           param_notes()$cvSigmaEq) %>%
  st_files(output = glue("{modelRun}-pk-params.docx"))

# Preview pmtables output
params_all %>% st_as_image(border = "0.2cm 0.7cm 2.4cm 0.7cm")

# Convert to gt output and preview
gt_table <- st_as_gt(params_all)
gt_table

# Save
save_gt_docx(gt_table)
