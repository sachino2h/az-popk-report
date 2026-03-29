################################################################################
## Model Diagnostics Script
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Purpose:
##
## * This file produces goodness-of-fit (GOF) diagnostic plots using 
##   Rmd templates (located in the script/template folder)
## * Assumes your model has been run previously
## * GOF diagnostic plots are saved as report-ready PNG files 
##   in results/diagnostics
## * Creates individual plots of DV, PRED, and IPRED vs TIME and TAD
##   as report-ready multipage pdf files in results/diagnostics/run
## * Additionally creates interactive HTML files of diagnostic plots, to 
##   facilitate easier review and QC
##
################################################################################


### Packages ----------------------------
library(tidyverse)
library(here)
library(rmarkdown)


### helper functions ----------------------------
source(here("scripts", "functions", "functions-diagnostics.R"))

### Model details -------------------
model_dir <- here("models", "pk")
mod_run <- 106

### RMD template -------------------
rmd_template <- here("scripts", "templates", "pk-diagnostics.Rmd")

# General goodness-of-fit plots

model_specifics = list(
  cont_cov_flag = "diagContCov", 
  cat_cov_flag = "diagCatCov",
  #run_mrggsave = TRUE, # uncomment to write individual PNG files for each figure
  eta_names = c("ETA1//ETA-KA", "ETA2//ETA-V/F", "ETA3//ETA-CL/F")
)

model_diagnostics(
  file.path(model_dir, mod_run),
  .p = model_specifics,
  template = rmd_template
) %>%
  browseURL()


## Individual plots of DV/PRED/IPRED versus TIME/TAD 

model_diagnostics(
  file.path(model_dir, mod_run),
  # .p = list(log_dv = TRUE),   ## optionally pass in document parameters
  template = here("scripts", "templates", "id-dv-pred-plots.Rmd")
) %>%
  browseURL()

