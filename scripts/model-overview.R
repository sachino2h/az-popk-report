################################################################################
## Model Overview Script
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Purpose:
##
## * This file should be populated with key models at the end of the model
##   development process, before final QC
## * Executes NONMEM models (via bbr)
## * Creates goodness-of-fit diagnostic plots as report-ready 
##   PNG files in results/diagnostics
##   diagnostic plots as report-ready PNG files in results/diagnostics
## * Additionally creates interactive HTML files of diagnostic plots, to 
##   facilitate easier review and QC
##
################################################################################

library(bbr)
library(here)
library(magrittr)

# source functions for rendering diagnostics
source(here("scripts", "functions", "functions-diagnostics.R"))

# Identify models of interest
modDir <- here("models", "pk")
BASE_MODEL <- "102"   ### Put the name of your base model here
FINAL_MODEL <- "106"  ### Put the name of your final model here

# Change to TRUE to re-run the NONMEM models
RUN_MODELS <- FALSE 

# Remember to set the path to yspec YAML file, and any namespace options you
# want to use (or NULL if no namespaces needed) at the top of each diagnostic 
# template once. If there are other 'params' that will be consistent throughout
# your project we recommend defining them at the top on the templates once 
# rather than having to pass them in on every call. 

###################
# Base Model
###################

# Load the model in bbr and print any annotation to the console
base_mod <- read_model(file.path(modDir, BASE_MODEL))
print(base_mod)

# Check if the model file and data file are up to date 
#   Should return TRUE for both, if FALSE then the file(s) 
#   have changed since the model was last executed
print(check_up_to_date(base_mod))

# Print high-level model summary
print(model_summary(base_mod))

# Re-submit the model, if necessary (set RUN_MODELS <- TRUE above to re-run)
#   This may be needed to check reproducibility, or if 
#   the model or data have changed
if (isTRUE(RUN_MODELS)) {
  submit_model(
    base_mod # add extra arguments here for parallelization, etc.
  )  
}


# Render diagnostics (general GOF plots)
#   This will create an HTML document for interactive review,
#   as well as write out individual .pdf and .png files for each figure
model_diagnostics(
  .mod = base_mod,
  .p = list(
    cont_cov_flag = "diagContCov", 
    cat_cov_flag = "diagCatCov",
    eta_names = c("ETA1//ETA-KA", "ETA2//ETA-V/F", "ETA3//ETA-CL/F"),
    run_mrggsave = TRUE # set to FALSE if you only want HTML
  ),
  template =  here("scripts", "templates", "pk-diagnostics.Rmd")
) %>%
  browseURL()     # this will open the HTML doc in your browser

# Render individual diagnostics 
# (individual DV / PRED /IPREF versus time / tad - linear and log scale)
#   This will create an HTML document for interactive review,
#   as well as write out a summary pdf file for each set of figures
model_diagnostics(
  .mod = base_mod,
  # .p = list(log_dv = TRUE),   ## optionally pass in document parameters
  template = here("scripts", "templates", "id-dv-pred-plots.Rmd")
) %>%
  browseURL()  # this will open the HTML doc in your browser


###################
# Final Model
###################

# Load the model in bbr and print any annotation to the console
final_mod <- read_model(file.path(modDir, FINAL_MODEL))
print(final_mod)

# Check if the model file and data file are up to date 
#   Should return TRUE for both, if FALSE then the file(s) 
#   have changed since the model was last executed
print(check_up_to_date(final_mod))

# Print high-level model summary
print(model_summary(final_mod))

# Compare base model and final model model files
model_diff(final_mod, base_mod)

# Re-submit the model, if necessary (set RUN_MODELS <- TRUE above to re-run)
#   This may be needed to check reproducibility, or if 
#   the model or data have changed
if (isTRUE(RUN_MODELS)) {
  submit_model(
    final_mod # add extra arguments here for parallelization, etc.
  )  
}

# Render diagnostics (general GOF plots)
#   This will create an HTML document for interactive review,
#   as well as write out individual .png files for each figure
model_diagnostics(
  .mod = final_mod,
  .p = list(
    cont_cov_flag = "diagContCov", 
    cat_cov_flag = "diagCatCov",
    eta_names = c("ETA1//ETA-KA", "ETA2//ETA-V/F", "ETA3//ETA-CL/F"),
    run_mrggsave = TRUE # set to FALSE if you only want HTML
  ),
  template =  here("scripts", "templates", "pk-diagnostics.Rmd")
) %>%
  browseURL()     # this will open the HTML doc in your browser

# Render individual diagnostics 
# (individual DV / PRED /IPREF versus time / tad - linear and log scale)
#   This will create an HTML document for interactive review,
#   as well as write out a summary pdf file for each set of figures
model_diagnostics(
  .mod = final_mod,
  # .p = list(log_dv = TRUE),   ## optionally pass in document parameters
  template = here("scripts", "templates", "id-dv-pred-plots.Rmd")
) %>%
  browseURL()  # this will open the HTML doc in your browser
