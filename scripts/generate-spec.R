################################################################################
## Generate Spec Script
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Note:
##
## This script has been superseded by scripts/build-spec.R, which creates a YAML
## specification file from AZ XLSX data specification files. Use this script
## only if your project is not able to use scripts/build-spec.R.
##
## Purpose:
##
## This script provides an example of generating a dataset-specific 
## specification (spec) file for an assembled dataset using a lookup.yaml.
## 
## The user needs a NONMEM ready dataset and lookup.yaml before running this script. 
## The lookup.yaml should be customized to describe the data in the current project. 
## For example, check the study name decodes, categorical and continuous covariates 
## included and their decodes or units, respectively. 
## 
## More information on specification files: 
## https://metrumresearchgroup.github.io/yspec/articles/yspec.html
##
## This script is intended to generate the initial specification file only, 
## additional updates during the project should be made directly to the 
## lookup and dataset-specific yamls.
################################################################################

### Packages ----------------------------
suppressPackageStartupMessages(library(tidyverse))
library(yspec)
library(here)
library(magrittr)
library(data.table)

### Directories ------------------------------
# Location of dataset. 
dataDir <- here("data", "derived") 

# Location of lookup YAML file. 
# Generated spec will be saved to this file location as well.
specDir <- here("data", "example-specs")

### Read in dataset --------------------------
dat <- fread(file = here(dataDir, "pk.csv"), na.strings='.') 

### Read in project lookup.yml ----------------------------
lookup <- ys_load(here(specDir, "lookup.yml")) %>% as.data.frame()

### Create spec header --------------------------
## This can be edited here or in constructed spec at the end. 
# For further customization options, go to: 
# https://metrumresearchgroup.github.io/yspec/articles/reference.html
spec_header <- c('lookup_file: [lookup.yml]',
                 'description: UPDATE PROJECT DESCRIPTION HERE',
                 'sponsor: AstraZeneca',
                 'projectnumber: azn0000')

### Add flags --------------------------
## Manually add your flags here
## Users can add as many or as few flag categories as they wish. If you do not 
## need a certain flag, remove it entirely from the flags list.
flags <- c('contcov: [AGE, WT, ALB, EGFR]',         #eda: Baseline continuous covariate
           'catcov: [SEX, CP, RF]',                 #eda: Baseline categorical covariate
           'diagContCov: [AGE, WT, ALB, EGFR]',     #diagnostics: Baseline continuous covariate
           'diagCatCov: [RF, CP]')                  #diagnostics: Baseline categorical covariate

### Check that all variables in dataset are in lookup --------------------------
## This should be empty, i.e., all dataset columns should be described in the lookup.yml. 
## If this is not empty, the dataset includes one or more columns not described 
## in the lookup.yml.
manual_add_to_lookup <- names(dat)[!(names(dat) %in% lookup$name)]
manual_add_to_lookup 
## If needed, update the lookup.yml manually
## file.edit(here(specDir, "lookup.yml"))

### Select the dataset variables from the lookup --------------------------
lookup_1 <- lookup %>% 
  filter(name %in% names(dat)) %>%
  pull(name) %>% 
  paste0(":") 

### Generate full spec --------------------------
final <- c("SETUP__:", paste(" ", spec_header), 
           "  flags:", paste("   ", flags), 
           lookup_1, "")

### Save out full spec --------------------------
spec_filepath <- here(specDir, "pk-example.yml")

## Helper function for writing out spec
writeSpec = function(.spec, .filepath, .overwrite = FALSE){
  # check if the file exists, if not write it out
  if (!file.exists(.filepath) | .overwrite) {
    writeLines(.spec, .filepath)
  } else {
    stop("File already exists. To overwrite it, update `.overwrite` to TRUE")
  }
}

## Write out spec
writeSpec(
  .spec = final, 
  .filepath = spec_filepath, 
  .overwrite = FALSE
)

### Load in the spec --------------------------
spec <- load_spec(spec_filepath)
ys_check(dat, spec)
## If this fails, you will likely need to modify the lookup.yml file 
