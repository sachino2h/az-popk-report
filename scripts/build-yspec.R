################################################################################
## Build yspec YAML file
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Purpose:
##
## This script provides an example of generating a dataset-specific 
## specification (spec) YAML file, to be used downstream by the yspec package.
## 
## The user needs a NONMEM ready dataset, as well as XLSX data specification
## files. For more information on the specific inputs and usage of this 
## script, see the document/README-build-yspec.md file, in this repo.
## 
## More information on specification files: 
## https://metrumresearchgroup.github.io/yspec/articles/yspec.html
##
## This script is intended to generate the initial specification file only, 
## additional updates during the project should be made directly to the 
## dataset-specific YAML file(s).
################################################################################


# Environment -------------------------------------------------------------
library(dplyr)
library(readxl)
library(here)
library(yspec)

# Source the updated create_yaml_spec function
source(here("scripts/functions/create-yaml-spec.R"))

# Inputs ------------------------------------------------------------------
code_list_loc <- here("data/example-specs/code_list.xlsx")
spec_file_loc <- here("data/example-specs/pk_spec.xlsx")
data_set_loc <- here("data/derived/pk.csv")


# Code List ---------------------------------------------------------------
code_list <- 
  read_xlsx(
    path = code_list_loc, 
    sheet = "Sheet1"
  ) %>%
  select(
    column = `Code Names`,
    decode = Codes,
    values = Values
  )

# Spec --------------------------------------------------------------------
spec_data <- 
  read_xlsx(
    path = spec_file_loc, 
    sheet = "Dataset Specifications",
    skip = 1
  ) %>% 
  clean_column_names() %>% 
  select(
    column = Variable_Name,
    short = SAS_Label,
    type = SAS_Type,
    unit = Unit
  )

### Add SETUP__ options --------------------------
## Manually add your flags here
## Users can add as many or as few flag categories as they wish. If you do not 
## need a certain flag, remove it entirely from the flags list.
setup_opts <- list(
  extend_file = "pk-extend.yml",   # for more detail, see https://metrumresearchgroup.github.io/ysp-book/extension.html
  flags = list(
    "contcov" = c("AGE", "WT", "ALB", "EGFR"),       #eda: Baseline continuous covariate
    "catcov" = c("SEX", "CP", "RF"),                 #eda: Baseline categorical covariate
    "diagContCov" = c("AGE", "WT", "ALB", "EGFR"),   #diagnostics: Baseline continuous covariate
    "diagCatCov" = c("RF", "CP")                     #diagnostics: Baseline categorical covariate    
  )
)

# Create yspec ------------------------------------------------------------
create_yaml_spec(
  description = "PK Dataset",
  spec_data = spec_data,
  code_list = code_list,
  output_file = here("data/derived/pk.yml"),
  setup_opts = setup_opts
)

# Read in newly created yspec ---------------------------------------------
pk_spec <- yspec::ys_load(here("data/derived/pk.yml"))

# Load data ---------------------------------------------------------------
pk <- data.table::fread(file = data_set_loc, na.strings = '.')

# Check against data ------------------------------------------------------
yspec::ys_check(
  data = pk,
  spec = pk_spec
)

# Use yspec to generate example outputs -------------------------------------------
pk <- pk %>% yspec::ys_add_factors(pk_spec)

pk %>%
  pmtables::pt_data_inventory(
    by = c("Study" = "STUDY_f")
  ) %>%
  pmtables::st_new() %>% 
  pmtables::st_as_image()
