
################################################################################
## EDA Tables Script
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Purpose:
##
## This document provides examples of the typical EDA
## tables you might include in your report. This is not 
## meant to be an exhaustive list and will not be appropriate
## for all projects. 
## It includes examples of using pmtables and how you can 
## leverage information in your dataset yspec to name columns etc.
##
################################################################################

### Packages ----------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(pmtables)
  library(yspec)
  library(here)
  library(magrittr)
  library(data.table)
  library(mrggsave)
  library(gt)
  library(flextable)
})


### Directories ------------------------------
scriptDir <- here("script")
tabDir <- here("results", "table", "eda")
dataDir <- here("data", "derived")

fs::dir_create(tabDir)

### Helper functions -------------------------
source(here("scripts", "functions", "functions-eda.R"))
source(here("scripts", "functions", "functions-make-gt.R"))

# Set table options ----------------------------
options(mrg.script = "eda-tables.R",   # current script
        pmtables.dir = tabDir,         # location to save tables
        pmtables.path.type = "proj")   # include full filepath in footer

### Read in dataset --------------------------
dat = fread(file = here(dataDir, "pk.csv"), na.strings = '.')

### spec and glossary details ----------------------------
spec <- ys_load(here(dataDir, "pk.yml"))
glo <- read_glossary(here("report/glossary.yaml"))

# Look at which namespaces are available in the yspec
ys_namespace(spec)

# uncomment to optionally include an alternative spec 
# This takes decode.long first (if available), then decode.tex (if available), then base
#spec <- ys_namespace(spec, c("long", "tex")) 

# Define units for later (using the alternative version of the spec)
units = ys_get_unit(spec, parens = TRUE)

# Grab flags specified in your spec
flags <- pull_meta(spec, "flags")

# Get a list of "short (units)" from spec (using the alternative version of the spec)
covlab = ys_get_short_unit(spec, title_case = TRUE)

### For PK data summary tables ---------------

## Filter to observations only
# i.e., filter to remove dosing records (keep BLQ).
# Remember to also filter out any records (aside from BLQ) excluded from the model
pkSum = dat %>% 
  filter(EVID != 1) %>% 
  ys_factors(spec, DOSE, STUDY) # Don't turn BLQ into a factor


## Create word tables -------
# Code initially uses pmtables for it's summary functions 
# and table formatting options.

# Data inventory table by dose - using pmtables
pkSumDose = pkSum %>%
  rename("Dose" = "DOSE") %>% 
  pt_data_inventory(by = "Dose") %>% 
  st_new() %>% 
  st_notes_sub("OBS", glossary_notes(glo, "OBS")) %>% 
  st_notes_glo(glo, "LLOQ") %>% 
  st_files(output = "pk-data-sum-dose.docx")

# Preview pmtables output
pkSumDose %>% st_as_image()

# Convert to gt output and preview 
gt_table <- st_as_gt(pkSumDose)
gt_table

# Save table as docx
save_gt_docx(gt_table)


# Data inventory table by dose and study - using pmtables
pkSumDoseStudyPMT = pkSum %>%
  pt_data_inventory(stacked = TRUE,
                    by = c('Dose group' = "DOSE"),
                    panel = as.panel("STUDY", prefix = "Study:")) %>%
  st_new() %>% 
  st_notes_sub("OBS", glossary_notes(glo, "OBS")) %>% 
  st_notes_glo(glo, "LLOQ") %>% 
  st_files(output = "pk-data-sum-dose-study.docx")

# Preview pmtables output
pkSumDoseStudyPMT %>% st_as_image()

# Convert to gt output and preview 
gt_table <- st_as_gt(pkSumDoseStudyPMT)
gt_table

# Save
save_gt_docx(gt_table)



### For covariate summary tables -------------

# Get categorical covariates of interest from the spec flags
catCovDF <- ys_select(spec, flags$catcov)
catCovNames <- ys_get_short(catCovDF)

# Get continuous covariates of interest from the spec flags
contCovDF <- ys_select(spec, flags$contcov)
contCovNames <- names(contCovDF)

# Filter full dataset to covariates of interest plus column for STUDY
covID = dat  %>% 
  # Remember to filter out any records excluded from the model here if applicable
  select(ID, STUDY, flags$catcov, flags$contcov) %>% 
  ys_factors(spec)

# Get baseline covariates
baselineCovs = covID %>% 
  distinct(ID, .keep_all = TRUE) 

#  -------------------------------------- -------------------------------------
# Combined continuous and categorical summaries
# -------------------------------------- --------------------------------------

# make table covariate summary table - using pmtables
contCatSumStudy = baselineCovs %>% 
  pt_demographics(
    cols_cat = flags$catcov,
    cols_cont = flags$contcov,
    span = c("Study" = "STUDY"),
    all_name = "Overall",
    table = covlab,               # renaming covariate values in rows
    fun = cont_long_custom        # using a custom summary function
  ) %>% 
  st_new() %>% 
  # st_align(Statistic = col_ragged(2.3)) %>% 
  st_filter(!(Statistic == "Missing" & Overall == "0")) %>% 
  st_notes("Categorical summary is count (percent)") %>% 
  st_notes_glo(glo, "nEDA", "SD", "IQR", "Min", "Max", "GFR", "CP") %>% 
  st_notes_detach(width = 1) %>% 
  st_files(output = "cont-cat-cov-sum-study.docx")

# Preview pmtables output
contCatSumStudy %>% st_as_image(border = "1.2cm 0.7cm 1.2cm 0.7cm")

# Convert to gt output and preview 
gt_table <- st_as_gt(contCatSumStudy)
gt_table

# Save
save_gt_docx(gt_table)


# If this combined covariate table gets too long consider using the tables below
# Below are examples of separate categorical or continuous covariate tables

#  -------------------------------------- -------------------------------------
# Categorical covariate summaries only
# -------------------------------------- --------------------------------------

# By study
catSumStudy = baselineCovs %>%
  pt_cat_long(
    cols = flags$catcov, 
    span = c("Study" = "STUDY"),
    all_name_span = "Overall", 
    table = catCovNames ) %>%
  st_new %>% 
  st_notes_sub("records", glossary_notes(glo, "nEDA", "CP")) %>% 
  st_files(output = "cat-cov-sum-study.docx")

# Preview pmtables output
catSumStudy %>% st_as_image() 

# Convert to gt output and preview
gt_table <- st_as_gt(catSumStudy)
gt_table

# Save
save_gt_docx(gt_table)


# By renal function
rfSumDoseStudy = pkSum %>% 
  distinct(ID, DOSE, STUDY, RF, .keep_all = TRUE) %>% 
  ys_factors(spec, DOSE, STUDY, RF) %>% 
  pt_cat_wide(
    cols = vars("Renal function" = "RF"),
    by = c("Dose Group" = "DOSE")
  ) %>% 
  st_new %>% 
  st_notes_sub("records", glossary_notes(glo, "nEDA")) %>% 
  st_files(output = "rf-dose-study.docx")

# Preview pmtables output
rfSumDoseStudy %>% st_as_image()

# Convert to gt output and preview 
gt_table <- st_as_gt(rfSumDoseStudy)
gt_table

# Save
save_gt_docx(gt_table)


#  -------------------------------------- --------------------------------------
# Continuous summaries
# -------------------------------------- --------------------------------------

# By study
contSumStudy = baselineCovs %>%
  pt_cont_long(
    cols = contCovNames,
    panel = as.panel("STUDY", prefix = "Study:"),
    table = covlab ) %>% 
  st_new %>% 
  st_notes_sub("records", glossary_notes(glo, "nEDA")) %>% 
  st_notes_glo(glo, "GFR") %>% 
  st_notes_detach() %>% 
  st_files(output = "cont-cov-sum-study.docx")

# Preview pmtables output
contSumStudy %>% st_as_image() 

# Convert to gt output and preview
gt_table <- st_as_gt(contSumStudy)
gt_table

# Save
save_gt_docx(gt_table)



#  -------------------------------------- -------------------------------------
# Alternative data disposition tables
# -------------------------------------- --------------------------------------

##' Please note the following tables are not part of MetrumRG's workflow but have 
##' been included as an example of how users can recreate the data summary table
##' provided in the TFL document. This option requires more manual coding by
##' the user than the proposed option above but is included for completeness. 
##' 
##' The column titled "Number (%) of excluded obs." was not included here because,
##' per AZ feedback, there is no consensus way of excluding observations. 
##' MetrumRG make a separate table summarising excluded data including the number
##' of records excluded and the reason for those exclusions. 
##' 

# Summarise the data by the covariates of interest by dose group
summary = pkSum %>%
  pt_data_inventory(by = c("Dose" = "DOSE"), 
                    summarize_all = FALSE)

# Extract the dataframe of summary data
df = summary$data

# Manipulate the table to recreate the requested format
df = df %>% 
  mutate(Total = Number.OBS + Number.BLQ,
         BLQ = paste0(Number.BLQ, " (", Percent.BLQ, ")")) %>% 
  select(DOSE, SUBJ = Number.SUBJ, Total, BLQ)

# Turn df into a formatted table
pkByDose = df %>% 
  st_new() %>% 
  st_rename("Dose" = "DOSE",
            "Number of...Subjects" = "SUBJ", 
            "Total number ...of obs." = "Total", 
            "Number (%)...of obs. below...the LLOQ" = "BLQ") %>% 
  st_center(DOSE = 'l') %>%
  st_notes_glo(glo, "LLOQ") %>% 
  st_notes("obs.: observations") %>% 
  st_notes_str() %>% 
  st_notes_detach() %>% 
  st_files(output = "alt-pk-data-sum-dose.docx")

# Preview pmtables output
pkByDose %>% st_as_image() 

# Convert to gt output and preview 
gt_table <- st_as_gt(pkByDose)
gt_table

# Save
save_gt_docx(gt_table)


# Summarise the data by the covariates of interest by dose group and study
summary2 = pkSum %>%
  pt_data_inventory(by = c("Dose" = "DOSE"),
                    panel = as.panel("STUDY", prefix = "Study: "), 
                    summarize_all = FALSE)

# Extract the dataframe of summary data
df2 = summary2$data

# Manipulate the table to recreate the requested format
df2 = df2 %>% 
  mutate(Total = Number.OBS + Number.BLQ,
         BLQ = paste0(Number.BLQ, " (", `Group percent.BLQ`, ")")) %>% 
  select(Study = STUDY, DOSE, SUBJ = Number.SUBJ, Total, BLQ)


# Turn df into a formatted table
pkByDoseStudy = df2 %>% 
  st_new() %>% 
  st_rename("Dose" = "DOSE",
            "Number of...Subjects" = "SUBJ",
            "Total number ...of obs." = "Total",
            "Number (%)...of obs. below...the LLOQ" = "BLQ") %>%
  st_center(DOSE = 'l') %>%
  st_panel("Study", "Study: ") %>%   ## To panel by study
  # st_clear_reps("Study") %>%       ## or to keep as a column without replicates
  st_notes_glo(glo, "LLOQ") %>% 
  st_notes("obs.: observations") %>%
  st_notes_str() %>%
  st_notes_detach() %>%
  st_files(output = "alt-pk-data-sum-dose-study.docx")

# Preview pmtables output
pkByDoseStudy %>% st_as_image() 

# Save
save_gt_docx(gt_table)
