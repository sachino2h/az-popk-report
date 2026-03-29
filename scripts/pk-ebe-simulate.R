################################################################################
## PK EBE Simulation
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Purpose:
##
#' - Read in EBE from final model run 106
#' - Simulate each individual to steady state at 25 mg daily
#' - Summarize steady-state Cmax, Cmin, and AUC
################################################################################

### Packages ----------------------------
library(dplyr)
library(tidyr)
library(mrgsolve)
library(bbr)
library(mrgmisc)
library(pmtables)
library(pmplots)
library(data.table)
library(here)
library(forcats)
library(mrggsave)
library(glue)
library(yspec)
library(fs)
library(magrittr)

### Model run ----------------------------
runno <- 106

# Set script options ----------------------------
options(
  mrg.script = "scripts/pk-ebe-simulate.R", 
  bbr.verbose = FALSE, 
  pmtables.dir = here(glue("results/table/{runno}")), 
  mrggsave.dir = here(glue("results/figure/sim/{runno}")), 
  mrggsave.dev = "png,pdf",
  pmtables.path.type = "proj",
  mrgsolve.soloc = here("scripts/build")
)

### Directories ----------------------------
dir_create(getOption("pmtables.dir"))
dir_create(getOption("mrggsave.dir"))

### Helper functions -------------------------
source(here("scripts", "functions", "functions-make-gt.R"))

#' Spec and glossary ----------------------------
spec <- ys_load(here("data/derived/pk.yml")) %>% ys_extend()
glo <- read_glossary(here("report/glossary.yaml"))

#' The simulation model ----------------------------
mod <- mread(here(glue("models/pk/simmod/{runno}.mod")))

#' This will calculate Cmax to the nearest 0.1 hour; will also influence AUC
#' calculation by trapezoidal rule;  this can be customized (made smaller) 
#' for more precision
mod <- update(mod, delta = 0.1, end = 24)

#' Read in the NONMEM output to get post-hoc parameters
data <- nm_join(here(glue("models/pk/{runno}")))

#' Just grab dose records
dose_rec <- filter(data, EVID==1)

#' Boil down to one record per subject
pars <- distinct(dose_rec, ID, .keep_all = TRUE)

#' Check that we have all input parameters in the data
#' already; we'll require the ETAs at simulation time
param_tags(mod)
check_data_names(pars, mod)

#' 25 mg once daily dosing to steady state
dose <- mutate(pars, II=24, SS=1, TIME=0, EVID=1, CMT=1, AMT=25)
dose <- mutate(dose, DOSE = AMT)
count(dose, RF, AMT)

#' Simulate
out <- mrgsim_df(
  mod, 
  dose, 
  recover = "ACTARM,RF,CL,DOSE", 
  etasrc = "data.all",
  recsort = 3
)

#' Calculate exposures
#' AUC is calculated by trapezoidal rule with unit adjustment
pk1 <- 
  out %>% 
  group_by(ID, DOSE) %>% 
  summarise(
    CMAXss = max(IPRED), 
    CMINss = min(IPRED), 
    AUCss = auc_partial(TIME, IPRED)/1000, 
    .groups = "drop"
  )

id <- distinct(data, ID, USUBJID, STUDY, RF, WT)
pk2 <- left_join(pk1, id, by = "ID")
pk2 <- select(pk2, STUDY,  USUBJID, everything()) 

#' Calculate weight quartiles in normal function and mild impairment
#' Cut into 4 groups
pk <- 
  pk2 %>% 
  filter(RF %in% c("mild", "norm")) %>% 
  mutate(WCUT = cut_number(WT, n = 4)) %>%  
  mutate(QN = as.integer(WCUT)) %>%     
  mutate(Q = paste0("Q", QN, " ", WCUT)) %>% 
  ungroup()

#' Check the breaks
pk %>% count(QN, Q, WCUT)

#' # Plots
#' 
#' All are for normal renal function or mild impairment, 25 mg
jitt <- position_jitter(h = 0, w = 0.1, s = 369)
ys <- list(limits = c(0, NA))

#' Pull these from the spec, adding body weight quartile; will be used later
lab <- axis_col_labs(spec, vars = c("AUCss", "CMAXss", "CMINss"))
bwq <- "Body weight quartile"
lab[["BWQ"]] <- glue("Q//{bwq}")

#' ## AUC,ss
p1 <- pm_box(
  pk, 
  x = lab[["BWQ"]], 
  y = lab[["AUCss"]], 
  points = list(position = jitt), 
  ys = ys
); p1

mrggsave_last(stem = "{runno}-ebe-wtq-auc")

#' ## Cmax,ss
p2 <- pm_box(
  pk, 
  x = lab[["BWQ"]], 
  y = lab[["CMAXss"]], 
  points = list(position = jitt), 
  ys = ys
); p2

mrggsave_last(stem = "{runno}-ebe-wtq-cmax")

#' ## Cmin,ss
p3 <- pm_box(
  pk, 
  x = lab[["BWQ"]], 
  y = lab[["CMINss"]], 
  points = list(position = jitt), 
  ys = ys
); p3

mrggsave_last(stem = "{runno}-ebe-wtq-cmin")

#' # Generate table
#' 
#' Will do this in 2 parts: 
#' 1. Body weight summary - Min, Median, Max
#' 2. Exposure summary 
#'   - Geometric Mean (Geometric SD)
#'   - Median (Range)
#' 

#' First, make the data set long
long <- pivot_longer(pk, cols = c("AUCss", "CMAXss", "CMINss"))
long <- mutate(long, name = fct_inorder(name))

#' Save this out 
fwrite(
  x = long,
  file = here("data/derived/pk-ebe-simulate.csv"),
  na = ".",
  quote = FALSE
)

#' Generate a quantile label with number of subjects
long <- 
  long %>% 
  group_by(name, QN) %>% 
  mutate(n = paste0("n=", n())) %>% 
  mutate(Qn = paste0("Q", QN, "...", n)) %>% 
  ungroup()

#' Body weight summary
weight <- filter(long, name=="AUCss")
weight <- 
  weight %>% 
  group_by(Qn) %>% 
  summarise(
    Minimum = min(WT), 
    Median = median(WT), 
    Maximum = max(WT), 
    .groups = "drop"
  )

weight <- mutate(weight, across(c(Minimum:Maximum), sig))
weight <- pivot_longer(weight, cols = c(Minimum:Maximum), names_to = "stat")
weight <- pivot_wider(weight, names_from = "Qn")

#' The panel name; this will be decoded by the spec
weight <- mutate(weight, name = "WT")

#' Summarize geometric mean (geometric sd) and median (range) for each exposure
summ1 <- 
  long %>% 
  group_by(name, Q, Qn) %>% 
  summarise( 
    Gmean = exp(mean(log(value))), 
    Gsd = exp(sd(log(value))), 
    Med = median(value), 
    Min = min(value), 
    Max = max(value),
    Range = paste(c(sig(Min), sig(Max, maxex = Inf)), collapse = ", "),
    statmean = paste(sig(Gmean), ensure_parens(sig(Gsd))),
    statmedian = paste(sig(Med), ensure_parens(Range)),
    .groups = "drop"
  )

summ2 <- pivot_longer(summ1, cols = contains("stat"), names_to = "stat")

#' Reshape exposure metrics
summ3 <- select(summ2, name, stat, Qn, value)
summ4 <- pivot_wider(
  summ3, 
  names_from = "Qn",
  values_from = "value"
)

#' Recode each stat
summ4 <- mutate(
  summ4, 
  stat = case_match(
    stat, 
    "statmean" ~ "GeoMean (GeoSD)", 
    "statmedian" ~ "Median (Range)",
  )
)

#' Bind it all together and arrange
summall <- bind_rows(summ4, weight)
summall <- mutate(summall, name = fct_inorder(name))
summall <- arrange(summall, name)
summall <- select(summall, name, stat, everything())

#' Better names for the panel headers
table <- ys_select(spec, AUCss, CMAXss, CMINss, WT) %>% ys_get_short_unit()
summall <- mutate(summall, name = factor(name, labels = table))

#' 
#' Make the table
#'
#' Make some table notes from the spec
exposures <- ys_select(spec, AUCss, CMAXss, CMINss)

exposure_notes <- paste(
  unlist(ys_get_short(exposures)), 
  unlist(ys_get_label(exposures)), 
  sep = ": "
)

tab <- summall %>%
  st_new() %>% 
  st_panel("name") %>%
  st_center(.l = "stat") %>% 
  st_blank(stat) %>% 
  st_span(title = bwq, contains("Q")) %>% 
  st_notes(
    "Q1-Q4: first through fourth quantiles",
    exposure_notes
  ) %>%
  st_notes_glo(glo, GeoMean, GeoSD, collapse = NULL) %>% 
  st_notes_detach(width = 1) %>% 
  st_caption(
    "Steady-state exposure by body weight quartiles. All exposures
   were simulated for 25 mg daily dose in patients with normal renal 
   function or mild impairment."
  ) %>% 
  st_files(output = glue("{runno}-ebe-wtq.docx"))

# Preview pmtables output
tab %>% st_as_image()

# Convert and preview gt output
gt_table <- st_as_gt(tab)
gt_table

# Save
save_gt_docx(gt_table)
