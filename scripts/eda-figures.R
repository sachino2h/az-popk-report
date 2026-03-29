################################################################################
## EDA Figures Script
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Purpose:
##
## This document provides examples of the typical EDA
## figures you might include in your report. This is not 
## meant to be an exhaustive list and will not be appropriate
## for all projects. 
## It includes examples of using pmplots and how you can 
## leverage information in your dataset yspec to name panels/axis etc.
##
################################################################################

### Packages ----------------------------
library(tidyverse)
library(yaml)
library(yspec)
library(pmplots)
library(mrggsave)
library(mrgmisc)
library(here)
library(data.table)

### Directories ----------------------------
scriptDir = here("script")
figDir = here("results", "figure", "eda")
dataDir = here("data", "derived")

if(!dir.exists(figDir)) dir.create(figDir)

### Helper functions ----------------------------
asNum  = function(f){ return(as.numeric(as.character(f))) }

### Set any script options ----------------------------
options(mrg.script = "eda-figures.R", 
        mrggsave.dir = figDir,
        mrggsave.dev = "pdf, png")

theme_set(theme_bw() + theme(legend.position = "bottom"))

options(ggplot2.discrete.colour = RColorBrewer::brewer.pal(name = "Dark2", n = 8))
options(ggplot2.discrete.fill = RColorBrewer::brewer.pal(name = "Dark2", n = 8))

### Read in dataset ----------------------------
dat = fread(file = here(dataDir, "pk.csv"), na.strings = '.')

### yspec details ----------------------------

# load in the spec file
spec = ys_load( here(dataDir, "pk.yml")) %>% 
  ys_extend()


# look at which namespaces are available in the yspec
ys_namespace(spec)

# uncomment to optionally include 
#  a combination of plot and tex alternative spec 
#spec = ys_namespace(spec, c("plot", "tex"))

# grab flags specified in pk.yml
flags <- pull_meta(spec, "flags")

# Use spec to grab column labels for plots'
contCovDF = ys_select(spec, flags$contcov)
contCovNames = names(contCovDF) 
contCovarListTex = axis_col_labs(spec, 
                                 contCovNames, 
                                 title_case = TRUE, 
                                 short = 10) 

# To manually change facet labels, in this case, put units on a new line
contCovarListTex["EGFR"] = "EGFR//EGFR \n(ml/min/1.73m^2)"


### Refactor table for plots ----------------------------
dat2 = dat %>% 
  mutate(DVNORM = DV/DOSE) %>% 
  ys_factors(spec) 


### Make a table of unique covariates ----------------------------
covar = dat2 %>% 
  distinct(ID, .keep_all = TRUE) %>% 
  rename(Study = STUDY) %>% 
  select(ID, AMT, LDOS, NUM, Study, flags$catcov, 
         flags$contcov) 

### Boxplots of Continuous vs categorical ----------------------------

# Continuous covs vs renal function group
renalContCat = covar %>% 
  wrap_cont_cat(x = axis_col_labs(spec, "RF"),   
                y = contCovarListTex,
                use_labels = TRUE
  )

mrggsave_last(stem = "cont-cat-renal",
         width = 6, height = 7)

## Continuous vs hepatic function group
hepContCat = covar %>% 
  wrap_cont_cat(x = axis_col_labs(spec, "CP"),
                y = contCovarListTex,
                use_labels = TRUE
  ) 

mrggsave_last(stem = "cont-cat-hepatic",
              width = 6, height = 7)

### Continuous covariate correlation plot ----------------------------

contVScont <-  covar %>%
  pairs_plot(y = contCovarListTex)

mrggsave(contVScont, stem = "cont-vs-cont", width = 6, height = 7)


### Concentration-time plots ----------------------------
dat3 = dat2 %>% filter(EVID_v == 0, BLQ_v == 0)

xLabT = ys_get_short_unit(ys_select(spec, TIME))
xLabTAD = ys_get_short_unit(ys_select(spec, TAD))
yLab = ys_get_short_unit(ys_select(spec, DV))
yLabNorm = ys_get_short_unit(ys_select(spec, DVNORM))
legendDose = ys_get_short(ys_select(spec, DOSE))


## Concentration vs time
p = ggplot(data = dat3, aes(x = TIME, y = DV)) +
  geom_point(aes(color = DOSE)) +
  geom_line(aes(group = ID, color = DOSE), alpha = 0.3)  + 
  facet_wrap( ~STUDY) +
  labs(x = xLabT, y = yLab, colour = legendDose)

mrggsave_last(stem = "conc-time-linear", width = 6, height = 7)

pLog = p + scale_y_log10()
mrggsave_last(stem = "conc-time-log", width = 6, height = 7)


## Dose-norm concentration vs time
pDN = ggplot(data = dat3, aes(x = TIME, y = DVNORM)) +
  geom_point(aes(color = DOSE)) +
  geom_line(aes(group = ID, color = DOSE), alpha = 0.3)  + 
  facet_wrap( ~STUDY) +
  labs(x = xLabT, y = yLabNorm, colour = legendDose)

mrggsave_last(stem = "dn-conc-time-linear", width = 6, height = 7)

pDNlog = pDN + scale_y_log10()
mrggsave_last(stem = "dn-conc-time-log", width = 6, height = 7)

## Concentration vs tad
pTAD = ggplot(data = dat3, aes(x = TAD, y = DV)) +
  geom_point(aes(color = DOSE)) +
  geom_line(aes(group = ID, color = DOSE), alpha = 0.3)  + 
  facet_wrap( ~STUDY) +
  labs(x = xLabTAD, y = yLab, colour = legendDose)

mrggsave_last(stem = "conc-tad-linear", width = 6, height = 7)

pTADLog = pTAD + scale_y_log10()
mrggsave_last(stem = "conc-tad-log", width = 6, height = 7)
