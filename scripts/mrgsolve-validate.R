################################################################################
## Validate mrgsolve model
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Purpose:
##
## This script runs a validation simulation, comparing output from the 
## mrgsolve model and a nonmem run.
##
################################################################################

library(here)
library(bbr)
library(mrgsolve)
library(dplyr)
library(glue)
library(ggplot2)
theme_set(theme_bw())

#' Run number
runno <- 106

#' Load the mrgsolve model
#' This code assumes that mrgsolve file name shares the same root as the 
#' NONMEM run number with `.mod` extension.
mod <- mread(glue(here("models/pk/simmod/{runno}.mod")))

#' Read in the model estimation data set
data <- nm_join(glue(here("models/pk/{runno}")))

#' Check that we have all expected columns
check_data_names(data, mod)

#' Simulate
out <- mrgsim_df(
  mod, 
  data, 
  etasrc = "data.all", 
  # Take `IPRED` from the data set and push it into the 
  # simulated output as `NMIPRED`
  recover = "EVID, NMIPRED = IPRED", 
  rtol = 1e-10 # Only need for ODE-based models
)

out <- filter(out, EVID==0)
head(out)

#' Compare NONMEM IPRED versus mrgsolve IPRED; if the model is coded 
#' correctly, we expect the two tools give very close answers
summary(out$NMIPRED - out$IPRED)

ggplot(out, aes(NMIPRED, IPRED)) + 
  geom_point(size = 1) + 
  geom_abline(intercept = 0, slope = 1, color = "red2") + 
  xlab("NOMMEM IPRED") + ylab("mrgsolve IPRED")
