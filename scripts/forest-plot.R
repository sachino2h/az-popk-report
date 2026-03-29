
################################################################################
## Forest Plot Script
## 
## Author: <add name>
## Date: <add date, for initial edits from template>
## Project: <add project identifier>
##
## Purpose:
##
## * This script creates a forest plot of covariate effects in the model.
## * There are two options shown for generating draws from a parameter uncertainty 
##   distribution:
##      Option 1: Reading in output from a bootstrap run
##      Option 2: Simulating from the covariance matrix using `simpar()`
## 
## * There are two options shown for determining the values of covariates for 
##      Option 1: Automatically select values representing quantiles of each 
##      covariate's distribution.
##      Option 2: Manually select values for each covariate.
##
## * Note: there are NO categorical covariates in this model; to enter 
##   covariate information for a discrete covariate, used a named vector
##   of values; there is an example below.
##
## * This script creates TWO example forest plots from example model (106.ctl) 
##   including covariates: WT, EGFR, ALB, AGE
##      Example 1: forest plot for calculated / structural parameters (CL as example)
##      Example 2: forest plot for derived parameters (Cmax,ss as example)
##
## * Please note that this script only provides examples of these two cases, and
##   the user should be prepared to make appropriate changes to fit the needs of 
##   the analysis. These examples are intended to be a starting place from which
##   the user can build and customize to suit their needs.
##
################################################################################

#' Required packages
library(tidyverse)
library(mrgsolve)
library(mrggsave)
library(here)
library(data.table)
library(yspec)
library(rlang)
library(bbr)
library(glue)
library(simpar)

modDir <- here("models", "pk")
simDir <- here("models", "pk", "simmod")
figDir <- here("results", "figure", "forest")
if(!dir.exists(figDir)) dir.create(figDir)
runno <- 106 # add run number

### Set any script options ----------------------------
options(mrg.script = "forest-plot.R", mrggsave.dir = figDir,
        mrggsave.dev = "pdf,png")

#' Load data specification object and modify
spec <- ys_load(here("data", "derived", "pk.yml"))

#' Pull in data/derived/pk-extend.yml and
spec <- ys_extend(spec)

#' Re-label Estimated GFR to eGFR and clearance to CL for plots
spec <- update_short(spec, CL = "CL", EGFR = "eGFR")

#' Create plot labels
#' Set up any specific labels for the plot 
all_labels <- ys_get_short(spec)
#' We want the CL label to include unit
all_labels$CL <- ys_get_short_unit(spec)$CL
#' Including this inline to make sure all plots get the same labeling; 
#' don't recommend putting information like this in the spec
all_labels$CI_label <- "Median [95% CI]"
all_labels$x_lab <- "Fraction and 95% CI \nRelative to Reference"
plot_labels <- as_labeller(unlist(all_labels))

#' Grab units from spec file.
unit <- ys_get_unit(spec) %>% 
  as_tibble() %>% 
  pivot_longer(everything(), values_to = "unit")

### GENERATING DRAWS FROM PARAMETER UNCERTAINTY DISTRIBUTION ### --------

# Choose one of the two methods for generating samples:
bootstrap <- FALSE  # Enter `TRUE` if using bootstrap output
covmat <- TRUE      # Enter `TRUE` if using covariance matrix
# Note: only ONE option can be TRUE or the code below
# will give an error and no output will be generated.

# Choose number of samples
n_subset <- 500

#### OPTION 1: BOOTSTRAP --------
if(bootstrap && !covmat){
  message("Using option 1: Reading in output from a bootstrap run")
  set.seed(12345)
  #' Load the bootstrap parameter estimates from model run and take a subset
  boot_run <- read_model(file.path(modDir, paste0(runno, "-boot")))
  post <- bootstrap_estimates(boot_run)
  post <- select(post, contains("THETA")) %>% slice_sample(n = n_subset) 
  post <- mutate(post, iter = row_number())
  
  #### OPTION 2: COVARIANCE MATRIX --------
} else if(covmat & !bootstrap) {
  message("Using option 2: Simulating from the covariance matrix using `simpar()`")
  
  #' This is a fixed-effects simulation; only THETA is required; 
  #' we simulate omega and sigma for a more complete example
  bb <- read_model(file.path(modDir, runno)) %>% model_summary() 
  th <- get_theta(bb)
  om <- get_omega(bb)
  sg <- get_sigma(bb)
  covar <- cov_cor(bb)$cov_theta
  
  set.seed(12345)
  post <- simpar( 
    nsim = n_subset,
    theta = th, 
    covar = covar,
    omega = om, 
    odf = 160, # Rule of thumb: set to number of individuals in dataset
    sigma = sg, 
    sdf = 3142 # Rule of thumb: set to number of observations in dataset
  ) %>% as.data.frame()
  names(post) <- gsub("[[:punct:]]", "", names(post))
  names(post) <- gsub("TH", "THETA", names(post))
  post <- select(post, contains("THETA"))
  post <- mutate(post, iter = row_number())
  
  ### GENERATE WARNING if method not correctly specified
} else {
  if(bootstrap && covmat | !bootstrap && !covmat) {
    stop("No output generated - choose either bootstrap or covmat to proceed.")
  }
}

### COVARIATE VALUE SELECTION: -------
#' 
#' There are two options - 
#' 1. Designate percentiles of the covariate distribution to simulate for forest plot.
#' 2. Manually assign values of the covariates to simulate for forest plot.
#' 
#' Use these logical values below to select which option OR delete the code for
#' the undesired method.

#' Choose one of the two methods for selecting covariate values:
opt1 <- FALSE # Set to TRUE for Option 1 (designate percentiles)
opt2 <- TRUE  # Set to TRUE for Option 2 (manual selection of values)
# Note: only ONE option can be TRUE or the code below
# will give an error and no output will be generated.

#' Get observed covariates
data <- nm_join(file.path(modDir, runno))
covs <- distinct(data, ID, EGFR, ALB, AGE, WT) 

#### OPTION 1: AUTOMATIC -----------
#' Automatically select 10th / 90th percentile as covariate values for the simulations.

if(opt1 && !opt2) {
  
  # Helper function:
  # The default is 10th and 90th percentiles.
  # This can be updated when the function is called.
  getPercentile <- function(.x, .p = c(0.1, 0.9)){
    signif(unname(quantile(.x, .p)), digits = 3)
  }
  
  # Create the list of values:
  # The order used here will be retained, with the first covariate appearing at 
  # the top of the plot
  x <- list(
    WT = getPercentile(covs$WT),
    EGFR = getPercentile(covs$EGFR, .p = c(0.1, 0.25, 0.9)), # Option to edit percentiles
    ALB = getPercentile(covs$ALB),
    AGE = getPercentile(covs$AGE)
  )

  #### OPTION 2: MANUAL ---------------
  #' Manually set values for covariates
} else if(opt2 && !opt1) {
  
  # View covariate value distributions:
  covs %>%
    pivot_longer(-ID) %>%
    group_by(name) %>% 
    summarise(as_tibble_row(quantile(value, c(0.1, 0.5, 0.9))))
  
  # Manually assign values:
  # The order used here will be retained, with the first covariate appearing at 
  # the top of the plot
  x <- list(
    WT   = c(53.6, 89.4),
    EGFR = c(47.3, 114),
    ALB  = c(3.24, 5.06),
    AGE  = c(22.1, 45.5)
  )
} else {
  if(opt1 && opt2 | !opt1 && !opt2) {
    stop("No output generated - choose method for covariate value selection.")
  }
}

#' If you want to enter a categorical covariate, use a named vector of values 
#' like this:
#' 
#' x <- list(
#'   WT   = c(53.6, 89.4),
#'   EGFR = c(47.3, 114),
#'   ALB  = c(3.24, 5.06),
#'   AGE  = c(22.1, 45.5),
#'   FORM = c(Capsule = 2, "Oral solution" = 3)
#' )
#' 

### FOREST PLOT EXAMPLES -------

#### EXAMPLE 1: CLEARANCE --------
#' Forest plot for calculated / structural parameters (CL as example)

#' Load the mrgsolve model
#' NOTE: YOU MUST CREATE THE mrgsolve MODEL MANUALLY
#'   and ensure that it is in the directory assigned to simDir (above).
#'   See "NONMEM-to-mrgsolve Primer" document for guidance, if needed.
mod <- mread(here(simDir, glue("{runno}.mod")), capture = "CL")

#' We never need randomness from the model (i.e., ETA, EPS) for this simulation
mod <- zero_re(mod)

# Setting 'end = -1' just allows the model to generate parameter estimates 
# (CL, in this case) for the subjects in the dataset. When parameters are 
# subject-level and not time-dependent they can be calculated at time = 0 without
# simulation for efficiency.
mod <- update(mod, end = -1)
mod

#' Simulate the reference subject 
#' (i.e. a subject with all covariates set to the reference value)
ref <- mrgsim(mod, idata = post) %>% select(iter = ID, ref = CL)

#' @param values the covariate values to simulate
#' @param col name of the covariate in the model (e.g. `WT`)
#' @param dose event object; use `NULL` for no event
simulate_for_forest <- function(values, col, dose = NULL) {
  #' Make an idata set
  idata <- tibble(!!sym(col) := values, LVL = seq_along(values))
  idata <- crossing(post, idata)
  idata <- mutate(idata, ID = row_number())
  
  #' Simulate
  out <-
    mrgsim_df(
      mod, 
      events = dose, 
      idata = idata, 
      carry_out = c(col, "LVL", "iter"), 
      recsort = 3
    ) 
  #' Groom the output
  out <- 
    out %>% 
    mutate(name = col, value = !!sym(col)) %>% 
    select(-!!sym(col)) %>%
    arrange(ID, time, LVL)
  
  #' Process renames
  if(is_named(values)) {
    out <- mutate(
      out, 
      value = factor(value, labels = names(values), levels = values)
    )
  } else {
    out <- mutate(out, value = fct_inorder(as.character(value)))  
  }
  
  out
}

#' Simulate the scenarios (set in Covariate Value Selection section above)
#' We pass in nothing for the `dose` argument since this simulation doesn't 
#' require a dosing regimen
out <- imap(x, simulate_for_forest) %>% list_rbind()

#' Get ready to plot
sims <- left_join(out, ref, by = "iter")
sims <- left_join(sims, unit, by = "name")
sims <- mutate(sims, cov_level = paste(value, unit))
sims <- mutate(sims, across(c(value, cov_level), fct_inorder))
sims <- mutate(sims, name = factor(name, levels = rev(names(x))))

#' We want to plot the clearance relative to reference
sims <- mutate(sims, relcl = CL/ref)

#' Summarize the simulated clearances
sum_data <- pmforest::summarize_data(
  data = sims , 
  value = "relcl",
  group = "name",
  group_level = "cov_level",
  probs = c(0.025, 0.975),
  statistic = "median"
)

#' Plot
clp <- pmforest::plot_forest(
  data = sum_data,
  summary_label = plot_labels,
  text_size = 3.5,
  shape_size = 2.5,
  shapes = "circle",
  vline_intercept = 1,
  x_lab = all_labels$x_lab,
  CI_label = all_labels$CI_label,
  plot_width = 8, 
  x_breaks = c(0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6), 
  x_limit = c(0.4, 1.45),
  annotate_CI = TRUE,
  nrow = 1
) 

clp

mrggsave(clp, stem = "{runno}-pk-forest-cl", width = 6, height = 6)
# Note: The width and height of the plot can be altered here to suit the 
# amount of whitespace around text and on figures.


#### EXAMPLE 2: CMAX,SS --------
#' 
#' Forest plot for derived parameters (Cmax,ss as example)
#' 
#' Setting the output grid to 0.1 hours over the 24 hour dosing interval; 
#' you can make `delta` smaller for more precision on Cmax
mod <- update(mod, delta = 0.1, end = 24)
mod

#' Define an intervention
regimen <- ev(amt = 15, evid = 1, ii = 24, ss = 1)

#' Set up scenarios:
#' This should reflect the order of the covariates and the order of 
#' covariate levels
#' These levels are levels of interest and typically chosen to cover the 
#' distribution of covariates in the data set


#' Simulate the reference subject 
#' (i.e. a subject with all covariates set to the reference value)
#' For this to work, all the defaults in `param(mod)` need to be set to 
#' their reference values
ref <- mod %>% 
  mrgsim(
    events = regimen, 
    idata = post, 
    recover = "iter", 
    recsort = 3
  ) %>%
  group_by(iter) %>%
  summarise(CMAX = max(IPRED), .groups = "drop") %>%
  select(iter, refCMAX = CMAX)

#' Simulate the scenarios (set in Covariate Value Selection section above)
out <- imap(x, simulate_for_forest, dose = regimen) %>% list_rbind()

#' Summarise
sims <- 
  out %>% 
  group_by(name, value, LVL, iter) %>% 
  summarise(CMAX = max(IPRED), .groups = "drop") 

#' Get ready to plot
sims <- left_join(sims, ref, by = "iter")
sims <- left_join(sims, unit, by = "name")
sims <- mutate(sims, cov_level = paste(value, unit))
sims <- mutate(sims, across(c(value, cov_level), fct_inorder))
sims <- mutate(sims, name = factor(name, levels = rev(names(x))))

#' We want to plot the Cmax relative to reference
sims <- mutate(sims, relCMAX = CMAX/refCMAX)

#' Summarize the simulated cmaxs
sum_data_cmax <- pmforest::summarize_data(
  data = sims , 
  value = "relCMAX",
  group = "name",
  group_level = "cov_level",
  probs = c(0.025, 0.975),
  statistic = "median"
)

#' Plot
cmaxp <- pmforest::plot_forest(
  data = sum_data_cmax,
  summary_label = plot_labels,
  text_size = 3.5,
  shape_size = 2.5,
  shapes = "circle",
  vline_intercept = 1,
  x_lab = all_labels$x_lab,
  CI_label = all_labels$CI_label,
  plot_width = 8, 
  x_breaks = c(0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6), 
  x_limit = c(0.4, 1.75),
  annotate_CI = TRUE,
  nrow = 1
) 

cmaxp

mrggsave(cmaxp, stem = "{runno}-pk-forest-cmax", width = 6, height = 6)
# Note: The width and height of the plot can be altered here to suit the 
# amount of whitespace around text and on figures.
