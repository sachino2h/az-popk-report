
# pk-diagnostics-template.Rmd
 - Main example template for rendering diagnostic plots

*Core parameters:*

  * *cov_flags* named list denoting the flag names specifying both
    continuous and categorical covariates. If a `yspec_path` is provided, you
    can pull covariates from the specification file rather than specifying
    `cont_cov` and `cat_cov`.
    * *cont_cov, cat_cov* vector of continuous and categorical covariates.
      Overrides anything found in the `yspec` specification YAML file.
  * *eta_names* Character vector defining the names of each eta column. 
    Should be in the form of `c('ETA_NAME//ETA_COLUMN')`.
  * *endpoint_name_unit* Character string defining the endpoint name and units
  * *log_dv* Logical (T/F). If `TRUE`, indicates `DV`, `PRED`, and `IPRED`
    columns are reported as `Log(x)`. The exponent of those columns will be used
    if this argument is set to `TRUE`.

 
# id-dv-pred-plots.Rmd
 - Template for individual PRED plots (one plot per ID)

