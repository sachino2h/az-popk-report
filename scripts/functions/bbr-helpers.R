library(bbr)

# Example of custom helper function
# This one compares final estimates to final gradients
par_est_grd <- function(.mod){
  ptab <- .mod %>% 
    model_summary() %>% 
    param_estimates() %>% 
    transmute(
      name=parameter_names,
      estimate, stderr, shrinkage
    )
  final_grd <- .mod %>% 
    nm_grd() %>% 
    slice(n()) %>% 
    pivot_longer(cols=c(-ITERATION)) %>% 
    transmute(name = name, final_grd=value)
  return(
    full_join(ptab, final_grd, by="name")
  )
}



## updated to return vector of all run numbers needing to be rerun
## intentioned to be piped into submit_models function
out_of_date <- function(.dir){
  config_log(.dir) %>% 
    select(run, model_has_changed, data_has_changed) %>% 
    filter(model_has_changed | data_has_changed) %>% 
    pull(run)
}


## check if a given file exists in the model output directory
## and open it in RStudio if it does. This is useful for files
## like OUTPUT and PRDERR that may not be present.
open_file_if_exists <- function(.mod, .file) {
  .path <- file.path(get_output_dir(.mod), .file) 
  if (fs::file_exists(.path)) {
    file.edit(.path)
  } else {
    print(paste(
      fs::path_rel(.path, here::here()), 
      "does not exist. This may mean the model has finished running, or this file was not produced."
    ))
  }
}

check_for_simulation_output <- function(.mod) {
  .sim <- get_simulation(.mod) ## this will error if no simulation
  if (!check_nonmem_finished(.sim)) {
    rlang::abort(
      c(
        "Simulation is incomplete or has not been run.",
        "i"= paste(
          "If it hasn't been run, see the",
          glue::glue("{cli::style_bold('Re-running existing simulation')}"),
          glue::glue("section of {cli::col_blue('?get_simulation')}")
        )
      )
    )
  }
  return(invisible(TRUE))
}