##' Purpose: 
##' define functions that are called repeatedly while producing diagnostics
##' for model runs

#' Render a model diagnostics template Rmd to HTML file
#' 
#' Example diagnostic templates can be found in `script/diagnostic-templates/`.
#' Users can modify these, or create their own templates. See "Details" section.
#' 
#' @details The only requirement for an `.Rmd` template to work with this
#' function is that it is parameterized and expects a parameter called `model_run`
#' (for the run number) and a parameter called `model_dir` (for the path to the
#' directory containing the model files).
#'
#' To be clear, **users do _not_ need to pass through these `model_run` and
#' `model_dir`** because they will be parsed internally.
#' 
#' @return Invisibly returns the path to the rendered HTML file. This is 
#' intended to make it easy to pipe into something like `browseURL()` for
#' opening in your browser.
#' 
#' @param .mod Either a path to a model output directory, or a `bbi_model`
#'   object (an object returned from [bbr::read_model()]/[bbr::new_model()] or
#'   [bbr::model_summary()])
#' @param .p A named list of parameters to pass through to the `.Rmd`
#'   template.
#' @param template Path to the `.Rmd` template to render. Defaults to
#'   `script/diagnostic-templates/diagnostics-basic.Rmd`.
#' @param dest_dir Destination directory to write the rendered HTML into. If
#'   `NULL`, the default, will render to `bbr::get_output_dir(.mod)`.
model_diagnostics <- function(
    .mod, 
    .p = setNames(list(), character(0)), 
    template = here::here("scripts", "templates", "pk-diagnostics.Rmd"),
    dest_dir = NULL
) {
  if (inherits(.mod, "bbi_model")) {
    .mod <- .mod$absolute_model_path
  }
  checkmate::assert_string(.mod)
  checkmate::assert_directory_exists(.mod)
  
  checkmate::assert_string(template)
  checkmate::assert_file_exists(template)
  
  checkmate::assert_list(.p, names = "named")
  checkmate::assert_string(dest_dir, null.ok = TRUE)
  
  # extract model id and build paths
  if (any(c("model_run", "model_dir", "script") %in% names(.p))) {
    warning(paste(
      "No need to pass `model_run`, `model_dir`, or `script` into model_diagnostics() because they are inferred from function inputs.",
      "Passed values will be ignored."
    ))
  }
  model_run <- basename(.mod)
  .p$model_run <- model_run
  .p$model_dir <- dirname(.mod)
  .p$script <- basename(template)
  
  stem <- tools::file_path_sans_ext(basename(template))
  html_file <- glue::glue("{stem}-{model_run}.html")
  
  # if no destination dir passed, put in model output dir
  if (is.null(dest_dir)) {
    dest_dir <- .mod
  }
  
  # pass through to render
  rmarkdown::render(
    template,
    params = .p,
    output_dir = dest_dir,
    output_file = html_file,
    envir = new.env()
  )
  
  invisible(return(file.path(dest_dir, html_file)))
}

