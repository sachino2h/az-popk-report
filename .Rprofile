local({
  
  # Specify R version per project
  R_VERSION <- "4.4"
  # Metworx blueprint version for project
  METWORX_PREFIX <- "25-02"

  correct_r <- grepl(
    paste0("R version ", R_VERSION),
    R.version[["version.string"]], 
    fixed = TRUE
  )
  
  if (!correct_r) {
    # stop(paste0("This project only works with R ", R_VERSION), call. = FALSE)
  }
  
  metworx_version <- Sys.getenv("METWORX_VERSION")
  if (!startsWith(metworx_version, paste0(METWORX_PREFIX, "."))) {
    # stop(
    #  sprintf(
    #    "Workflow is running Metworx %s but this project requires Metworx %s.*",
    #    metworx_version,
    #    METWORX_PREFIX
    #  ),
    #  call. = FALSE
    # )
  }

  options(
    # set some bbr opinionated defaults, these won't impact users who don't use bbr
    'bbr.bbi_exe_path' = file.path(getwd(), "bin", "bbi"),
    
    # do not check that the project library is synced with lockfile on load
    renv.config.synchronized.check = FALSE,
    
    # do not print renv startup messages - we have our own
    renv.config.startup.quiet = TRUE
  )
  
  source("renv/activate.R")
  
  libpath_1 <- .libPaths()[1]
  lib_pkgs <- list.files(libpath_1)
  
  # This is the case where only renv itself is installed
  if (identical(lib_pkgs, "renv")) {
    warning(
      "No packages installed, run 'pkgr install' in the terminal to install.",
      call. = FALSE
    )
  } else {
    # Attempt to set options(repos) from pkgr.yml "Repos" section.
    # This is primarily to ensure that creating an renv.lock file
    # with renv::snapshot() will point to the same repos as pkgr.
    pkgr_list <- tryCatch(
      suppressWarnings(yaml::read_yaml("pkgr.yml")),
      error = identity
    )
    
    if (inherits(pkgr_list, "error")) {
      warning(
        "Extracting repos from pkgr.yml failed: ", conditionMessage(pkgr_list),
        call. = FALSE
      )
    } else {
      # if Repos is empty, repos_to_set will be "NULL"
      repos_to_set <- unlist(pkgr_list$Repos)
      if (!is.null(repos_to_set)) {
        # a value must be set to CRAN or R will complain
        # point to MPN if it's there, else grab the first one (it's arbitrary)
        if (!("CRAN" %in% names(repos_to_set))) {
          repos_to_set[["CRAN"]] <- 
            if ("MPN" %in% names(repos_to_set)) {
              repos_to_set[["MPN"]]
            } else {
              repos_to_set[[1]]
            }
        }
        repos_txt <- paste0(
          "\n - ", names(repos_to_set), ": ", repos_to_set, collapse = ""
        )
        options(repos = repos_to_set)
      } else {
        warning(
          "No repos found in pkgr.yml",
          call. = FALSE
        )
      }
    }
  }
  
  if (interactive()) {
    # project info ------------------------------------------------------------
    message("Project:   ", basename(getwd()))                                                                         
    message("R version: ", R_VERSION)                                                                                 
    message("Library:   ", libpath_1)
    if(exists("repos_txt")){
      message("Repo(s):   ", repos_txt)
    }
  }
})
