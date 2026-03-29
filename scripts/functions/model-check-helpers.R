#' Check that `.join_col` is set up properly, and that a description is present
#' @param .mod a `bbi_nonmem_model` object
#' @param .join_col a column for joining in the input data to output tables.
#'  See `?nm_join()` for more details.
check_model_setup <- function(.mod, .join_col = "NUM"){
  
  # Not meant to be run on bootstrap or simulation model objects
  checkmate::assert_true(inherits(.mod, "bbi_nonmem_model"))
  
  # Check for model description
  if (is.null(.mod$description)) {
    rlang::inform(
      paste(
        "Your model does not have a description. Are you sure you want to execute",
        "your model before adding one?"
      )
    )
    if(utils::menu(choices = c('Yes','No'))!=1){
      rlang::abort(
        paste(
          "Please use `mod <- add_description(mod, '<description of new model>')`",
          "to describe your basic model structure."
        )
      )
    }
  }
  
  # Check for presence of table records
  has_tables <- mod_has_record(.mod, "table")
  if(isFALSE(has_tables)) rlang::warn("Model has no table records")
  
  # Check that .join_col is in input data and $INPUT record
  good_input <- check_input_data(.mod, .join_col = .join_col)
  
  # Check for .join_col or "ID" in all tables, and for the inclusion of format options
  good_tables <- check_table_records(.mod, .join_col = .join_col)
  
  # Check for upper-case extension for MSF file (dont check that it exists though)
  good_msf <- check_msf_file(.mod, .check_exists = FALSE)
  
  return(has_tables && good_input && good_tables && good_msf)
}

#' Check that `.join_col` is in input data and `$INPUT` record
#' @param .mod a `bbi_nonmem_model` object
#' @param .join_col a column for joining in the input data to output tables.
#'  See `?nm_join()` for more details.
check_input_data <- function(.mod, .join_col = "NUM"){
  # Check that .join_col is in input data
  data_input_names <- get_input_columns(.mod)
  good_input_data <- .join_col %in% data_input_names
  
  # Check that .join_col is also in input record
  record_input_names <- get_input_columns(.mod, from_data = FALSE)
  good_input_record <- .join_col %in% unname(record_input_names)
  
  good_input <- all(c(good_input_data, good_input_record))
  if(isFALSE(good_input)){
    data_path <- fs::path_rel(get_data_path(.mod), getwd())
    msg <- c(
      "Data input issue:",
      paste(
        glue::glue("  Check that .join_col ('{.join_col}') is"),
        glue::glue("present in both the `$INPUT` record and dataset (`{data_path}`)")
      )
    )
    rlang::warn(msg)
  }
  return(invisible(good_input))
}

#' Check that `.join_col` is in table files (`$TABLE` records if model is un-submitted)
#' @param .mod a `bbi_nonmem_model` object
#' @param .join_col a column for joining in the input data to output tables.
#'  See `?nm_join()` for more details.
check_table_records <- function(.mod, .join_col = "NUM"){
  
  if(isFALSE(mod_has_record(.mod, "table"))){
    return(FALSE)
  }
  
  # Model submission status checks
  is_finished <- check_nonmem_finished(.mod)
  
  # Check table records if model hasnt been submitted, otherwise read in tables
  if(isTRUE(is_finished)){
    all_table_cols <- get_table_columns(.mod)
  }else{
    all_table_cols <- get_table_columns(.mod, from_data = FALSE)
  }
  
  # Check for .join_col or "ID" in all tables
  # - We dont specifically check if the table is FIRSTONLY (n_recs = nrows),
  #   as that requires reading in the model summary and fully reading in the
  #   tables, and the goal here is to do a quicker check ahead of time
  # - A `$TABLE` record must contain either the .join_col and/or "ID" to join
  #   properly
  has_join_col_or_id <- purrr::map_lgl(all_table_cols, function(tab_cols){
    any(c(.join_col, "ID") %in% tab_cols)
  })
  good_tables <- all(has_join_col_or_id)
  
  if(isFALSE(good_tables)){
    missing_cols <- names(has_join_col_or_id)[which(has_join_col_or_id == FALSE)]
    missing_cols_txt <- paste(missing_cols, collapse = ", ")
    msg <- c(
      "Table record issue:",
      glue::glue("  `$TABLE` records must include the provided .join_col ('{.join_col}') and/or 'ID' for FIRSTONLY tables."),
      glue::glue("  The following table records do not have either: `{missing_cols_txt}`")
    )
    rlang::warn(msg)
  }
  
  # Check for format in each table
  table_fmts <- get_table_format(.mod)
  good_format <- all(!is.na(table_fmts))
  if(isFALSE(good_format)){
    missing_fmt <- names(table_fmts)[which(is.na(table_fmts))]
    missing_fmt_txt <- paste(missing_fmt, collapse = ", ")
    rlang::warn(
      c(
        "Table record issue:",
        glue::glue("  The following $TABLE record(s) do not have a `FORMAT` option: `{missing_fmt_txt}`"),
        "  This causes problems in datasets with > 99,999 rows because it truncates the row number",
        "  You can add this option via `set_table_format(mod)`"
      )
    )
  }
  
  return(invisible(good_tables && good_format))
}

#' Helper to determine if `nm_join()` can be called on a `bbi_nonmem_model`
#' object. Will check that `.join_col` was set up correctly.
#' @param .mod a `bbi_nonmem_model` object
#' @param .join_col a column for joining in the input data to output tables.
#'  See `?nm_join()` for more details.
can_be_nm_joined <- function(.mod, .join_col = "NUM"){
  
  if(inherits(.mod, "bbi_nonmem_summary")){
    .mod <- bbr::read_model(.mod$absolute_model_path)
  }else{
    checkmate::assert_true(inherits(.mod, "bbi_nonmem_model"))
  }
  
  # Model submission status checks
  is_finished <- check_nonmem_finished(.mod)
  
  # Check for presence of table records
  has_tables <- mod_has_record(.mod, "table")
  
  reasons <- c()
  if(isFALSE(is_finished)){
    reasons <- c(reasons, "Model has not finished executing")
  }
  
  if(isFALSE(has_tables)){
    reasons <- c(reasons, "Model has no table records. Nothing to join to `nm_data()`")
  }
  
  if(isFALSE(is_finished) || isFALSE(has_tables)){
    reasons_txt <- paste0(" - ", reasons, collapse = "\n")
    # dont abort, so that other checks are also run
    rlang::warn(
      c(
        "`nm_join()` cannot be used to join model output and input data",
        "i"="Reasons:",
        reasons_txt
      )
    )
  }
  
  # Check that .join_col is in input data and $INPUT record
  good_input <- check_input_data(.mod, .join_col = .join_col)
  
  # Check for .join_col or "ID" in all tables, and for the inclusion of format options
  good_tables <- check_table_records(.mod, .join_col = .join_col)
  
  # Asses overall join
  good_join <- is_finished && has_tables && good_input && good_tables
  return(good_join)
}

#' Helper for setting the format for all `$TABLE` records
#' 
#' `NONMEM` may not report values to enough significant figures using the default
#' `FORMAT` option. Adding this option ensures that the `.join_col` (typically 
#'  `NUM`, a row number column), is properly captured when saving out table files.
#'   - This is important for datasets with > 99,999 rows, when attempting to call
#'   `nm_join()`.
#' 
#' @param .mod a `bbi_nonmem_model` object
#' @param format a character string denoting the format of `$TABLE` records
set_table_format <- function(.mod, format = "s1PE12.5"){
  mod_path <- bbr::get_model_path(.mod)
  ctl <- nmrec::read_ctl(mod_path)
  table_recs <- nmrec::select_records(ctl, "table")
  
  purrr::walk(table_recs, function(table_rec){
    nmrec::set_record_option(table_rec, "FORMAT", format)
  })
  
  nmrec::write_ctl(ctl, mod_path)
  return(invisible(.mod))
}

### Simulation ###

#' Check that the MSF file has an upper-case extension and exists
#' @param .mod a `bbi_nonmem_model` object
check_msf_file <- function(.mod, .check_exists = TRUE){
  msf_path <- get_msf_path(.mod, .check_exists = FALSE)
  if(is.null(msf_path)){
    msf_file <- paste0(get_model_id(.mod), ".MSF")
    rlang::warn(glue::glue("MSFO option not set. Add `MSFO={msf_file}` to `$EST` record"))
    return(FALSE)
  }else{
    # Check that MSF file has upper-case extension
    msf_ext <- fs::path_ext(msf_path)
    good_msf <- identical(msf_ext, toupper(msf_ext))
    if(isFALSE(good_msf)){
      rlang::warn(
        c(
          "The MSFO option missing upper-case extension",
          paste(
            glue::glue("Try updating `{basename(get_model_path(.mod))}` to use"),
            "an upper case extension and re-submitting before simulating"
          ),
          "e.g., `$EST MSFO=1.msf` --> `$EST MSFO=1.MSF`"
        )
      )
    }
    
    if(isTRUE(.check_exists) && !fs::file_exists(msf_path)){
      good_msf <- FALSE
      msf_path_name <- basename(msf_path)
      rlang::warn(glue::glue("Could not find referenced MSF path ({msf_path_name}) at `{msf_path}`"))
    }
    
    return(good_msf)
  }
}

# nmrec helpers -----------------------------------------------------------


#' Check if a `bbr` model has a given record type
#' @param .mod a `bbi_nonmem_model` object
#' @param type record type. This may be spelled any way that's accepted in a
#'  `NONMEM` control stream.
mod_has_record <- function(.mod, type){
  assert_record_type(.mod, type)
  mod_path <- bbr::get_model_path(.mod)
  ctl <- nmrec::read_ctl(mod_path)
  recs <- nmrec::select_records(ctl, type)
  has_rec <- !rlang::is_empty(recs)
  return(has_rec)
}


#' Pull all records of a given record type from a `bbr` model
#' @param .mod a `bbi_nonmem_model` object
#' @param type record type. This may be spelled any way that's accepted in a
#'  `NONMEM` control stream.
get_records <- function(.mod, type){
  assert_record_type(.mod, type)
  mod_path <- bbr::get_model_path(.mod)
  ctl <- nmrec::read_ctl(mod_path)
  recs <- nmrec::select_records(ctl, type)
  if(rlang::is_empty(recs)){
    return(NULL)
  }else{
    return(recs)
  }
}

#' Retrieve input data column names from the input dataset
#' @param .mod a `bbi_nonmem_model` object
#' @param from_data Logical (T/F). If `TRUE`, the default, get the column names
#'  from the first line of the referenced dataset (input data or table file). If
#'  `FALSE`, parse the control stream and retrieve from the relevant record type
#'  (`$INPUT` or `$TABLE`).
get_input_columns <- function(.mod, from_data = TRUE){
  if(isTRUE(from_data)){
    data_path <- bbr::get_data_path(.mod)
    input_data <- data.table::fread(
      data_path, na.strings = ".", verbose = FALSE, nrows = 1
    )
    input_cols <- names(input_data)
  }else{
    inputs <- get_records(.mod, "input")[[1]]
    inputs$parse()
    
    input_col_opts <- purrr::keep(inputs$values, function(val){
      inherits(val, c("nmrec_option_flag", "nmrec_option_value")) &&
        !inherits(val, "nmrec_option_record_name")
    })
    
    input_col_names <- purrr::map_chr(input_col_opts, function(val) val$name)
    input_cols <- purrr::map_chr(input_col_opts, function(val){
      ifelse(inherits(val, "nmrec_option_flag"), val$name, as.character(val$value))
    }) %>% stats::setNames(input_col_names)
  }
  return(input_cols)
}

#' Retrieve table columns names from the tabled out files
#' @param .mod a `bbi_nonmem_model` object
#' @param from_data Logical (T/F). If `TRUE`, the default, get the column names
#'  from the first line of the referenced dataset (input data or table file). If
#'  `FALSE`, parse the control stream and retrieve from the relevant record type
#'  (`$INPUT` or `$TABLE`).
get_table_columns <- function(.mod, from_data = TRUE){
  # Name tables by filename if present
  tables <- get_records(.mod, "table")
  table_names <- purrr::imap(tables, function(table, .y){
    table_file <- nmrec::get_record_option(table, "file")
    if(!is.null(table_file)) table_file$value else paste("Record", .y)
  }) %>% purrr::list_c()
  
  if(isTRUE(from_data)){
    tab_files <- bbr::nm_table_files(.mod)
    table_cols <- purrr::map(tab_files, function(tab_file){
      tab <- data.table::fread(
        tab_file, na.strings = ".", verbose = FALSE, nrows = 1, skip = 1
      )
      table_cols <- names(tab)
    })
  }else{
    table_cols <- purrr::map(tables, function(table){
      table$parse()
      table_col_opts <- purrr::keep(table$values, function(val){
        inherits(val, "nmrec_option_pos")
      })
      table_cols <- purrr::map(table_col_opts, function(col){
        stringr::str_split(col$format(), " ")[[1]]
      }) %>% purrr::list_c()
    })
  }
  table_cols <- table_cols %>% stats::setNames(table_names)
  return(table_cols)
}


#' Helper for returning any `FORMAT` options in a `$TABLE` record
#' @param .mod a `bbi_nonmem_model` object
get_table_format <- function(.mod){
  tables <- get_records(.mod, "table")
  
  table_fmts <- purrr::imap(tables, function(table, .y){
    table_fmt <- nmrec::get_record_option(table, "format")
    table_file <- nmrec::get_record_option(table, "file")
    table_name <- if(!is.null(table_file)) table_file$value else paste("Record", .y)
    table_fmt <- if(!is.null(table_fmt)) table_fmt$value else NA_character_
    table_fmt %>% stats::setNames(table_name)
  }) %>% purrr::list_c()
  
  return(table_fmts)
}


#' Helper for checking if a specified record type is valid.
#'
#' Checks if the specified record type is valid. Note that this does _not_ check
#' if the record is present in the control stream, just whether `nmrec` recognizes
#' the spelling.
#' 
#' @param .mod a `bbi_nonmem_model` object
#' @param type record type. This may be spelled any way that's accepted in a
#'  `NONMEM` control stream.
assert_record_type <- function(.mod, type){
  checkmate::assert_character(type)
  mod_path <- bbr::get_model_path(.mod)
  ctl <- nmrec::read_ctl(mod_path)
  recs <- tryCatch(
    nmrec::select_records(ctl, type),
    error = function(cond) return(cond)
  )
  
  if(inherits(recs, "error")){
    rlang::abort(
      c(
        "Issue when looking for record type. Failed with the following message:",
        recs$message
      )
    )
  }
  
  return(invisible(TRUE))
}


#' Get path to `MSF` file path
#'
#' Extract `MSF` file path as defined in a `$ESTIMATION` record in a `NONMEM`
#'  control stream file, and construct absolute file path
#'
#' @param .mod a `bbi_model` object
#' @param .check_exists If `TRUE`, the default, will throw an error if the file does not exist
#' @returns absolute file path to `MSF` file, or `NULL` if one doesnt exist.
get_msf_path <- function(.mod, .check_exists = TRUE){
  mod_path <- bbr::get_model_path(.mod)
  ctl <- nmrec::read_ctl(mod_path)
  ests <- nmrec::select_records(ctl, "est")
  
  msf_path <- purrr::map(ests, function(est){
    opt <- nmrec::get_record_option(est, "msf")
    if(!is.null(opt)) opt$value else opt
  }) %>% purrr::list_c()
  
  if(!is.null(msf_path)){
    if(length(msf_path) != 1){
      msf_txt <- paste0(msf_path, collapse = ", ")
      rlang::abort(
        c(glue("Multiple MSF files found: {msf_txt}"))
      )
    }
    msf_path <- file.path(.mod$absolute_model_path, msf_path) %>%
      fs::path_norm() %>% as.character()
  }
  
  if(isTRUE(.check_exists)){
    checkmate::assert_file_exists(msf_path)
  }
  
  return(msf_path)
}