#' Create YAML Specification File from Specification and Code Lists
#'
#' This function generates a YAML specification file from given data frames containing variable specifications and code mappings.
#'
#' @param description A character string describing the data specification to be included in the YAML file.
#' @param spec_data A data frame containing the variable specifications. Must include columns: `column`, `short`, `type`, `unit`.
#' @param code_list A data frame containing the code mappings. Must include columns: `column`, `decode`, `values`.
#' @param output_file A character string specifying the path to the output YAML file.
#' @param setup_opts A named list of items that will be added to the SETUP__ section of the yspec YAML (e.g., flags, extend_file, lookup_file, etc.)
#'
#' @return This function does not return a value. It writes a YAML file to the specified output path and returns the specification list invisibly.
#'
#' @examples
#' \dontrun{
#' # Sample data frames
#' spec_data <- data.frame(
#'   column = c("Var1", "Var2", "DATE", "TIME"),
#'   short = c("Variable 1 Description", "Variable 2 Description", "Date of event", "Time of event"),
#'   type = c("Num", "Char", "Date", "Time"),
#'   unit = c("mg/L", NA, NA, NA)
#' )
#' 
#' code_list <- data.frame(
#'   column = c("Var1", "Var1", "Var2"),
#'   decode = c(1, 2, NA),
#'   values = c("Value 1", "Value 2", NA)
#' )
#' 
#' # Create YAML specification
#' create_yaml_spec(
#'   description = "ABC123 Population Pharmacokinetic (PK) Model Data Specification",
#'   spec_data = spec_data,
#'   code_list = code_list,
#'   output_file = "specification.yaml"
#' )
#' }
create_yaml_spec <- function(description, spec_data, code_list, output_file, setup_opts = list()) {
  
  # Check for required columns in spec_data
  required_spec_cols <- c("column", "short", "type", "unit")
  if (!all(required_spec_cols %in% names(spec_data))) {
    stop("spec_data must contain the following columns: ", paste(required_spec_cols, collapse = ", "))
  }
  
  # Check for required columns in code_list
  required_code_cols <- c("column", "decode", "values")
  if (!all(required_code_cols %in% names(code_list))) {
    stop("code_list must contain the following columns: ", paste(required_code_cols, collapse = ", "))
  }
  
  # Define type mapping
  type_mapping <- c(
    "num" = "numeric",
    "numeric" = "numeric",
    "char" = "character",
    "character" = "character",
    "date" = "numeric",
    "time" = "numeric"
    # Add more mappings as needed
  )
  
  # Join spec_data and code_list on 'column'
  spec_with_codes <- dplyr::left_join(spec_data, code_list, by = "column")
  
  # Get unique column names
  var_names <- unique(spec_with_codes$column)
  
  # Initialize an empty list to store specifications
  spec_list <- list()
  
  for (var.i in var_names) {
    # Filter the data for the current variable
    var_data.i <- dplyr::filter(spec_with_codes, column == var.i)
    
    # Extract unique values for variable attributes
    short.i <- unique(var_data.i$short)
    type.i <- unique(var_data.i$type)
    unit.i <- unique(var_data.i$unit)
    
    if (length(short.i) > 1) {
      stop("Multiple short values found for variable '", var.i, "': ",
           paste(short.i, collapse = ", "))
    }
    
    if (length(type.i) > 1) {
      stop("Multiple type values found for variable '", var.i, "': ",
           paste(type.i, collapse = ", "))
    }
    
    # Determine the variable type based on 'type' using the mapping
    type_key.i <- tolower(type.i)
    if (type_key.i %in% names(type_mapping)) {
      type_final.i <- type_mapping[type_key.i]
    } else {
      warning("Unrecognized type '", type.i, "' for variable '", var.i, "'. Defaulting to 'numeric'.")
      type_final.i <- "numeric"  # Default to numeric if type is unrecognized
    }
    
    var_spec.i <- list(
      short = short.i,
      type = type_final.i
    )
    
    # Handle units
    non_na_units.i <- unique(unit.i[!is.na(unit.i)])
    if (length(non_na_units.i) > 1) {
      stop("Multiple units found for variable '", var.i, "': ",
           paste(non_na_units.i, collapse = ", "))
    } else if (length(non_na_units.i) == 1) {
      var_spec.i$unit <- non_na_units.i
    }
    
    # Handle decode and values
    valid_codes.i <- dplyr::filter(var_data.i, !is.na(decode) & !is.na(values))
    
    if (nrow(valid_codes.i) > 0) {
      # Convert decode to integer where possible
      converted_decode.i <- lapply(valid_codes.i$decode, function(x) {
        # Check if x is a character string that represents an integer
        if (is.character(x) && grepl("^[0-9]+$", x)) {
          as.integer(x)
        } else if (is.numeric(x) && x %% 1 == 0) {
          as.integer(x)
        } else {
          x
        }
      })
      
      var_spec.i$values <- converted_decode.i
      var_spec.i$decode <- as.list(valid_codes.i$values)
    }
    
    # Add the variable specification to the main list
    spec_list[[var.i]] <- var_spec.i
  }
  
  # Define the SETUP__ section using the input parameter
  setup_section <- list(
    SETUP__ = c(
      list(description = description),
      setup_opts
    )
  )
  
  # Combine the SETUP__ section with the variable specifications
  final_spec <- c(setup_section, spec_list)
  
  # Write the combined specification list to a YAML file
  yaml::write_yaml(final_spec, output_file)
  
  # Inform the user
  message("YAML specification file created at: ", output_file, "\n")
  
  # Return the final specification list invisibly
  invisible(final_spec)
}


clean_column_names <- function(df) {
  names(df) <- names(df) %>%
    gsub(" ", "_", .) %>%         # Replace spaces with underscores
    gsub("[^a-zA-Z0-9_]", "", .)  # Remove non-alphanumeric characters except underscores
  return(df)
}