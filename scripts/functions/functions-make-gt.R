
#' Turn a `pmtables` `stobject` into a `gt` table
#' @param st_object An `st_object`. For example, as created with
#'  `pmtables::st_new()` (main method when using `pmtables` exclusively) or
#'  helpers like `pmparams::make_pmtable()`.
#' 
#' @details
#' Notes about limitations and pmtables artifacts this function does not handle:
#'  - Column alignment specified via `st_align`, `st_center`, etc. is handled,
#'    though any column width specification is ignored.
#'  - LaTeX superscript & subscript conversion is limited to digits.
#' 
st_as_gt <- function(st_object){
  
  if(!inherits(st_object, "stobject")){
    cli::cli_abort("Must be an {.var stobject} object. See {.func ?pmtables::st_new()}")
  }
  
  # Gather pmtables artifacts
  stable_data <- pmtables::get_stable_data(pmtables::stable(st_object, inspect = TRUE))
  
  # Store raw data for utilizing LaTeX for bold, italics, and hline creations
  data_raw <- st_object$data
  
  # Data used for gt table
  #  - Substitute LaTeX for Unicode where applicable, and cleanup remaining LaTeX
  data_init <- replace_latex(data_raw)
  
  # Create initial gt object
  gt_table <- gt::gt(data_init)
  
  # Adjust cell padding, font size, and horizontal borders for HTML preview
  # - these options help format the HTML preview of the gt table to look closer
  #   to what can be expected when exported to MS Word and/or the pmtables output
  # - None of these options have any effect when exported to Word
  # - Turns off horizontal borders (which pmtables doesn't have)
  # - Decreases row padding and font size to help the table fit in the viewer
  #   better (also matches pmtables PDF and MS Word output more closely)
  gt_table <- gt_table %>% gt::tab_options(table_body.hlines.width = gt::px(0))
  gt_table <- gt_table %>% gt::tab_options(data_row.padding = gt::px(4))
  gt_table <- gt_table %>% gt::tab_options(table.font.size = gt::px(10))
  
  # Create span(s)
  gt_table <- st_as_gt_spans(gt_table, span_data = stable_data$span_data)
  
  # Create panels
  gt_table <- st_as_gt_panels(gt_table, panel_data = stable_data$panel)
  
  # Rename columns (must happen after span is done. Unsure about panel interaction)
  gt_table <- st_as_gt_cols_rename(gt_table, cols_tex = stable_data$cols_tex)
  
  # Align cells
  gt_table <- st_as_gt_align(gt_table, align = stable_data$align)
  
  # Add notes
  gt_table <- st_as_gt_footnotes(gt_table, stable_data$notes)
  
  # Apply bold & italics formatting and horizontal lines using existing latex
  gt_table <- apply_latex_styling(gt_table, data_raw)
  
  # Attach stable data for downstream use
  attr(gt_table, "stable_data") <- stable_data
  
  return(gt_table)
}


# Apply gt spans and rename columns that include the span label within them
# given information in `stobject$span_data`
st_as_gt_spans <- function(gt_table, span_data){
  if(!is.null(span_data)){
    spans <- span_data$span
    # span levels
    for(span.i in spans){
      span_vars <- unique(span.i$title)
      span_vars <- span_vars[span_vars != ""]
      # span(s) per span level
      for (var in span_vars) {
        # spanned columns per span
        span_cols <- span.i %>% dplyr::filter(title == var) %>%
          dplyr::pull(col)
        
        # Apply span and column renaming
        gt_table <- gt_table %>% gt::tab_spanner(label = var, columns = span_cols)
      }
    }
  }
  return(gt_table)
}

# Apply panels using information in `stobject$panel`
# Note: This only supports a single panel column currently
st_as_gt_panels <- function(gt_table, panel_data) {
  add_panel <- length(panel_data$col) >= 1 && all(panel_data$col != "")
  if(isTRUE(add_panel)){
    panel_col <- names(panel_data$col)[1]
    panel_prefix <- panel_data$prefix 
    skip_pattern <- panel_data$prefix_skip
    gt_data <- gt_table$`_data`
    
    # Unique values for panel groups
    panel_groups <- gt_data %>% dplyr::pull(.data[[panel_col]]) %>% unique()
    
    # Hide panel column
    gt_table <- gt_table %>% gt::cols_hide(columns = panel_col)
    
    # Format panel labels with prefix
    panel_labels <- purrr::map_chr(panel_groups, function(panel){
      if(!is.null(skip_pattern) && grepl(skip_pattern, panel)){
        panel
      }else{
        paste(panel_prefix, panel)
      }
    })
    
    # Loop over each panel group and apply the panel
    # - must be done in reverse order
    panel_groups_ordered <- rev(panel_groups)
    panel_labels_ordered <- rev(panel_labels)
    
    for(i in seq_along(panel_groups_ordered)){
      panel <- panel_groups_ordered[i]
      panel_label <- panel_labels_ordered[i]
      
      gt_table <- gt_table %>%
        gt::tab_row_group(
          label = panel_label,
          rows = gt_data[[panel_col]] == panel
        ) %>%
        # Remove default row group borders
        gt::tab_options(
          row_group.border.top.width = gt::px(0),
          row_group.border.bottom.width = gt::px(0)
        )
    }
    
    # Additional styling if specified in panel_data
    if(isTRUE(panel_data$bold)){
      gt_table <- gt_table %>%
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_row_groups(groups = panel_labels)
        )
    }
    
    if(isTRUE(panel_data$hline)){
      gt_table <- gt_table %>%
        gt::tab_options(
          row_group.border.bottom.width = gt::px(2),
          row_group.border.bottom.color = "white",
          row_group.border.top.width = gt::px(1),
          row_group.border.top.color = "black",
        )
    }
  }
  
  return(gt_table)
}


# Rename columns using latex defined in `stable_data$cols_tex`
st_as_gt_cols_rename <- function(gt_table, cols_tex){
  # Parse `cols_tex` to create a mapping of columns and their multi-line labels
  # Remove LaTeX-specific code (e.g., "\\\\" and "[-0.52em]")
  cols_tex_fmt <- gsub("\\\\|\\[.*?\\]", "", cols_tex)
  
  # Split the `cols_tex` into lines and fields
  cols_lines_list <- purrr::map(strsplit(cols_tex_fmt, "&"), trimws)
  
  header_data <- gt_table$`_boxhead`
  current_cols <- unlist(header_data$column_label[header_data$type != "hidden"])
  
  # Create label mappings
  label_map <- list()
  for(i in seq_along(current_cols)){
    formatted_lines <- purrr::imap_chr(cols_lines_list, function(line, row){
      if(grepl("n\\s*=", line[i]) && row > 1){
        # Add parentheses when the sub-header contains "n = " or "n="
        paste0("(", line[i], ")")
      }else{
        line[i]
      }
    })
    
    formatted_lines <- formatted_lines[formatted_lines != ""]
    
    # Combine lines and convert to markdown
    # - <br> separator is only relevant for Rstudio viewing, but has no effect
    #   once exported to Word (auto removed).
    # - This may eventually be supported by gt when exported to Word, but is
    #   a known limitation.
    newline_sep <- " <br>" # Add space before line break for exporting to Word
    multi_line_label <- paste(formatted_lines, collapse = newline_sep)
    # Cant have the column name be equal to HTML _alone_ for word rendering, 
    # though it's ok if it's embedded within the string (filtered out)
    if(multi_line_label == newline_sep) multi_line_label <- ""
    label_map[[current_cols[i]]] <- gt_md_maybe(multi_line_label)
  }
  
  # Apply column renaming and multi-line labels to `gt_table`
  gt_table <- do.call(gt::cols_label, c(list(gt_table), label_map))
  
  return(gt_table)
}

gt_md_maybe <- function(x) {
  # Labeling an empty string as markdown causes gtsave to error.
  if (nzchar(x)) {
    gt::md(x)
  } else {
    x
  }
}

# Add footnotes
# To remove footnotes: gt::rm_footnotes(gt_table, footnotes = everything())
st_as_gt_footnotes <- function(gt_table, notes){
  if(!is.null(notes)){
    notes_fmt <- replace_latex(notes)
    for(note.i in notes_fmt){
      gt_table <- gt_table %>% gt::tab_footnote(footnote = note.i)
    }
    # Remove line after notes
    gt_table <- gt_table %>% gt::tab_options(
      table.border.bottom.width = gt::px(0)
    )
  }
  return(gt_table)
}


# Apply alignment options to all cells
# - Note: panels are currently always left aligned if present
st_as_gt_align <- function(gt_table, align) {
  
  # Helper function for parsing pmtables alignment option
  convert_pmtable_alignment <- function(pm_align){
    gt_align <- dplyr::case_when(
      pm_align == "l" ~ "left",
      pm_align == "r" ~ "right",
      pm_align == "c" ~ "center",
      # Check for LaTeX alignment keywords
      # - Note: raggedright corresponds to left alignment (same with raggedleft)
      stringr::str_detect(pm_align, "raggedright") ~ "left",
      stringr::str_detect(pm_align, "raggedleft") ~ "right",
      stringr::str_detect(pm_align, "centering") ~ "center",
      TRUE ~  NA_character_
    )
    
    if(is.na(gt_align)){
      cli::cli_warn(
        c(
          "gt cannot use the specified pmtables alignment ({.val {pm_align}})",
          "i" = "Setting to left alignment"
        )
      )
      gt_align <- "left"
    }
    return(gt_align)
  }
  
  # Apply default alignment to all body and header cells (not panels)
  default_align <- convert_pmtable_alignment(align$default)
  gt_table <- gt_table %>%
    gt::tab_style(
      style = gt::cell_text(align = default_align),
      locations = list(
        gt::cells_body(),
        gt::cells_column_labels()
      )
    )
  
  # If certain columns have specific alignment options
  if(!rlang::is_empty(align$update)){
    for(i in seq_along(align$update)){
      align_col <- names(align$update)[i]
      pm_align <- convert_pmtable_alignment(align$update[[i]])
      
      # Apply alignment to specific column
      gt_table <- gt_table %>%
        gt::tab_style(
          style = gt::cell_text(align = pm_align),
          locations = list(
            gt::cells_body(columns = align_col),
            gt::cells_column_labels(columns = align_col)
          )
        )
    }
  }
  
  # Check for "outer" alignment specification
  # - Assumptions: this applies to left-most and right-most columns shown
  # - i.e. need to filter out hidden (paneled) columns first
  col_data <- gt_table[["_boxhead"]]
  cols_visible <- col_data[["var"]][col_data$type != "hidden"]
  if (length(cols_visible) && !identical(align[["outer"]], "none")) {
    col_left <- cols_visible[1]
    col_right <- cols_visible[length(cols_visible)]

    if (identical(align[["outer"]], "lr")) {
      # ^ Special case: .outer accepts an additional "lr" value (align text of
      # left-most column to left and text of right-most column to right).
      aligns_outer <- c("left", "right")
      # Edge case: if there's just one column, the right alignment will take
      # precedence (due to order of processing).
      cols_outer <- c(col_left, col_right)
    } else {
      aligns_outer <- convert_pmtable_alignment(align[["outer"]])
      if (identical(aligns_outer, "left")) {
        cols_outer <- col_left
      } else if (identical(aligns_outer, "right")) {
        cols_outer <- col_right
      } else {
        # This should be unreachable. The only other case that
        # convert_pmtable_alignment handles is "center", but
        # pmtables::cols_align wouldn't accept center alignment for .outer.
        stop(
          "bug: convert_pmtable_alignment returned unexpected type: ",
          deparse1(aligns_outer)
        )
      }
    }

    for (i in seq_along(cols_outer)) {
      gt_table <- gt_table %>%
        gt::tab_style(
          style = gt::cell_text(align = aligns_outer[i]),
          locations = list(
            gt::cells_body(columns = cols_outer[i]),
            gt::cells_column_labels(columns = cols_outer[i])
          )
        )
    }
  }
  
  return(gt_table)
}


# Separate styling functions ----------------------------------------------


# Add vertical and horizontal borders to all cells
# - Note: some spanner borders do not render once exported to word, though
# I havent seen any other methods for doing this
apply_all_borders <- function(gt_table, footnotes = FALSE){
  gt_table <- gt_table %>% 
    gt::tab_style(
      style = gt::cell_borders(
        sides = "all",
        weight = gt::px(1)
      ),
      locations = list(
        # Borders for all body cells
        gt::cells_body(),
        # Borders for row groups (panels)
        gt::cells_row_groups(),
        # Borders for all column headers
        gt::cells_column_labels(),
        # Borders for spanned column headers
        gt::cells_column_spanners()
      )
    ) %>%
    # Borders for row groups (panels) for Word
    gt::tab_options(
      row_group.border.top.width = gt::px(1),
      row_group.border.bottom.width = gt::px(1),
      row_group.border.left.width = gt::px(1),
      row_group.border.right.width = gt::px(1),
      row_group.border.top.color = "black",
      row_group.border.bottom.color = "black",
      row_group.border.left.color = "black",
      row_group.border.right.color = "black"
    ) %>%
    # Borders for column headers (including span) for Word
    gt::tab_options(
      column_labels.border.top.width = gt::px(1),
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.lr.width = gt::px(1),
      column_labels.border.top.color = "black",
      column_labels.border.bottom.color = "black",
      column_labels.border.lr.color = "black"
    )
  
  if(isTRUE(footnotes)){
    gt_table <- gt_table %>% 
      gt::tab_style(
        style = gt::cell_borders(
          sides = c("all"),
          weight = gt::px(1)
        ),
        locations = gt::cells_footnotes()
      )
  }
  
  return(gt_table)
}


# Saving ------------------------------------------------------------------

#' Saves the gt_table to the location defined in the st_object
#' @param gt_table `gt` table returned from `st_as_gt`
#' @param detach Logical (T/F). Whether to detach the footnotes as text. If 
#'  `NULL` (the default), this argument is set based on whether
#'  [pmtables::st_notes_detach()] was used. See details section in 
#'  `save_with_detached_notes()` for more information on use cases.
#'  
#' @details
#' If `detach = NULL`, other arguments passed to [pmtables::st_notes_detach()]
#' are not respected (i.e. formatting options). 
#' 
save_gt_docx <- function(gt_table, detach = NULL){
  stable_data <- attributes(gt_table)$stable_data
  
  # Ensure this function is only being used when paired with st_as_gt
  if(is.null(stable_data)){
    cli::cli_abort(
      c(
        "{.var stable_data} attribute not attached to gt table.",
        "Did you create this with {.func st_as_gt}?"
      )
    )
  }
  
  stable_file <- stable_data$stable_file
  if(!is.null(stable_file)){
    file_name <- basename(stable_file)
    if(fs::path_ext(file_name) != "docx"){
      cli::cli_abort(
        c(
          "This function is only for saving a gt table to {.val .docx} format",
          "i" = "See {.func ?gt::gtsave} for saving gt tables to other formats"
        )
      )
    }
    
    # Apply all borders for word format for neat and consistent formatting
    gt_table <- apply_all_borders(gt_table)
    
    # Handle detached footnotes
    # If NULL, determine based on pmtables (if st_notes_detach() was used)
    if(is.null(detach)) detach <- !is.null(stable_data$mini_notes)
    
    # Save out to docx
    if(isTRUE(detach)){
      save_with_detached_notes(gt_table)
    }else{
      gt::gtsave(gt_table, filename = file_name, path = dirname(stable_file))
    }
    
    return(invisible(stable_file))
  }else{
    cli::cli_abort(
      c(
        "No file path attached to {.var gt_table}, which is required for saving the file",
        "i" = "See {.func ?pmtables::st_files()} for attaching a file name to an st object"
      )
    )
  }
}


#' Detach the footnotes using officer
#' 
#' Saves the table out without footnotes, reads it back in and attaches the notes,
#' and saves it back out. This is for use with MS Word only.
#' 
#' @details
#' By default, footnotes attached to `gt` tables are part of the table. Tables
#' that span multiple pages repeat the column headers on the next page. This is
#' nice for additional rows, but is not valuable when footnotes are the sole
#' reason for repeated headers. 
#' 
#' This function serves two purposes:
#'  - avoids repeated headers when footnotes cause the table to span multiple pages
#'  - footnotes are attached as paragraphs rather than being a part of the table
#'    (allowing users to move them around and format them more easily)
#' 
save_with_detached_notes <- function(gt_table){
  if(!requireNamespace("officer", quietly = TRUE)){
    cli::cli_abort(
      "The {.pkg officer} package is required to detach the footnotes"
    )
  }
  stable_data <- attributes(gt_table)$stable_data
  stable_file <- stable_data$stable_file
  
  # Gather formatted footnotes from gt_table
  notes <- unlist(gt_table$`_footnotes`$footnotes)
  
  # Remove footnotes from existing table and save out
  gt_table_m <- gt::rm_footnotes(gt_table, footnotes = everything())
  gt::gtsave(gt_table_m, filename = basename(stable_file), path = dirname(stable_file))
  
  # Read in table via officer
  doc_data <- officer::read_docx(stable_file)
  # Paragraph formatting properties
  ft_par_p <- officer::fp_par(text.align = "left")
  # Text formatting properties
  ft_par_t <- officer::fp_text_lite(font.size = 10)
  
  # Add the footnotes as text
  for(note in notes){
    fmt_note <- officer::fpar(note, fp_p = ft_par_p, fp_t = ft_par_t)
    doc_data <- officer::body_add_fpar(doc_data, value = fmt_note)
  }
  
  # Save the file back out
  print(doc_data, target = stable_file)
  
  return(invisible(stable_file))
}

# LaTeX replacement -------------------------------------------------------

replace_latex <- function(x) {
  replace_latex_math(x) %>%
    replace_latex_styling() %>%
    remove_backslashes()
}

replace_latex_math <- function(x) {
  purrr::modify_if(
    x, is.character,
    function(s) {
      # Rather than parsing within a string to see whether a command is in math
      # mode, take the shortcut of considering the entire string as math mode if
      # it was an even number of un-escaped dollar signs.
      math_re <- "(?<!\\\\)\\$"
      n_delim <- stringr::str_count(s, math_re)
      is_math <- n_delim != 0 & n_delim %% 2 == 0
      if (any(is_math)) {
        s[is_math] <- replace_latex_math_commands(s[is_math]) %>%
          replace_latex_superscripts() %>%
          replace_latex_subscripts() %>%
          stringr::str_remove_all(math_re)
      }

      return(s)
    }
  )
}

replace_latex_superscripts <- function(s) {
  fn <- function(x) {
    stringr::str_replace_all(
      x,
      stringr::fixed(c(
        "^" = "",
        "{" = "",
        "}" = "",
        "0" = "\u2070",
        "1" = "\u00B9",
        "2" = "\u00B2",
        "3" = "\u00B3",
        "4" = "\u2074",
        "5" = "\u2075",
        "6" = "\u2076",
        "7" = "\u2077",
        "8" = "\u2078",
        "9" = "\u2079"
      ))
    )
  }

  stringr::str_replace_all(s, "\\^\\{.*?(?<!\\\\)\\}", fn) %>%
    stringr::str_replace_all("\\^.?", fn)
}

replace_latex_subscripts <- function(s) {
  fn <- function(x) {
    stringr::str_replace_all(
      x,
      stringr::fixed(c(
        "_" = "",
        "{" = "",
        "}" = "",
        "0" = "\u2080",
        "1" = "\u2081",
        "2" = "\u2082",
        "3" = "\u2083",
        "4" = "\u2084",
        "5" = "\u2085",
        "6" = "\u2086",
        "7" = "\u2087",
        "8" = "\u2088",
        "9" = "\u2089"
      ))
    )
  }

  stringr::str_replace_all(s, "_\\{.*?(?<!\\\\)\\}", fn) %>%
    stringr::str_replace_all("(?<!\\\\)_.?", fn)
}

replace_latex_math_commands <- function(s) {
  subs <- c(
    # Lowercase Greek letters
    "alpha" = "\u03B1",
    "beta" = "\u03B2",
    "gamma" = "\u03B3",
    "delta" = "\u03B4",
    "epsilon" = "\u03B5",
    "zeta" = "\u03B6",
    "eta" = "\u03B7",
    "theta" = "\u03B8",
    "iota" = "\u03B9",
    "kappa" = "\u03BA",
    "lambda" = "\u03BB",
    "mu" = "\u03BC",
    "nu" = "\u03BD",
    "xi" = "\u03BE",
    "omicron" = "\u03BF",
    "pi" = "\u03C0",
    "rho" = "\u03C1",
    "sigma" = "\u03C3",
    "tau" = "\u03C4",
    "upsilon" = "\u03C5",
    "phi" = "\u03C6",
    "chi" = "\u03C7",
    "psi" = "\u03C8",
    "omega" = "\u03C9",
    # Uppercase Greek letters
    "Alpha" = "\u0391",
    "Beta" = "\u0392",
    "Gamma" = "\u0393",
    "Delta" = "\u0394",
    "Epsilon" = "\u0395",
    "Zeta" = "\u0396",
    "Eta" = "\u0397",
    "Theta" = "\u0398",
    "Iota" = "\u0399",
    "Kappa" = "\u039A",
    "Lambda" = "\u039B",
    "Mu" = "\u039C",
    "Nu" = "\u039D",
    "Xi" = "\u039E",
    "Omicron" = "\u039F",
    "Pi" = "\u03A0",
    "Rho" = "\u03A1",
    "Sigma" = "\u03A3",
    "Tau" = "\u03A4",
    "Upsilon" = "\u03A5",
    "Phi" = "\u03A6",
    "Chi" = "\u03A7",
    "Psi" = "\u03A8",
    "Omega" = "\u03A9",
    # Common math operators
    "cdot" = "\u22C5",
    "div" = "\u00F7",
    "exp" = "exp",
    "infty" = "\u221E",
    "pm" = "\u00B1",
    "times" = "\u00D7"
  )

  names(subs) <- paste0("(?<!\\\\)\\\\", names(subs), "(?=[^A-Za-z]|$)")
  return(stringr::str_replace_all(s, subs))
}

latex_styling_re <- c(
  bf = "\\{\\\\bf (.*?)(?<!\\\\)\\}",
  hline = "(?<!\\\\)\\\\hline(?=[^A-Za-z]|$)",
  it = "\\{\\\\it (.*?)(?<!\\\\)\\}",
  textbf = "(?<!\\\\)\\\\textbf\\{(.*?)(?<!\\\\)\\}",
  textit = "(?<!\\\\)\\\\textit\\{(.*?)(?<!\\\\)\\}"
)

# Function to apply bold & italics styling to specific cells, and horizontal
# lines to full rows
# - uses `gt` styling functions rather than unicode conversion
apply_latex_styling <- function(gt_table, data_raw) {
  bold_cells <- find_cells(
    data_raw,
    paste0(latex_styling_re[["bf"]], "|", latex_styling_re[["textbf"]])
  )
  if (length(bold_cells[["row"]])) {
    gt_table <- gt::tab_style(
      gt_table,
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(
        rows = bold_cells[["row"]],
        columns = bold_cells[["col"]]
      )
    )
  }

  italic_cells <- find_cells(
    data_raw,
    paste0(latex_styling_re[["it"]], "|", latex_styling_re[["textit"]])
  )
  if (length(italic_cells[["row"]])) {
    gt_table <- gt::tab_style(
      gt_table,
      style = gt::cell_text(style = "italic"),
      locations = gt::cells_body(
        rows = italic_cells[["row"]],
        columns = italic_cells[["col"]]
      )
    )
  }
  
  # Apply horizontal line styling to _full rows_
  hline_cells <- find_cells(data_raw, latex_styling_re[["hline"]])
  hline_rows <- unique(hline_cells[["row"]])
  if (length(hline_rows) > 0) {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = gt::cell_borders(
          sides = "top",
          color = "#A9A9A9",
          weight = gt::px(2)
        ),
        locations = gt::cells_body(rows = hline_rows)
      )
  }
  
  return(gt_table)
}

# Return row and column indices of string cells in `data` that match `pattern`.
find_cells <- function(data, pattern) {
  res <- purrr::modify_if(
    data, is.character,
    function(s) stringr::str_detect(s, pattern),
    .else = function(...) FALSE
  )
  idxs <- which(as.matrix(res), arr.ind = TRUE)
  return(list(row = unname(idxs[, "row"]), col = unname(idxs[, "col"])))
}

replace_latex_styling <- function(x) {
  purrr::modify_if(
    x, is.character,
    function(s) {
      stringr::str_remove_all(s, latex_styling_re[["hline"]]) %>%
        stringr::str_replace_all(latex_styling_re[["bf"]], "\\1") %>%
        stringr::str_replace_all(latex_styling_re[["it"]], "\\1") %>%
        stringr::str_replace_all(latex_styling_re[["textbf"]], "\\1") %>%
        stringr::str_replace_all(latex_styling_re[["textit"]], "\\1") %>%
        trimws()
    }
  )
}

remove_backslashes <- function(x) {
  purrr::modify_if(
    x, is.character,
    function(s) stringr::str_remove_all(s, stringr::fixed("\\"))
  )
}
