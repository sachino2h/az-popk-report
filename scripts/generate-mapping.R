# =============================================================================
# generate-mapping.R
# 1. Read placeholders from template
# 2. Read table and figure files from config paths
# 3. Create mapping.yaml
# 4. Create mapping.html
# =============================================================================

`%||%` <- function(a, b) if (!is.null(a)) a else b

normalize_mapping_key <- function(value) {
  value <- as.character(value %||% "")

  if (grepl(":", value, fixed = TRUE)) {
    sub("^[^:]*:", "", value)
  } else {
    value
  }
}

to_relative_path <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path) || !nzchar(path)) {
    return("")
  }

  project_path <- normalizePath(here::here(), winslash = "/", mustWork = FALSE)
  file_path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  sub(paste0("^", project_path, "/?"), "", file_path)
}

yaml_text <- function(value) {
  value <- as.character(value %||% "")
  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

yaml_field_lines <- function(key, value, indent = 0) {
  value <- as.character(value %||% "")
  value <- gsub("\r\n?", "\n", value)
  prefix <- paste0(strrep(" ", indent), yaml_text(key), ":")

  if (!grepl("\n", value, fixed = TRUE)) {
    return(paste0(prefix, " ", yaml_text(value)))
  }

  line_values <- strsplit(value, "\n", fixed = TRUE)[[1]]
  if (grepl("\n$", value)) {
    line_values <- c(line_values, "")
  }

  chomp <- if (grepl("\n$", value)) "|" else "|-"
  c(
    paste0(prefix, " ", chomp),
    paste0(strrep(" ", indent + 2), line_values)
  )
}

read_template_placeholders <- function(template_path) {
  doc <- officer::read_docx(template_path)
  doc_data <- officer::docx_summary(doc)
  full_text <- paste(doc_data$text, collapse = " ")

  parse_placeholder_text <- function(matches, wrapper_pattern) {
    values <- stringr::str_replace_all(matches, wrapper_pattern, "")
    unique(values[nzchar(values)])
  }

  parse_block_placeholder <- function(raw_value) {
    raw_value <- raw_value %||% ""

    prefix_match <- stringr::str_match(
      raw_value,
      "^([[:alpha:]][[:alnum:]_-]*):(.*)$"
    )

    if (nrow(prefix_match) == 1) {
      prefix <- tolower(prefix_match[, 2])
      label <- prefix_match[, 3]

      if (!is.na(prefix) && !is.na(label) && prefix == "text" && nzchar(label)) {
        return(list(target = "inline", name = label, type = ""))
      }

      if (!is.na(prefix) && !is.na(label) && prefix %in% c("table", "figure", "image") && nzchar(label)) {
        normalized_type <- if (prefix == "figure") "image" else prefix
        return(list(target = "block", name = label, type = normalized_type))
      }
    }

    list(target = "block", name = normalize_mapping_key(raw_value), type = "")
  }

  inline_matches <- stringr::str_extract_all(full_text, "\\{\\{(.*?)\\}\\}")[[1]]
  inline_keys <- vapply(
    parse_placeholder_text(inline_matches, "\\{\\{|\\}\\}"),
    normalize_mapping_key,
    character(1)
  )

  block_matches <- stringr::str_extract_all(full_text, "<<(.*?)>>")[[1]]
  raw_block_values <- parse_placeholder_text(block_matches, "<<|>>")
  parsed_block_placeholders <- lapply(raw_block_values, parse_block_placeholder)

  prefixed_inline_keys <- vapply(
    Filter(function(x) identical(x$target, "inline"), parsed_block_placeholders),
    `[[`,
    character(1),
    "name"
  )

  block_specs <- Filter(function(x) identical(x$target, "block"), parsed_block_placeholders)
  names(block_specs) <- vapply(block_specs, `[[`, character(1), "name")

  list(
    inline_keys = unique(c(inline_keys, prefixed_inline_keys)),
    block_specs = block_specs
  )
}

read_asset_files <- function(dirs, kind) {
  assets <- list()

  for (dir_path in dirs %||% character()) {
    if (!dir.exists(dir_path)) {
      next
    }

    files <- list.files(
      path = dir_path,
      recursive = TRUE,
      full.names = TRUE,
      no.. = TRUE
    )

    files <- sort(files[file.exists(files)])
    files <- files[basename(files) != ".DS_Store"]

    if (length(files) == 0) {
      next
    }

    for (file_path in files) {
      extension <- tolower(tools::file_ext(file_path))

      # Figures list should only include image assets for mapping selection.
      # Explicitly exclude PDF from available figure assets.
      if (identical(kind, "figure") && identical(extension, "pdf")) {
        next
      }

      assets[[length(assets) + 1]] <- list(
        path = to_relative_path(file_path),
        name = basename(file_path),
        kind = kind,
        dir = to_relative_path(dir_path),
        extension = extension
      )
    }
  }

  assets
}

collect_used_asset_names <- function(mapping_yaml_path) {
  if (is.null(mapping_yaml_path) || !nzchar(mapping_yaml_path) || !file.exists(mapping_yaml_path)) {
    return(character(0))
  }

  existing <- tryCatch(
    yaml::read_yaml(mapping_yaml_path),
    error = function(e) NULL
  )
  if (is.null(existing) || is.null(existing$blocks) || !is.list(existing$blocks)) {
    return(character(0))
  }

  used <- character(0)
  for (block_name in names(existing$blocks)) {
    block <- existing$blocks[[block_name]]
    files <- block$files
    if (is.null(files) || length(files) == 0) next
    vals <- unlist(files, use.names = FALSE)
    vals <- as.character(vals)
    vals <- vals[nzchar(vals)]
    if (length(vals) > 0) {
      used <- c(used, basename(vals))
    }
  }

  unique(used)
}

new_inline_mapping <- function(inline_keys) {
  inline <- list()

  for (key in inline_keys) {
    inline[[key]] <- list(
      type = "text",
      status = "new",
      value = ""
    )
  }

  inline
}

new_block_mapping <- function(block_specs, default_status) {
  blocks <- list()

  for (block_spec in block_specs %||% list()) {
    block_name <- block_spec$name %||% ""
    if (!nzchar(block_name)) {
      next
    }

    blocks[[block_name]] <- list(
      type = block_spec$type %||% "",
      title = "",
      footnote = "",
      status = default_status,
      files = list()
    )
  }

  blocks
}

normalize_mapping_config <- function(config) {
  config$paths <- config$paths %||% list()
  config$mapping <- config$mapping %||% list()

  config$paths$template_in <- config$paths$template_in %||%
    here::here("PopPK_Report_merge_TFL_filled_draft.docx")
  config$paths$mapping_yaml <- config$paths$mapping_yaml %||%
    here::here("output", "mapping.yaml")
  config$paths$mapping_html <- config$paths$mapping_html %||%
    file.path(dirname(config$paths$mapping_yaml), "mapping.html")
  config$paths$source_table_dirs <- config$paths$source_table_dirs %||% character()
  config$paths$source_figure_dirs <- config$paths$source_figure_dirs %||% character()

  config$mapping$default_block_status <- config$mapping$default_block_status %||% "new"
  config$mapping$html_template <- config$mapping$html_template %||%
    here::here("scripts", "templates", "mapping-editor-template.html")

  config
}

build_mapping <- function(config) {
  config <- normalize_mapping_config(config)
  placeholders <- read_template_placeholders(config$paths$template_in)
  table_assets <- read_asset_files(config$paths$source_table_dirs, "table")
  figure_assets <- read_asset_files(config$paths$source_figure_dirs, "figure")
  used_asset_names <- collect_used_asset_names(config$paths$mapping_yaml)

  if (length(used_asset_names) > 0) {
    table_assets <- Filter(function(x) !(x$name %in% used_asset_names), table_assets)
    figure_assets <- Filter(function(x) !(x$name %in% used_asset_names), figure_assets)
  }

  list(
    metadata = list(
      generated_on = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      template = to_relative_path(config$paths$template_in),
      output_yaml = to_relative_path(config$paths$mapping_yaml),
      output_html = to_relative_path(config$paths$mapping_html)
    ),
    available_assets = list(
      tables = table_assets,
      figures = figure_assets
    ),
    inline = new_inline_mapping(placeholders$inline_keys),
    blocks = new_block_mapping(
      block_specs = placeholders$block_specs,
      default_status = config$mapping$default_block_status
    )
  )
}

add_commented_assets <- function(lines, title, asset_paths) {
  lines <- c(lines, paste0("# ", title))

  if (length(asset_paths) == 0) {
    lines <- c(lines, "#   (none found)")
  } else {
    lines <- c(lines, paste0("#   - ", asset_paths))
  }

  c(lines, "#")
}

render_yaml_text <- function(mapping) {
  table_paths <- vapply(mapping$available_assets$tables, `[[`, character(1), "path")
  figure_paths <- vapply(mapping$available_assets$figures, `[[`, character(1), "path")
  table_names <- vapply(mapping$available_assets$tables, `[[`, character(1), "name")
  figure_names <- vapply(mapping$available_assets$figures, `[[`, character(1), "name")

  lines <- c(
    "# ============================================================================",
    "# Mapping file generated by scripts/generate-mapping.R",
    paste0("# Generated on: ", mapping$metadata$generated_on),
    paste0("# Template: ", mapping$metadata$template),
    "#",
    "# Copy any commented asset path below into any block files list when needed.",
    "# ============================================================================",
    "#"
  )

  lines <- add_commented_assets(lines, "Available tables", table_names)
  lines <- add_commented_assets(lines, "Available figures", figure_names)

  lines <- c(lines, "metadata:")
  lines <- c(lines, yaml_field_lines("generated_on", mapping$metadata$generated_on, indent = 2))
  lines <- c(lines, yaml_field_lines("template", mapping$metadata$template, indent = 2))
  lines <- c(lines, yaml_field_lines("output_yaml", mapping$metadata$output_yaml, indent = 2))
  lines <- c(lines, yaml_field_lines("output_html", mapping$metadata$output_html, indent = 2))

  if (length(mapping$inline) == 0) {
    lines <- c(lines, "", "inline: {}")
  } else {
    lines <- c(lines, "", "inline:")
    for (name in names(mapping$inline)) {
      inline_item <- mapping$inline[[name]]
      inline_type <- if (is.list(inline_item)) inline_item$type %||% "text" else "text"
      inline_status <- if (is.list(inline_item)) inline_item$status %||% "new" else "new"
      inline_value <- if (is.list(inline_item)) inline_item$value %||% "" else inline_item

      lines <- c(lines, paste0("  ", yaml_text(name), ":"))
      lines <- c(lines, yaml_field_lines("type", inline_type, indent = 4))
      lines <- c(lines, yaml_field_lines("status", inline_status, indent = 4))
      lines <- c(lines, yaml_field_lines("value", inline_value, indent = 4))
    }
  }

  if (length(mapping$blocks) == 0) {
    lines <- c(lines, "", "blocks: {}")
  } else {
    lines <- c(lines, "", "blocks:")

    block_names <- names(mapping$blocks)
    for (i in seq_along(block_names)) {
      block_name <- block_names[[i]]
      block <- mapping$blocks[[block_name]]

      lines <- c(lines, paste0("  ", yaml_text(block_name), ":"))
      lines <- c(lines, yaml_field_lines("type", block$type, indent = 4))
      lines <- c(lines, yaml_field_lines("title", block$title, indent = 4))
      lines <- c(lines, yaml_field_lines("footnote", block$footnote, indent = 4))
      lines <- c(lines, yaml_field_lines("status", block$status, indent = 4))

      if (length(block$files) == 0) {
        lines <- c(lines, "    files: []")
      } else {
        lines <- c(lines, "    files:")
        lines <- c(lines, paste0("      - ", yaml_text(unlist(block$files, use.names = FALSE))))
      }

      if (i < length(block_names)) {
        lines <- c(lines, "")
      }
    }
  }

  paste(lines, collapse = "\n")
}

build_html_data <- function(mapping) {
  block_data <- list()
  inline_data <- list()

  for (inline_name in names(mapping$inline)) {
    inline_item <- mapping$inline[[inline_name]]
    inline_data[[inline_name]] <- if (is.list(inline_item)) {
      inline_item$value %||% ""
    } else {
      inline_item %||% ""
    }
  }

  for (block_name in names(mapping$blocks)) {
    block <- mapping$blocks[[block_name]]

    block_data[[length(block_data) + 1]] <- list(
      name = block_name,
      type = block$type,
      title = block$title,
      footnote = block$footnote,
      status = block$status,
      files = unlist(block$files, use.names = FALSE)
    )
  }

  list(
    metadata = mapping$metadata,
    inline = inline_data,
    blocks = block_data,
    available_assets = mapping$available_assets
  )
}

generate_mapping_html <- function(mapping, config) {
  config <- normalize_mapping_config(config)
  html_template <- paste(
    readLines(config$mapping$html_template, warn = FALSE),
    collapse = "\n"
  )

  html_data <- jsonlite::toJSON(
    build_html_data(mapping),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )

  html_text <- sub("__MAPPING_DATA__", html_data, html_template, fixed = TRUE)
  writeLines(html_text, config$paths$mapping_html, useBytes = TRUE)
}

generate_mapping_yaml <- function(config) {
  config <- normalize_mapping_config(config)
  mapping <- build_mapping(config)
  yaml_text_out <- render_yaml_text(mapping)

  dir.create(dirname(config$paths$mapping_yaml), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(config$paths$mapping_html), recursive = TRUE, showWarnings = FALSE)

  writeLines(yaml_text_out, config$paths$mapping_yaml, useBytes = TRUE)
  generate_mapping_html(mapping, config)

  message("mapping.yaml generated at: ", config$paths$mapping_yaml)
  message("mapping.html generated at: ", config$paths$mapping_html)

  invisible(mapping)
}
