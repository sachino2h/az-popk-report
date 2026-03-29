# =============================================================================
# main_script.R  —  Round 1 build
#
# PLACEHOLDER FORMAT IN TEMPLATE:
#   <<Vn:variable_name | Tt:Table/Figure title | Fn:Footnote text>>
#
# All three fields are encoded inline — no separate title/footnote paragraphs
# needed in the template. The build script:
#   1. Parses Vn / Tt / Fn from each placeholder
#   2. Inserts: hidden-meta | title paragraph | content | footnote paragraph
#   3. Wraps everything in a named Word bookmark (rpfy_<varname>)
#      so docx_to_yaml.R can reconstruct mapping.yaml from the output doc
#
# WORKFLOW:
#   Step 1 — source("scripts/generate-mapping.R"); generate_mapping_yaml(config)
#   Step 2 — Open output/mapping.html OR edit output/mapping.yaml directly
#   Step 3 — build_full_report(paths)
# =============================================================================

library(officer)
library(flextable)
library(tidyverse)
library(here)
library(yaml)
library(jsonlite)
library(stringr)
library(dplyr)
library(xml2)
library(azreportifyr)

initialize_python()

`%||%` <- function(a, b) if (!is.null(a)) a else b

source(here::here("scripts", "config.R"))
source(here::here("scripts", "logger.R"))
source(here::here("scripts", "generate-mapping.R"))
source(here::here("scripts", "functions", "functions-doc.R"))

config <- get_config()
if (file.exists(config$paths$mapping_yaml)) {
  cleanup_duplicate_files_keys_in_yaml(config$paths$mapping_yaml)
}

generate_magic_doc <- function(
  config,
  mode = c("from_template", "from_reviewed_output"),
  reviewed_doc_path = NULL,
  output_doc_path = NULL,
  inline_tag_style = "brace",
  mark_missing_in_reviewed_output = FALSE
) {
  mode <- match.arg(mode)
  ymlFilePath <- config$paths$mapping_yaml

  if (file.exists(ymlFilePath)) {
    cleanup_duplicate_files_keys_in_yaml(ymlFilePath)
  }

  if (identical(mode, "from_template")) {
    inputDocPath <- config$paths$template_in
    outputDocPath <- config$paths$magic_doc_out

    tags <- extract_tag_pairs_from_doc(inputDocPath, c("<<>>", "{{}}"))
    yml_data <- yaml::read_yaml(ymlFilePath)
    validate_doc_yaml_sections(tags, yml_data)

    apply_result <- apply_magic_doc_replacements(
      input_doc_path = inputDocPath,
      output_doc_path = outputDocPath,
      tags = tags,
      yml_data = yml_data,
      yml_path = ymlFilePath
    )

    return(list(
      replacements = apply_result$replacements,
      remaining_non_inline_tags = apply_result$remaining_non_inline_tags,
      output = apply_result$output
    ))
  }

  if (is.null(reviewed_doc_path) || !nzchar(reviewed_doc_path)) {
    stop("`reviewed_doc_path` is required for mode = 'from_reviewed_output'.", call. = FALSE)
  }
  if (!file.exists(reviewed_doc_path)) {
    stop(paste0("Reviewed DOCX not found: ", reviewed_doc_path), call. = FALSE)
  }

  if (is.null(output_doc_path) || !nzchar(output_doc_path)) {
    output_doc_path <- file.path(
      dirname(config$paths$magic_doc_out),
      "PopPK_Report_merge_TFL_filled_draft-magic-from-output.docx"
    )
  }

  generate_magic_doc_from_reviewed_output(
    yaml_path = ymlFilePath,
    reviewed_doc_path = reviewed_doc_path,
    output_doc_path = output_doc_path,
    inline_tag_style = inline_tag_style,
    mark_missing = mark_missing_in_reviewed_output
  )
}

generate_magic_doc_from_output <- function(
  config,
  reviewed_doc_path,
  output_doc_path = NULL,
  inline_tag_style = "brace",
  mark_missing_in_reviewed_output = FALSE
) {
  generate_magic_doc(
    config = config,
    mode = "from_reviewed_output",
    reviewed_doc_path = reviewed_doc_path,
    output_doc_path = output_doc_path,
    inline_tag_style = inline_tag_style,
    mark_missing_in_reviewed_output = mark_missing_in_reviewed_output
  )
}

sync_reviewed_doc_to_yaml <- function(reviewDocPath, config, add_new_blocks = TRUE) {
  ymlFilePath <- config$paths$mapping_yaml
  
  block_result <- sync_yaml_with_review_doc(
    yaml_path = ymlFilePath,
    review_doc_path = reviewDocPath,
    add_new_blocks = add_new_blocks
  )
  
  inline_result <- sync_inline_tags_with_review_doc(
    yaml_path = ymlFilePath,
    review_doc_path = reviewDocPath
  )

  list(
    yaml_path = ymlFilePath,
    blocks = block_result,
    inline = inline_result
  )
}

sync_review_yaml <- function(config, mode = c("doc_to_yaml", "yaml_to_doc"), reviewDocPath, outputDocPath = NULL, add_new_blocks = TRUE) {
  mode <- match.arg(mode)

  if (mode == "doc_to_yaml") {
    return(sync_reviewed_doc_to_yaml(
      reviewDocPath = reviewDocPath,
      config = config,
      add_new_blocks = add_new_blocks
    ))
  }

  sync_yaml_to_review_doc(
    yaml_path = config$paths$mapping_yaml,
    review_doc_path = reviewDocPath,
    output_doc_path = outputDocPath
  )
}

# User-facing backward basic step:
# Update existing YAML from latest reviewed report DOC only.
# This does not regenerate intermediate DOC; it updates YAML in-place with
# style-preserving writers already used by sync helpers.
update_yaml_from_reviewed_doc <- function(config, latest_reviewed_doc, add_new_blocks = FALSE) {
  if (missing(config) || is.null(config) || !is.list(config)) {
    stop("`config` is required and must be a list.", call. = FALSE)
  }
  if (missing(latest_reviewed_doc) || !nzchar(latest_reviewed_doc)) {
    stop("`latest_reviewed_doc` is required.", call. = FALSE)
  }
  if (!file.exists(latest_reviewed_doc)) {
    stop(paste0("Reviewed DOC not found: ", latest_reviewed_doc), call. = FALSE)
  }
  if (is.null(config$paths$mapping_yaml) || !nzchar(config$paths$mapping_yaml)) {
    stop("`config$paths$mapping_yaml` is required.", call. = FALSE)
  }
  if (!file.exists(config$paths$mapping_yaml)) {
    stop(paste0("YAML file not found: ", config$paths$mapping_yaml), call. = FALSE)
  }

  is_report_doc <- has_rpfy_block_markers(latest_reviewed_doc)

  block_result <- sync_yaml_with_review_doc(
    yaml_path = config$paths$mapping_yaml,
    review_doc_path = latest_reviewed_doc,
    add_new_blocks = add_new_blocks,
    allow_key_rename = FALSE
  )

  inline_result <- if (isTRUE(is_report_doc)) {
    sync_inline_tags_with_review_report_doc(
      yaml_path = config$paths$mapping_yaml,
      review_doc_path = latest_reviewed_doc
    )
  } else {
    sync_inline_tags_with_review_doc(
      yaml_path = config$paths$mapping_yaml,
      review_doc_path = latest_reviewed_doc
    )
  }

  list(
    yaml_path = config$paths$mapping_yaml,
    is_report_doc = is_report_doc,
    blocks = block_result,
    inline = inline_result
  )
}

stage_reportifyr_assets <- function(config) {
  output_root <- file.path(config$paths$report_out_dir, "temp_assets")
  tables_out <- file.path(output_root, "tables")
  figures_out <- file.path(output_root, "figures")

  dir.create(tables_out, recursive = TRUE, showWarnings = FALSE)
  dir.create(figures_out, recursive = TRUE, showWarnings = FALSE)

  stage_one_type <- function(source_dirs, target_dir, label) {
    source_dirs <- source_dirs[dir.exists(source_dirs)]
    if (length(source_dirs) == 0) {
      return(invisible(NULL))
    }

    all_files <- character(0)
    for (dir_path in source_dirs) {
      files <- list.files(
        path = dir_path,
        full.names = TRUE,
        recursive = TRUE,
        no.. = TRUE
      )
      files <- files[file.exists(files)]
      all_files <- c(all_files, files)
    }

    all_files <- unique(all_files)
    all_files <- all_files[basename(all_files) != ".DS_Store"]

    if (length(all_files) == 0) {
      return(invisible(NULL))
    }

    file_names <- basename(all_files)
    dup_names <- unique(file_names[duplicated(file_names)])
    if (length(dup_names) > 0) {
      stop(
        paste0(
          "Duplicate ", label, " filenames found across source dirs: ",
          paste(dup_names, collapse = ", "),
          ". Keep unique filenames for staging."
        ),
        call. = FALSE
      )
    }

    for (i in seq_along(all_files)) {
      file.copy(all_files[[i]], file.path(target_dir, file_names[[i]]), overwrite = TRUE)
    }
  }

  stage_one_type(config$paths$source_table_dirs, tables_out, "table")
  stage_one_type(config$paths$source_figure_dirs, figures_out, "figure")

  list(
    tables_path = tables_out,
    figures_path = figures_out
  )
}

# =============================================================================
# 7. RUN
# =============================================================================
# --- FORWARD: template -> intermediate -> report ---
generate_mapping_yaml(config)

magic_result <- generate_magic_doc(
  config = config,
  mode = "from_template"
)

sync_review_yaml(
  config = config,
  mode = "yaml_to_doc",
  reviewDocPath = config$paths$magic_doc_out
)

dir.create(config$paths$report_out_dir, recursive = TRUE, showWarnings = FALSE)
staged_assets <- stage_reportifyr_assets(config)

forward_result <- azreportifyr::build_report_extended(
  docx_in = config$paths$magic_doc_out,
  docx_out = file.path(config$paths$report_out_dir, "report_v002.docx"),
  figures_path = staged_assets$figures_path,
  tables_path = staged_assets$tables_path,
  yaml_in = config$paths$mapping_yaml,
  config_yaml = here::here("report", "config.yml"),
  version = 2,
  versions_root = config$paths$report_out_dir
)

# Backward basic step: update YAML from latest reviewed report.
# Point this path to the reviewed file received back from user.
latest_reviewed_doc <- file.path(config$paths$report_out_dir, "v002", "report_v002.docx")
yaml_update_result <- update_yaml_from_reviewed_doc(
  config = config,
  latest_reviewed_doc = latest_reviewed_doc,
  add_new_blocks = FALSE
)

# Regenerate intermediate magic DOC from reviewed output + updated YAML
magic_round2_result <- generate_magic_doc(
  config = config,
  mode = "from_reviewed_output",
  reviewed_doc_path = latest_reviewed_doc,
  output_doc_path = file.path(
    dirname(config$paths$magic_doc_out),
    "PopPK_Report_merge_TFL_filled_draft-magic.docx"
  ),
  mark_missing_in_reviewed_output = TRUE
)
