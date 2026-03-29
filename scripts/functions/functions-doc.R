library(officer)

WORD_NS <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"

detect_doc_tag_type <- function(raw_tag, clean_value) {
  raw_tag <- as.character(raw_tag)
  clean_value <- as.character(clean_value)

  if (startsWith(raw_tag, "{{")) {
    return("inline")
  }

  prefix_match <- regexec("^([[:alpha:]][[:alnum:]_-]*):", clean_value)
  prefix_parts <- regmatches(clean_value, prefix_match)[[1]]

  if (length(prefix_parts) > 1) {
    return(tolower(prefix_parts[2]))
  }

  "block"
}

extract_tag_pairs_from_doc <- function(file_path, patterns = c("<<>>", "{{}}")) {
  doc <- read_docx(file_path)
  doc_data <- docx_summary(doc)
  doc_text <- paste(doc_data$text, collapse = " ")
  
  out <- list()
  
  for (p in patterns) {
    if (p == "<<>>") {
      rx <- "<<(.*?)>>"
    } else if (p == "{{}}") {
      rx <- "\\{\\{(.*?)\\}\\}"
    } else {
      stop(paste("Unsupported pattern:", p), call. = FALSE)
    }
    
    matches <- stringr::str_match_all(doc_text, rx)[[1]]
    
    if (nrow(matches) > 0) {
      extracted_values <- matches[, 2]
      clean_values <- vapply(extracted_values, normalize_doc_key, character(1))
      tag_values <- vapply(
        seq_len(nrow(matches)),
        function(i) detect_doc_tag_type(matches[i, 1], extracted_values[i]),
        character(1)
      )

      out[[length(out) + 1]] <- data.frame(
        tag   = tag_values,
        raw   = matches[, 1],
        clean = clean_values,
        stringsAsFactors = FALSE
      )
    }
  }
  
  if (length(out) == 0) {
    return(data.frame(
      tag = character(0),
      raw = character(0),
      clean = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  unique(do.call(rbind, out))
}

normalize_doc_key <- function(tag) {
  if (is.null(tag) || length(tag) == 0 || is.na(tag)) {
    tag <- ""
  }

  tag <- as.character(tag)

  if (grepl(":", tag, fixed = TRUE)) {
    sub("^[^:]*:", "", tag)
  } else {
    tag
  }
}

get_yaml_keys <- function(yml_data) {
  inline_keys <- if (!is.null(yml_data$inline) && is.list(yml_data$inline)) {
    names(yml_data$inline)
  } else {
    character(0)
  }
  
  block_keys <- if (!is.null(yml_data$blocks) && is.list(yml_data$blocks)) {
    names(yml_data$blocks)
  } else {
    character(0)
  }
  
  unique(c(inline_keys, block_keys))
}

validate_doc_yaml_keys <- function(doc_tags, yaml_keys) {
  normalized_doc_keys <- if ("clean" %in% names(doc_tags)) {
    unique(doc_tags$clean)
  } else {
    character(0)
  }

  yaml_keys <- unique(vapply(yaml_keys, normalize_doc_key, character(1)))
  
  missing_in_yaml <- setdiff(normalized_doc_keys, yaml_keys)
  missing_in_doc  <- setdiff(yaml_keys, normalized_doc_keys)
  
  if (length(missing_in_yaml) > 0 || length(missing_in_doc) > 0) {
    msg <- c()
    
    if (length(missing_in_yaml) > 0) {
      msg <- c(
        msg,
        paste0(
          "These document keys are missing in YAML: ",
          paste(sprintf("'%s'", missing_in_yaml), collapse = ", ")
        )
      )
    }
    
    if (length(missing_in_doc) > 0) {
      msg <- c(
        msg,
        paste0(
          "These YAML keys are missing in document: ",
          paste(sprintf("'%s'", missing_in_doc), collapse = ", ")
        )
      )
    }
    
    stop(paste(msg, collapse = "\n"), call. = FALSE)
  }
  
  invisible(TRUE)
}

get_yaml_section_keys <- function(yml_data, section_name) {
  section_data <- yml_data[[section_name]]

  if (is.null(section_data) || !is.list(section_data)) {
    return(character(0))
  }

  unique(vapply(names(section_data), normalize_doc_key, character(1)))
}

validate_doc_yaml_sections <- function(doc_tags, yml_data) {
  inline_doc_keys <- unique(doc_tags$clean[doc_tags$tag %in% c("inline", "text")])
  block_doc_keys <- unique(doc_tags$clean[!doc_tags$tag %in% c("inline", "text", "title")])

  inline_yaml_keys <- get_yaml_section_keys(yml_data, "inline")
  block_yaml_keys <- get_yaml_section_keys(yml_data, "blocks")

  validate_doc_yaml_keys(
    data.frame(clean = inline_doc_keys, stringsAsFactors = FALSE),
    inline_yaml_keys
  )

  validate_doc_yaml_keys(
    data.frame(clean = block_doc_keys, stringsAsFactors = FALSE),
    block_yaml_keys
  )

  invisible(TRUE)
}

normalize_doc_scalar <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return("")
  }

  value <- as.character(x[[1]])
  value <- trimws(value)
  if (!nzchar(value)) "" else value
}

normalize_doc_files <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return("")
  }

  values <- unlist(x, use.names = FALSE)
  if (length(values) == 0) {
    return("")
  }

  values <- trimws(as.character(values))
  values <- values[!is.na(values) & nzchar(values)]

  if (length(values) == 0) {
    return("")
  }

  paste(values, collapse = ", ")
}

yaml_single_quote <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    value <- ""
  }
  value <- as.character(value)
  paste0("'", gsub("'", "''", value, fixed = TRUE), "'")
}

is_missing_marker <- function(value) {
  value <- normalize_doc_scalar(value)
  if (!nzchar(value)) {
    return(FALSE)
  }

  grepl("^MISSING(_VALUE)?\\(.*\\)$", value)
}

normalize_review_value <- function(value) {
  value <- normalize_doc_scalar(value)
  if (is_missing_marker(value)) {
    return("")
  }
  value
}

field_or_missing <- function(value, label) {
  if (!nzchar(value)) {
    return(list(value = paste0("MISSING_VALUE(", label, ")"), missing = TRUE))
  }

  list(value = value, missing = FALSE)
}

normalize_block_key <- function(key) {
  key <- as.character(key)
  key <- trimws(key)
  key <- tolower(key)
  key <- gsub("[[:space:]]+", " ", key)
  key
}

slugify_key <- function(key) {
  key <- as.character(key)
  key <- tolower(key)
  key <- gsub("[^a-z0-9]+", "_", key)
  key <- gsub("^_+|_+$", "", key)
  if (!nzchar(key)) "block" else key
}

make_block_id <- function(block_key) {
  paste0("blk_", slugify_key(block_key))
}

make_inline_id <- function(inline_key) {
  paste0("inl_", slugify_key(inline_key))
}

ensure_unique_block_key <- function(candidate_key, existing_keys, exclude_key = NULL) {
  base <- normalize_doc_scalar(candidate_key)
  base <- gsub("[[:space:]]+", " ", base)
  if (!nzchar(base)) {
    base <- "new block"
  }
  if (is.null(exclude_key)) {
    exclude_key <- character(0)
  }
  keys <- setdiff(existing_keys, exclude_key)

  if (!(base %in% keys)) {
    return(base)
  }

  idx <- 2
  next_key <- paste0(base, " (", idx, ")")
  while (next_key %in% keys) {
    idx <- idx + 1
    next_key <- paste0(base, " (", idx, ")")
  }
  next_key
}

normalize_doc_key_exact <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) {
    return("")
  }

  value <- as.character(x[[1]])
  if (is.na(value)) {
    return("")
  }
  value
}

ensure_unique_block_key_exact <- function(candidate_key, existing_keys, exclude_key = NULL) {
  base <- normalize_doc_key_exact(candidate_key)
  if (!nzchar(base)) {
    base <- "new block"
  }
  if (is.null(exclude_key)) {
    exclude_key <- character(0)
  }
  keys <- setdiff(existing_keys, exclude_key)

  if (!(base %in% keys)) {
    return(base)
  }

  idx <- 2
  next_key <- paste0(base, " (", idx, ")")
  while (next_key %in% keys) {
    idx <- idx + 1
    next_key <- paste0(base, " (", idx, ")")
  }
  next_key
}

ensure_block_ids <- function(yml_data) {
  if (is.null(yml_data$blocks) || !is.list(yml_data$blocks)) {
    return(list(data = yml_data, changed = FALSE))
  }

  changed <- FALSE

  for (block_key in names(yml_data$blocks)) {
    block_data <- yml_data$blocks[[block_key]]
    if (is.null(block_data) || !is.list(block_data)) {
      block_data <- list()
    }

    current_id <- normalize_doc_scalar(block_data$id)
    if (!nzchar(current_id)) {
      block_data$id <- make_block_id(block_key)
      changed <- TRUE
    }

    yml_data$blocks[[block_key]] <- block_data
  }

  list(data = yml_data, changed = changed)
}

resolve_duplicate_block_ids <- function(yml_data) {
  if (is.null(yml_data$blocks) || !is.list(yml_data$blocks)) {
    return(list(data = yml_data, changed = FALSE))
  }

  block_keys <- names(yml_data$blocks)
  block_ids <- vapply(
    block_keys,
    function(key) normalize_doc_scalar(yml_data$blocks[[key]]$id),
    character(1)
  )
  names(block_ids) <- block_keys

  dup_ids <- unique(block_ids[nzchar(block_ids) & duplicated(block_ids)])
  if (length(dup_ids) == 0) {
    return(list(data = yml_data, changed = FALSE))
  }

  status_score <- function(status_value) {
    status_value <- normalize_doc_scalar(status_value)
    switch(
      status_value,
      "updated" = 6,
      "reordered" = 5,
      "unchanged" = 4,
      "new" = 3,
      "deleted" = 1,
      2
    )
  }

  changed <- FALSE

  for (dup_id in dup_ids) {
    keys <- names(block_ids)[block_ids == dup_id]
    if (length(keys) <= 1) {
      next
    }

    scored <- lapply(keys, function(key) {
      block <- yml_data$blocks[[key]]
      score <- status_score(block$status)
      if (identical(normalize_doc_scalar(block$key_renamed), "true")) {
        score <- score + 0.1
      }
      list(
        key = key,
        score = score,
        order = suppressWarnings(as.numeric(if (is.null(block$order)) NA else block$order))
      )
    })

    scores <- vapply(scored, `[[`, numeric(1), "score")
    best_idx <- which(scores == max(scores))

    if (length(best_idx) > 1) {
      orders <- vapply(scored[best_idx], function(x) x$order, numeric(1))
      finite_idx <- which(is.finite(orders))
      if (length(finite_idx) > 0) {
        best_idx <- best_idx[finite_idx[which.min(orders[finite_idx])]]
      } else {
        best_idx <- best_idx[[1]]
      }
    } else {
      best_idx <- best_idx[[1]]
    }

    keep_key <- scored[[best_idx]]$key
    drop_keys <- setdiff(keys, keep_key)
    if (length(drop_keys) > 0) {
      for (drop_key in drop_keys) {
        yml_data$blocks[[drop_key]] <- NULL
      }
      changed <- TRUE
    }
  }

  list(data = yml_data, changed = changed)
}

find_block_key_by_clean <- function(yml_data, clean_key) {
  if (is.null(yml_data$blocks) || !is.list(yml_data$blocks)) {
    return(clean_key)
  }

  block_keys <- names(yml_data$blocks)
  if (clean_key %in% block_keys) {
    return(clean_key)
  }

  target <- normalize_block_key(clean_key)
  for (block_key in block_keys) {
    if (identical(normalize_block_key(block_key), target)) {
      return(block_key)
    }
  }

  # Support renamed keys: template may still contain the previous key text.
  for (block_key in block_keys) {
    block_data <- yml_data$blocks[[block_key]]
    if (is.null(block_data) || !is.list(block_data)) {
      next
    }
    previous_key <- normalize_doc_scalar(block_data$previous_key)
    if (nzchar(previous_key) && identical(normalize_block_key(previous_key), target)) {
      return(block_key)
    }
  }

  clean_key
}

build_magic_block_replacement <- function(clean_key, yml_data) {
  block_key <- find_block_key_by_clean(yml_data, clean_key)
  block_data <- yml_data$blocks[[block_key]]
  if (is.null(block_data) || !is.list(block_data)) {
    block_data <- list()
  }

  block_id <- normalize_doc_scalar(block_data$id)
  if (!nzchar(block_id)) {
    block_id <- make_block_id(block_key)
  }

  display_name <- normalize_doc_key_exact(block_data$name)
  if (!nzchar(display_name)) {
    display_name <- normalize_doc_key_exact(block_key)
  }

  vn <- field_or_missing(display_name, "Name")
  vt <- field_or_missing(normalize_doc_scalar(block_data$title), "Title")
  vft <- field_or_missing(normalize_doc_scalar(block_data$footnote), "Footnote")
  vf <- field_or_missing(normalize_doc_files(block_data$files), "Files")
  vty <- field_or_missing(normalize_doc_scalar(block_data$type), "Type")

  replacement <- paste0(
    "<<Name:", vn$value,
    "|Title:", vt$value,
    "|Footnote:", vft$value,
    "|Files:", vf$value,
    "|Type:", vty$value,
    ">>"
  )

  list(
    block_key = block_key,
    block_id = block_id,
    replacement = replacement,
    has_missing = any(c(vn$missing, vt$missing, vft$missing, vf$missing, vty$missing))
  )
}

build_magic_replacements <- function(tags, yml_data) {
  replace_candidates <- tags |>
    dplyr::filter(!(tag %in% c("inline", "text"))) |>
    dplyr::distinct(raw, clean)

  if (nrow(replace_candidates) == 0) {
    return(data.frame(
      raw = character(0),
      clean = character(0),
      replacement = character(0),
      has_missing = logical(0),
      stringsAsFactors = FALSE
    ))
  }

  replacement_map <- vector("list", nrow(replace_candidates))

  for (i in seq_len(nrow(replace_candidates))) {
    raw_tag <- replace_candidates$raw[[i]]
    clean_key <- replace_candidates$clean[[i]]
    replacement_info <- build_magic_block_replacement(clean_key, yml_data)

    replacement_map[[i]] <- data.frame(
      raw = raw_tag,
      clean = clean_key,
      block_key = replacement_info$block_key,
      block_id = replacement_info$block_id,
      replacement = replacement_info$replacement,
      has_missing = replacement_info$has_missing,
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(replacement_map)
}

replace_split_placeholders <- function(doc, replacements_df) {
  if (nrow(replacements_df) == 0) {
    return(doc)
  }

  body_xml <- docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  paragraphs <- xml2::xml_find_all(body_xml, ".//w:p", ns)

  if (length(paragraphs) == 0) {
    return(doc)
  }

  for (paragraph in paragraphs) {
    text_nodes <- xml2::xml_find_all(paragraph, ".//w:t", ns)
    if (length(text_nodes) == 0) {
      next
    }

    full_text <- paste(xml2::xml_text(text_nodes), collapse = "")
    updated_text <- full_text

    for (i in seq_len(nrow(replacements_df))) {
      updated_text <- gsub(
        replacements_df$raw[[i]],
        replacements_df$replacement[[i]],
        updated_text,
        fixed = TRUE
      )
    }

    if (!identical(updated_text, full_text)) {
      xml2::xml_text(text_nodes[[1]]) <- updated_text
      if (length(text_nodes) > 1) {
        for (idx in 2:length(text_nodes)) {
          xml2::xml_text(text_nodes[[idx]]) <- ""
        }
      }
    }
  }

  doc
}

color_missing_runs_red <- function(doc) {
  body_xml <- docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  target_nodes <- xml2::xml_find_all(body_xml, ".//w:t[contains(., 'MISSING_VALUE(')]", ns)

  if (length(target_nodes) == 0) {
    return(doc)
  }

  for (text_node in target_nodes) {
    run_node <- xml2::xml_find_first(text_node, "ancestor::w:r[1]", ns)
    if (inherits(run_node, "xml_missing")) {
      next
    }

    rpr_node <- xml2::xml_find_first(run_node, "./w:rPr", ns)
    if (inherits(rpr_node, "xml_missing")) {
      rpr_node <- xml2::xml_add_child(
        run_node,
        xml2::read_xml(
          sprintf(
            '<w:rPr xmlns:w="%s"/>',
            WORD_NS
          )
        ),
        .where = 0
      )
    }

    color_node <- xml2::xml_find_first(rpr_node, "./w:color", ns)
    if (inherits(color_node, "xml_missing")) {
      xml2::xml_add_child(
        rpr_node,
        xml2::read_xml(
          sprintf(
            '<w:color xmlns:w="%s" w:val="FF0000"/>',
            WORD_NS
          )
        )
      )
    } else {
      xml2::xml_set_attr(color_node, "val", "FF0000")
    }
  }

  doc
}

add_hidden_ids_to_doc <- function(doc, replacements_df) {
  if (nrow(replacements_df) == 0) {
    return(doc)
  }

  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  paragraphs <- xml2::xml_find_all(body_xml, ".//w:p", ns)

  if (length(paragraphs) == 0) {
    return(doc)
  }

  for (paragraph in paragraphs) {
    text_nodes <- xml2::xml_find_all(paragraph, ".//w:t", ns)
    if (length(text_nodes) == 0) {
      next
    }

    full_text <- paste(xml2::xml_text(text_nodes), collapse = "")

    for (i in seq_len(nrow(replacements_df))) {
      replacement_value <- replacements_df$replacement[[i]]
      block_id <- normalize_doc_scalar(replacements_df$block_id[[i]])
      if (!nzchar(block_id)) {
        next
      }

      if (!grepl(replacement_value, full_text, fixed = TRUE)) {
        next
      }

      marker <- paste0("[[BLOCK_ID:", block_id, "]]")
      if (grepl(marker, full_text, fixed = TRUE)) {
        next
      }

      hidden_run <- xml2::read_xml(
        sprintf(
          '<w:r xmlns:w="%s"><w:rPr><w:vanish/></w:rPr><w:t>%s</w:t></w:r>',
          WORD_NS,
          marker
        )
      )
      xml2::xml_add_child(paragraph, hidden_run)
    }
  }

  doc
}

extract_magic_fields <- function(placeholder_text) {
  value <- gsub("^<<|>>$", "", placeholder_text)
  parts <- strsplit(value, "|", fixed = TRUE)[[1]]

  out <- list(Name = "", Title = "", Footnote = "", Files = "", Type = "")

  for (part in parts) {
    kv <- strsplit(part, ":", fixed = TRUE)[[1]]
    if (length(kv) < 2) {
      next
    }

    key <- trimws(kv[[1]])
    val_raw <- paste(kv[-1], collapse = ":")
    val <- if (identical(key, "Name")) val_raw else trimws(val_raw)

    if (key %in% names(out)) {
      out[[key]] <- val
    }
  }

  out
}

extract_hidden_block_id <- function(text_value) {
  text_value <- normalize_doc_scalar(text_value)
  if (!nzchar(text_value)) {
    return("")
  }

  block_match <- stringr::str_match(text_value, "\\[\\[\\s*BLOCK_ID\\s*:\\s*([^\\]]+)\\]\\]")
  if (nrow(block_match) > 0 && nzchar(normalize_doc_scalar(block_match[, 2]))) {
    return(normalize_doc_scalar(block_match[, 2]))
  }

  # reportifyr-style hidden IDs
  rpfy_match <- stringr::str_match(text_value, "(RPFY_[A-Za-z0-9_\\-]+)")
  if (nrow(rpfy_match) > 0 && nzchar(normalize_doc_scalar(rpfy_match[, 2]))) {
    return(normalize_doc_scalar(rpfy_match[, 2]))
  }

  # fallback: if a legacy blk_* id appears anywhere in hidden text
  blk_match <- stringr::str_match(text_value, "(blk_[A-Za-z0-9_\\-]+)")
  if (nrow(blk_match) > 0 && nzchar(normalize_doc_scalar(blk_match[, 2]))) {
    return(normalize_doc_scalar(blk_match[, 2]))
  }

  ""
}

remove_hidden_id_markers <- function(text_value) {
  text_value <- as.character(text_value)
  text_value <- gsub("\\[\\[\\s*BLOCK_ID\\s*:[^\\]]+\\]\\]", "", text_value, perl = TRUE)
  text_value <- gsub("\\[\\[\\s*INLINE_ID\\s*:[^\\]]+\\]\\]", "", text_value, perl = TRUE)
  text_value <- gsub("RPFY_[A-Za-z0-9_\\-]+", "", text_value, perl = TRUE)
  text_value
}

extract_review_blocks_from_doc <- function(doc_path) {
  report_rows <- extract_review_blocks_from_report_doc(doc_path)
  if (nrow(report_rows) > 0) {
    return(report_rows)
  }

  doc <- officer::read_docx(doc_path)
  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  paragraphs <- xml2::xml_find_all(body_xml, ".//w:p", ns)

  rows <- list()

  for (idx in seq_along(paragraphs)) {
    paragraph <- paragraphs[[idx]]
    text_nodes <- xml2::xml_find_all(paragraph, ".//w:t", ns)
    if (length(text_nodes) == 0) {
      next
    }

    full_text <- paste(xml2::xml_text(text_nodes), collapse = "")
    block_id <- extract_hidden_block_id(full_text)
    visible_text <- remove_hidden_id_markers(full_text)
    placeholders <- stringr::str_extract_all(visible_text, "<<(.*?)>>")[[1]]

    if (length(placeholders) == 0) {
      # Keep ID presence even when visible placeholders are removed in the reviewed DOC.
      if (nzchar(block_id)) {
        rows[[length(rows) + 1]] <- data.frame(
          block_id = block_id,
          name = "",
          title = "",
          footnote = "",
          files = "",
          type = "",
          has_placeholder = FALSE,
          order = idx,
          stringsAsFactors = FALSE
        )
      }
      next
    }

    for (placeholder in placeholders) {
      fields <- extract_magic_fields(placeholder)
      if (!nzchar(fields$Name) && !nzchar(fields$Title) && !nzchar(fields$Footnote) && !nzchar(fields$Files) && !nzchar(fields$Type)) {
        next
      }

      rows[[length(rows) + 1]] <- data.frame(
        block_id = block_id,
        name = fields$Name,
        title = fields$Title,
        footnote = fields$Footnote,
        files = fields$Files,
        type = fields$Type,
        has_placeholder = TRUE,
        order = idx,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      block_id = character(0),
      name = character(0),
      title = character(0),
      footnote = character(0),
      files = character(0),
      type = character(0),
      has_placeholder = logical(0),
      order = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  dplyr::bind_rows(rows)
}

has_rpfy_block_markers <- function(doc_path) {
  doc <- officer::read_docx(doc_path)
  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  text_nodes <- xml2::xml_find_all(body_xml, ".//w:t", ns)
  if (length(text_nodes) == 0) {
    return(FALSE)
  }
  all_text <- paste(xml2::xml_text(text_nodes), collapse = " ")
  grepl("RPFY_BLOCK_START:", all_text, fixed = TRUE)
}

extract_rpfy_markers_from_hidden_runs <- function(hidden_texts) {
  if (length(hidden_texts) == 0) {
    return(data.frame(
      marker_type = character(0),
      block_id = character(0),
      marker_value = character(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- list()
  for (text_value in hidden_texts) {
    marker <- normalize_doc_scalar(text_value)
    if (!grepl("RPFY_", marker, fixed = TRUE)) {
      next
    }

    # Remove legacy BLOCK_ID wrapper if present in same hidden run text.
    marker <- gsub("\\[\\[\\s*BLOCK_ID\\s*:[^\\]]+\\]\\]", "", marker, perl = TRUE)
    marker <- trimws(marker)
    if (!nzchar(marker)) {
      next
    }

    # Some report docs store multiple RPFY markers in one hidden run with no separator.
    # Split by marker prefix boundary and parse each token independently.
    marker <- gsub("(RPFY_[A-Z_]+:)", "|||\\1", marker, perl = TRUE)
    pieces <- strsplit(marker, "\\|\\|\\|", perl = TRUE)[[1]]
    pieces <- trimws(pieces)
    pieces <- pieces[nzchar(pieces) & grepl("^RPFY_[A-Z_]+:", pieces)]

    for (piece in pieces) {
      key_match <- stringr::str_match(piece, "^RPFY_(BLOCK_KEY|BLOCK_TYPE|INLINE_KEY):([^:]+):(.+)$")
      if (nrow(key_match) > 0 && nzchar(normalize_doc_scalar(key_match[, 3]))) {
        token <- normalize_doc_scalar(key_match[, 2])
        marker_type <- switch(
          token,
          "BLOCK_KEY" = "block_key",
          "BLOCK_TYPE" = "block_type",
          "INLINE_KEY" = "inline_key",
          ""
        )
        if (nzchar(marker_type)) {
          out[[length(out) + 1]] <- data.frame(
            marker_type = marker_type,
            block_id = normalize_doc_scalar(key_match[, 3]),
            marker_value = normalize_doc_scalar(key_match[, 4]),
            stringsAsFactors = FALSE
          )
          next
        }
      }

      file_match <- stringr::str_match(piece, "^RPFY_(IMAGE_FILE|TABLE_FILE):([^:]+):(.+)$")
      if (nrow(file_match) > 0 && nzchar(normalize_doc_scalar(file_match[, 3]))) {
        out[[length(out) + 1]] <- data.frame(
          marker_type = if (identical(normalize_doc_scalar(file_match[, 2]), "TABLE_FILE")) "table_file" else "image_file",
          block_id = normalize_doc_scalar(file_match[, 3]),
          marker_value = normalize_doc_scalar(file_match[, 4]),
          stringsAsFactors = FALSE
        )
        next
      }

      generic_match <- stringr::str_match(piece, "^RPFY_([A-Z_]+):(.+)$")
      if (nrow(generic_match) == 0) {
        next
      }

      token <- normalize_doc_scalar(generic_match[, 2])
      value <- normalize_doc_scalar(generic_match[, 3])
      if (!nzchar(value)) {
        next
      }

      marker_type <- switch(
        token,
        "BLOCK_START" = "block_start",
        "BLOCK_END" = "block_end",
        "TITLE_START" = "title_start",
        "TITLE_END" = "title_end",
        "FOOTNOTE_START" = "footnote_start",
        "FOOTNOTE_END" = "footnote_end",
        "IMAGE_START" = "image_start",
        "IMAGE_END" = "image_end",
        "TABLE_START" = "table_start",
        "TABLE_END" = "table_end",
        ""
      )

      if (!nzchar(marker_type)) {
        next
      }

      out[[length(out) + 1]] <- data.frame(
        marker_type = marker_type,
        block_id = value,
        marker_value = "",
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(out) == 0) {
    return(data.frame(
      marker_type = character(0),
      block_id = character(0),
      marker_value = character(0),
      stringsAsFactors = FALSE
    ))
  }

  dplyr::bind_rows(out)
}

extract_review_blocks_from_report_doc <- function(doc_path) {
  doc <- officer::read_docx(doc_path)
  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  paragraphs <- xml2::xml_find_all(body_xml, ".//w:p", ns)

  if (length(paragraphs) == 0) {
    return(data.frame(
      block_id = character(0),
      name = character(0),
      title = character(0),
      footnote = character(0),
      files = character(0),
      type = character(0),
      has_image_content = logical(0),
      has_table_content = logical(0),
      has_placeholder = logical(0),
      order = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  paragraph_visible <- character(length(paragraphs))
  paragraph_has_drawing <- logical(length(paragraphs))
  paragraph_in_table <- logical(length(paragraphs))
  title_text_by_block <- list()
  footnote_text_by_block <- list()
  marker_rows <- list()

  for (idx in seq_along(paragraphs)) {
    paragraph <- paragraphs[[idx]]
    run_nodes <- xml2::xml_find_all(paragraph, ".//w:r", ns)
    if (length(run_nodes) == 0) {
      paragraph_visible[[idx]] <- ""
      next
    }

    visible_texts <- character(0)
    active_title_ids <- character(0)
    active_footnote_ids <- character(0)

    for (run in run_nodes) {
      text_nodes <- xml2::xml_find_all(run, ".//w:t", ns)
      if (length(text_nodes) == 0) next
      run_text <- paste(xml2::xml_text(text_nodes), collapse = "")

      vanish_node <- xml2::xml_find_first(run, "./w:rPr/w:vanish", ns)
      is_hidden <- !inherits(vanish_node, "xml_missing")
      if (is_hidden) {
        run_markers <- extract_rpfy_markers_from_hidden_runs(run_text)
        if (nrow(run_markers) > 0) {
          run_markers$para_idx <- idx
          marker_rows[[length(marker_rows) + 1]] <- run_markers

          for (midx in seq_len(nrow(run_markers))) {
            marker_type <- normalize_doc_scalar(run_markers$marker_type[[midx]])
            marker_block_id <- normalize_doc_scalar(run_markers$block_id[[midx]])
            if (!nzchar(marker_block_id)) next

            if (identical(marker_type, "title_start")) {
              active_title_ids <- c(active_title_ids, marker_block_id)
            } else if (identical(marker_type, "title_end")) {
              m <- which(active_title_ids == marker_block_id)
              if (length(m) > 0) {
                active_title_ids <- active_title_ids[-max(m)]
              }
            } else if (identical(marker_type, "footnote_start")) {
              active_footnote_ids <- c(active_footnote_ids, marker_block_id)
            } else if (identical(marker_type, "footnote_end")) {
              m <- which(active_footnote_ids == marker_block_id)
              if (length(m) > 0) {
                active_footnote_ids <- active_footnote_ids[-max(m)]
              }
            }
          }
        }
      } else {
        visible_texts <- c(visible_texts, run_text)
        if (length(active_title_ids) > 0) {
          target_id <- active_title_ids[[length(active_title_ids)]]
          prev <- title_text_by_block[[target_id]]
          if (is.null(prev)) prev <- ""
          title_text_by_block[[target_id]] <- paste0(prev, run_text)
        }
        if (length(active_footnote_ids) > 0) {
          target_id <- active_footnote_ids[[length(active_footnote_ids)]]
          prev <- footnote_text_by_block[[target_id]]
          if (is.null(prev)) prev <- ""
          footnote_text_by_block[[target_id]] <- paste0(prev, run_text)
        }
      }
    }

    paragraph_visible[[idx]] <- paste(visible_texts, collapse = "")
    paragraph_has_drawing[[idx]] <- length(xml2::xml_find_all(paragraph, ".//w:drawing", ns)) > 0
    paragraph_in_table[[idx]] <- !inherits(
      xml2::xml_find_first(paragraph, "ancestor::w:tbl[1]", ns),
      "xml_missing"
    )
  }

  if (length(marker_rows) == 0) {
    return(data.frame(
      block_id = character(0),
      name = character(0),
      title = character(0),
      footnote = character(0),
      files = character(0),
      type = character(0),
      has_image_content = logical(0),
      has_table_content = logical(0),
      has_placeholder = logical(0),
      order = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  markers <- dplyr::bind_rows(marker_rows)
  markers$para_idx <- as.integer(markers$para_idx)

  block_marker_types <- c(
    "block_start", "block_end",
    "title_start", "title_end",
    "footnote_start", "footnote_end",
    "image_start", "image_end", "image_file",
    "table_start", "table_end", "table_file",
    "block_key", "block_type"
  )

  block_seed_rows <- markers |>
    dplyr::filter(marker_type %in% block_marker_types & nzchar(block_id)) |>
    dplyr::group_by(block_id) |>
    dplyr::summarise(para_idx = min(para_idx), .groups = "drop") |>
    dplyr::arrange(para_idx)

  if (nrow(block_seed_rows) == 0) {
    return(data.frame(
      block_id = character(0),
      name = character(0),
      title = character(0),
      footnote = character(0),
      files = character(0),
      type = character(0),
      has_image_content = logical(0),
      has_table_content = logical(0),
      has_placeholder = logical(0),
      order = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  rows <- list()
  for (i in seq_len(nrow(block_seed_rows))) {
    block_id <- normalize_doc_scalar(block_seed_rows$block_id[[i]])
    start_para <- as.integer(block_seed_rows$para_idx[[i]])
    if (!nzchar(block_id)) next

    block_events <- markers |>
      dplyr::filter(block_id == !!block_id)

    end_para_candidates <- block_events |>
      dplyr::filter(marker_type == "block_end") |>
      dplyr::arrange(para_idx) |>
      dplyr::pull(para_idx)
    end_para <- if (length(end_para_candidates) > 0) {
      as.integer(end_para_candidates[[1]])
    } else {
      max(as.integer(block_events$para_idx), na.rm = TRUE)
    }

    title_start <- block_events |>
      dplyr::filter(marker_type == "title_start") |>
      dplyr::arrange(para_idx) |>
      dplyr::pull(para_idx)
    title_end <- block_events |>
      dplyr::filter(marker_type == "title_end") |>
      dplyr::arrange(para_idx) |>
      dplyr::pull(para_idx)
    foot_start <- block_events |>
      dplyr::filter(marker_type == "footnote_start") |>
      dplyr::arrange(para_idx) |>
      dplyr::pull(para_idx)
    foot_end <- block_events |>
      dplyr::filter(marker_type == "footnote_end") |>
      dplyr::arrange(para_idx) |>
      dplyr::pull(para_idx)

    title <- normalize_review_value(title_text_by_block[[block_id]])
    footnote <- normalize_review_value(footnote_text_by_block[[block_id]])

    block_files <- block_events |>
      dplyr::filter(marker_type %in% c("image_file", "table_file")) |>
      dplyr::pull(marker_value)
    block_files <- unique(normalize_doc_scalar(block_files))
    block_files <- block_files[nzchar(block_files)]

    block_type_marker <- block_events |>
      dplyr::filter(marker_type == "block_type") |>
      dplyr::pull(marker_value)
    block_type_marker <- normalize_doc_scalar(block_type_marker)

    block_type <- ""
    if (nzchar(block_type_marker)) {
      block_type <- tolower(block_type_marker)
    } else if (any(block_events$marker_type == "image_start")) {
      block_type <- "image"
    } else if (any(block_events$marker_type == "table_start")) {
      block_type <- "table"
    }

    content_start <- min(start_para, end_para)
    content_end <- max(start_para, end_para)
    if (content_start < 1L) content_start <- 1L
    if (content_end > length(paragraphs)) content_end <- length(paragraphs)
    range_idx <- seq.int(content_start, content_end)

    # Detect real visible image/table content only (not hidden markers).
    has_image_content <- FALSE
    image_start_rows <- block_events |>
      dplyr::filter(marker_type == "image_start") |>
      dplyr::arrange(para_idx)
    image_end_rows <- block_events |>
      dplyr::filter(marker_type == "image_end") |>
      dplyr::arrange(para_idx)
    if (nrow(image_start_rows) > 0) {
      used_img_end <- integer(0)
      for (sidx in seq_len(nrow(image_start_rows))) {
        img_start <- as.integer(image_start_rows$para_idx[[sidx]])
        cand <- which(
          !(seq_len(nrow(image_end_rows)) %in% used_img_end) &
            as.integer(image_end_rows$para_idx) >= img_start
        )
        img_end <- if (length(cand) > 0) {
          pick <- cand[[1]]
          used_img_end <- c(used_img_end, pick)
          as.integer(image_end_rows$para_idx[[pick]])
        } else {
          # If IMAGE_END is missing, clamp to next section boundary inside
          # same block to avoid bleeding into unrelated later drawings.
          fallback_ends <- block_events |>
            dplyr::filter(
              para_idx >= img_start,
              marker_type %in% c("footnote_start", "table_start", "block_end")
            ) |>
            dplyr::arrange(para_idx) |>
            dplyr::pull(para_idx)
          if (length(fallback_ends) > 0) as.integer(fallback_ends[[1]]) else img_start
        }
        if (img_end < img_start) img_end <- img_start
        img_start <- max(1L, img_start)
        img_end <- min(length(paragraphs), img_end)
        if (img_start <= img_end) {
          if (any(paragraph_has_drawing[seq.int(img_start, img_end)], na.rm = TRUE)) {
            has_image_content <- TRUE
            break
          }
        }
      }
    } else if (length(range_idx) > 0) {
      has_image_content <- any(paragraph_has_drawing[range_idx], na.rm = TRUE)
    }

    has_table_content <- FALSE
    table_start_rows <- block_events |>
      dplyr::filter(marker_type == "table_start") |>
      dplyr::arrange(para_idx)
    table_end_rows <- block_events |>
      dplyr::filter(marker_type == "table_end") |>
      dplyr::arrange(para_idx)
    if (nrow(table_start_rows) > 0) {
      used_tbl_end <- integer(0)
      for (sidx in seq_len(nrow(table_start_rows))) {
        tbl_start <- as.integer(table_start_rows$para_idx[[sidx]])
        cand <- which(
          !(seq_len(nrow(table_end_rows)) %in% used_tbl_end) &
            as.integer(table_end_rows$para_idx) >= tbl_start
        )
        tbl_end <- if (length(cand) > 0) {
          pick <- cand[[1]]
          used_tbl_end <- c(used_tbl_end, pick)
          as.integer(table_end_rows$para_idx[[pick]])
        } else {
          # If TABLE_END is missing, clamp to next section boundary inside
          # same block to avoid bleeding into unrelated later tables.
          fallback_ends <- block_events |>
            dplyr::filter(
              para_idx >= tbl_start,
              marker_type %in% c("footnote_start", "image_start", "block_end")
            ) |>
            dplyr::arrange(para_idx) |>
            dplyr::pull(para_idx)
          if (length(fallback_ends) > 0) as.integer(fallback_ends[[1]]) else tbl_start
        }
        if (tbl_end < tbl_start) tbl_end <- tbl_start
        tbl_start <- max(1L, tbl_start)
        tbl_end <- min(length(paragraphs), tbl_end)
        if (tbl_start <= tbl_end) {
          if (any(paragraph_in_table[seq.int(tbl_start, tbl_end)], na.rm = TRUE)) {
            has_table_content <- TRUE
            break
          }
        }
      }
    } else if (length(range_idx) > 0) {
      has_table_content <- any(paragraph_in_table[range_idx], na.rm = TRUE)
    }

    files_value <- if (length(block_files) > 0) paste(block_files, collapse = ", ") else ""
    # Clear files when real asset content is gone, even if stale hidden file
    # markers remain after user edits in Word.
    if (identical(block_type, "image") && !isTRUE(has_image_content)) {
      files_value <- ""
    }
    if (identical(block_type, "table") && !isTRUE(has_table_content)) {
      files_value <- ""
    }

    block_name_marker <- block_events |>
      dplyr::filter(marker_type == "block_key") |>
      dplyr::pull(marker_value)
    block_name_marker <- normalize_doc_scalar(block_name_marker)

    has_payload <- (
      nzchar(normalize_doc_scalar(title)) ||
      nzchar(normalize_doc_scalar(footnote)) ||
      nzchar(normalize_doc_scalar(files_value)) ||
      isTRUE(has_image_content) ||
      isTRUE(has_table_content)
    )

    rows[[length(rows) + 1]] <- data.frame(
      block_id = block_id,
      name = block_name_marker,
      title = title,
      footnote = footnote,
      files = files_value,
      type = block_type,
      has_image_content = isTRUE(has_image_content),
      has_table_content = isTRUE(has_table_content),
      has_placeholder = isTRUE(has_payload),
      order = as.integer(start_para),
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) {
    return(data.frame(
      block_id = character(0),
      name = character(0),
      title = character(0),
      footnote = character(0),
      files = character(0),
      type = character(0),
      has_image_content = logical(0),
      has_table_content = logical(0),
      has_placeholder = logical(0),
      order = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  dplyr::bind_rows(rows) |>
    dplyr::arrange(order) |>
    dplyr::distinct(block_id, .keep_all = TRUE)
}

parse_files_field <- function(files_value) {
  files_value <- normalize_review_value(files_value)
  if (!nzchar(files_value)) {
    return(list())
  }

  values <- trimws(strsplit(files_value, ",", fixed = TRUE)[[1]])
  values <- values[nzchar(values)]
  as.list(values)
}

replace_or_insert_block_field <- function(lines, block_start, block_end, field_name, field_value) {
  field_pattern <- paste0("^\\s{4}(?:'", field_name, "'|", field_name, "):")
  field_line_idx <- which(grepl(field_pattern, lines[block_start:block_end]))

  new_line <- paste0("    '", field_name, "': ", yaml_single_quote(field_value))

  if (length(field_line_idx) > 0) {
    target_idx <- block_start + field_line_idx[[1]] - 1
    lines[target_idx] <- new_line
    return(lines)
  }

  insert_after <- block_start
  for (candidate in c("type", "title", "footnote", "status")) {
    pat <- paste0("^    '", candidate, "':")
    idx <- which(grepl(pat, lines[block_start:block_end]))
    if (length(idx) > 0) {
      insert_after <- max(insert_after, block_start + idx[[1]] - 1)
    }
  }

  append(lines, values = new_line, after = insert_after)
}

remove_block_field <- function(lines, block_start, block_end, field_name) {
  field_pattern <- paste0("^\\s{4}(?:'", field_name, "'|", field_name, "):")
  field_line_idx <- which(grepl(field_pattern, lines[block_start:block_end]))
  if (length(field_line_idx) == 0) {
    return(lines)
  }
  target_idx <- block_start + field_line_idx[[1]] - 1
  lines[-target_idx]
}

replace_or_insert_block_files <- function(lines, block_start, block_end, files_value) {
  new_file_lines <- if (length(files_value) == 0) {
    c("    files: []")
  } else {
    c("    files:", paste0("      - ", unlist(files_value, use.names = FALSE)))
  }

  # Remove all existing files fields (and their list items) inside this block,
  # then insert a single canonical files field.
  files_header_idx <- which(grepl("^\\s*files:\\s*(\\[\\s*\\])?\\s*$", lines[block_start:block_end]))
  if (length(files_header_idx) > 0) {
    abs_headers <- block_start + files_header_idx - 1L
    remove_idx <- integer(0)

    for (hdr in abs_headers) {
      end_idx <- hdr
      while (end_idx + 1L <= length(lines) && grepl("^\\s{6}-\\s", lines[end_idx + 1L])) {
        end_idx <- end_idx + 1L
      }
      remove_idx <- c(remove_idx, seq.int(hdr, end_idx))
    }

    remove_idx <- sort(unique(remove_idx))
    lines <- lines[-remove_idx]

    removed_in_block <- sum(remove_idx >= block_start & remove_idx <= block_end)
    block_end <- block_end - removed_in_block
  }

  insert_after <- block_start
  for (candidate in c("type", "title", "footnote", "status")) {
    pat <- paste0("^    '", candidate, "':")
    idx <- which(grepl(pat, lines[block_start:block_end]))
    if (length(idx) > 0) {
      insert_after <- max(insert_after, block_start + idx[[1]] - 1)
    }
  }

  append(lines, values = new_file_lines, after = insert_after)
}

append_new_block_text <- function(lines, block_key, block_data) {
  key_renamed <- identical(normalize_doc_scalar(block_data$key_renamed), "true")
  previous_key <- normalize_doc_scalar(block_data$previous_key)

  block_lines <- c(
    "",
    paste0("  ", yaml_single_quote(block_key), ":"),
    paste0("    'id': ", yaml_single_quote(normalize_doc_scalar(block_data$id))),
    paste0("    'type': ", yaml_single_quote(normalize_doc_scalar(block_data$type))),
    paste0("    'title': ", yaml_single_quote(normalize_doc_scalar(block_data$title))),
    paste0("    'footnote': ", yaml_single_quote(normalize_doc_scalar(block_data$footnote))),
    paste0("    'status': ", yaml_single_quote(normalize_doc_scalar(block_data$status)))
  )

  if (key_renamed && nzchar(previous_key)) {
    block_lines <- c(
      block_lines,
      paste0("    'previous_key': ", yaml_single_quote(previous_key)),
      "    'key_renamed': 'true'"
    )
  }

  files_value <- block_data$files
  if (is.null(files_value) || length(files_value) == 0) {
    block_lines <- c(block_lines, "    files: []")
  } else {
    block_lines <- c(block_lines, "    files:")
    block_lines <- c(block_lines, paste0("      - ", unlist(files_value, use.names = FALSE)))
  }

  c(lines, block_lines)
}

parse_yaml_block_ranges <- function(lines, blocks_start) {
  header_idx <- which(seq_along(lines) > blocks_start & grepl("^  '.+':\\s*$", lines))
  if (length(header_idx) == 0) {
    return(list())
  }

  out <- vector("list", length(header_idx))
  for (i in seq_along(header_idx)) {
    start_idx <- header_idx[[i]]
    end_idx <- if (i < length(header_idx)) header_idx[[i + 1]] - 1 else length(lines)
    raw_key <- sub("^\\s*'(.+)':\\s*$", "\\1", lines[start_idx])
    key <- gsub("''", "'", raw_key, fixed = TRUE)
    out[[i]] <- list(key = key, start = start_idx, end = end_idx)
  }
  out
}

remove_top_level_section <- function(lines, section_name) {
  start_idx <- which(grepl(paste0("^", section_name, ":\\s*$"), lines))
  if (length(start_idx) == 0) {
    return(lines)
  }

  start_idx <- start_idx[[1]]
  next_top <- which(
    seq_along(lines) > start_idx &
    grepl("^[A-Za-z_][A-Za-z0-9_]*:\\s*$", lines)
  )
  end_idx <- if (length(next_top) > 0) min(next_top) - 1 else length(lines)

  remove_start <- start_idx
  if (remove_start > 1 && identical(lines[remove_start - 1], "")) {
    remove_start <- remove_start - 1
  }

  lines[-seq.int(remove_start, end_idx)]
}

normalize_inline_entry <- function(entry, default_status = "unchanged") {
  if (is.list(entry) && !is.null(entry$type) && !is.null(entry$status) && !is.null(entry$value)) {
    return(list(
      id = normalize_doc_scalar(entry$id),
      type = "text",
      status = normalize_doc_scalar(entry$status),
      value = normalize_doc_scalar(entry$value)
    ))
  }

  list(
    id = "",
    type = "text",
    status = default_status,
    value = normalize_doc_scalar(entry)
  )
}

ensure_inline_ids <- function(yml_data) {
  if (is.null(yml_data$inline) || !is.list(yml_data$inline)) {
    return(list(data = yml_data, changed = FALSE))
  }

  changed <- FALSE
  used_ids <- character(0)

  for (key in names(yml_data$inline)) {
    entry <- normalize_inline_entry(yml_data$inline[[key]])
    current_id <- normalize_doc_scalar(entry$id)
    if (!nzchar(current_id)) {
      base <- make_inline_id(key)
      current_id <- base
      idx <- 2L
      while (current_id %in% used_ids) {
        current_id <- paste0(base, "_", idx)
        idx <- idx + 1L
      }
      entry$id <- current_id
      changed <- TRUE
    } else if (current_id %in% used_ids) {
      base <- current_id
      idx <- 2L
      next_id <- paste0(base, "_", idx)
      while (next_id %in% used_ids) {
        idx <- idx + 1L
        next_id <- paste0(base, "_", idx)
      }
      entry$id <- next_id
      changed <- TRUE
    }

    used_ids <- c(used_ids, normalize_doc_scalar(entry$id))
    yml_data$inline[[key]] <- entry
  }

  list(data = yml_data, changed = changed)
}

write_inline_sections_preserve_format <- function(yaml_path, inline_map) {
  lines <- readLines(yaml_path, warn = FALSE)
  if (length(lines) == 0) {
    stop("YAML file is empty; cannot preserve format.", call. = FALSE)
  }

  lines <- remove_top_level_section(lines, "inline_status")

  inline_start <- which(grepl("^inline:\\s*$", lines))
  blocks_start <- which(grepl("^blocks:\\s*$", lines))
  if (length(inline_start) == 0 || length(blocks_start) == 0) {
    stop("Could not locate inline/blocks sections in YAML.", call. = FALSE)
  }

  inline_start <- inline_start[[1]]
  blocks_start <- blocks_start[[1]]
  if (blocks_start <= inline_start) {
    stop("Invalid YAML structure: blocks section before inline.", call. = FALSE)
  }

  pre <- if (inline_start > 1) lines[1:(inline_start - 1)] else character(0)
  blocks_part <- lines[blocks_start:length(lines)]

  inline_lines <- c("inline:")
  if (length(inline_map) > 0) {
    for (key in names(inline_map)) {
      entry <- normalize_inline_entry(inline_map[[key]])
      inline_lines <- c(
        inline_lines,
        paste0("  ", yaml_single_quote(key), ":"),
        paste0("    'id': ", yaml_single_quote(entry$id)),
        paste0("    'type': ", yaml_single_quote(entry$type)),
        paste0("    'status': ", yaml_single_quote(entry$status)),
        paste0("    'value': ", yaml_single_quote(entry$value))
      )
    }
  } else {
    inline_lines <- c(inline_lines, "  {}")
  }

  merged <- c(pre, inline_lines, "", blocks_part)
  writeLines(merged, yaml_path, useBytes = TRUE)
  invisible(TRUE)
}

write_mapping_yaml_preserve_format <- function(yaml_path, yml_data) {
  lines <- readLines(yaml_path, warn = FALSE)
  if (length(lines) == 0) {
    stop("YAML file is empty; cannot preserve format.", call. = FALSE)
  }

  blocks_header_idx <- which(grepl("^blocks:\\s*$", lines))
  if (length(blocks_header_idx) == 0) {
    stop("Could not locate 'blocks:' section in YAML.", call. = FALSE)
  }
  blocks_start <- blocks_header_idx[[1]]

  block_keys <- if (!is.null(yml_data$blocks) && is.list(yml_data$blocks)) names(yml_data$blocks) else character(0)
  existing_block_keys <- character(0)

  for (block_key in block_keys) {
    key_pattern <- paste0("^  ", yaml_single_quote(block_key), ":\\s*$")
    start_idx <- which(seq_along(lines) > blocks_start & grepl(key_pattern, lines))

    if (length(start_idx) == 0) {
      prev_key <- normalize_doc_scalar(yml_data$blocks[[block_key]]$previous_key)
      if (nzchar(prev_key)) {
        prev_key_pattern <- paste0("^  ", yaml_single_quote(prev_key), ":\\s*$")
        start_idx <- which(seq_along(lines) > blocks_start & grepl(prev_key_pattern, lines))
        if (length(start_idx) > 0) {
          lines[start_idx[[1]]] <- paste0("  ", yaml_single_quote(block_key), ":")
        }
      }
    }

    if (length(start_idx) == 0) {
      next
    }

    block_start <- start_idx[[1]]
    existing_block_keys <- c(existing_block_keys, block_key)

    next_block_candidates <- which(
      seq_along(lines) > block_start &
      grepl("^  '.+':\\s*$", lines)
    )
    block_end <- if (length(next_block_candidates) > 0) min(next_block_candidates) - 1 else length(lines)

    block_data <- yml_data$blocks[[block_key]]
    lines <- remove_block_field(lines, block_start, block_end, "name")

    next_block_candidates <- which(seq_along(lines) > block_start & grepl("^  '.+':\\s*$", lines))
    block_end <- if (length(next_block_candidates) > 0) min(next_block_candidates) - 1 else length(lines)
    lines <- replace_or_insert_block_field(lines, block_start, block_end, "id", normalize_doc_scalar(block_data$id))

    next_block_candidates <- which(seq_along(lines) > block_start & grepl("^  '.+':\\s*$", lines))
    block_end <- if (length(next_block_candidates) > 0) min(next_block_candidates) - 1 else length(lines)
    lines <- replace_or_insert_block_field(lines, block_start, block_end, "type", normalize_doc_scalar(block_data$type))

    next_block_candidates <- which(seq_along(lines) > block_start & grepl("^  '.+':\\s*$", lines))
    block_end <- if (length(next_block_candidates) > 0) min(next_block_candidates) - 1 else length(lines)
    lines <- replace_or_insert_block_field(lines, block_start, block_end, "title", normalize_doc_scalar(block_data$title))

    next_block_candidates <- which(seq_along(lines) > block_start & grepl("^  '.+':\\s*$", lines))
    block_end <- if (length(next_block_candidates) > 0) min(next_block_candidates) - 1 else length(lines)
    lines <- replace_or_insert_block_field(lines, block_start, block_end, "footnote", normalize_doc_scalar(block_data$footnote))

    next_block_candidates <- which(seq_along(lines) > block_start & grepl("^  '.+':\\s*$", lines))
    block_end <- if (length(next_block_candidates) > 0) min(next_block_candidates) - 1 else length(lines)
    lines <- replace_or_insert_block_field(lines, block_start, block_end, "status", normalize_doc_scalar(block_data$status))

    key_renamed <- identical(normalize_doc_scalar(block_data$key_renamed), "true")
    previous_key <- normalize_doc_scalar(block_data$previous_key)
    if (key_renamed && nzchar(previous_key)) {
      next_block_candidates <- which(seq_along(lines) > block_start & grepl("^  '.+':\\s*$", lines))
      block_end <- if (length(next_block_candidates) > 0) min(next_block_candidates) - 1 else length(lines)
      lines <- replace_or_insert_block_field(lines, block_start, block_end, "previous_key", previous_key)

      next_block_candidates <- which(seq_along(lines) > block_start & grepl("^  '.+':\\s*$", lines))
      block_end <- if (length(next_block_candidates) > 0) min(next_block_candidates) - 1 else length(lines)
      lines <- replace_or_insert_block_field(lines, block_start, block_end, "key_renamed", "true")
    } else {
      next_block_candidates <- which(seq_along(lines) > block_start & grepl("^  '.+':\\s*$", lines))
      block_end <- if (length(next_block_candidates) > 0) min(next_block_candidates) - 1 else length(lines)
      lines <- remove_block_field(lines, block_start, block_end, "previous_key")

      next_block_candidates <- which(seq_along(lines) > block_start & grepl("^  '.+':\\s*$", lines))
      block_end <- if (length(next_block_candidates) > 0) min(next_block_candidates) - 1 else length(lines)
      lines <- remove_block_field(lines, block_start, block_end, "key_renamed")
    }

    next_block_candidates <- which(seq_along(lines) > block_start & grepl("^  '.+':\\s*$", lines))
    block_end <- if (length(next_block_candidates) > 0) min(next_block_candidates) - 1 else length(lines)
    lines <- replace_or_insert_block_files(lines, block_start, block_end, block_data$files)
  }

  new_block_keys <- setdiff(block_keys, existing_block_keys)
  if (length(new_block_keys) > 0) {
    for (block_key in new_block_keys) {
      lines <- append_new_block_text(lines, block_key, yml_data$blocks[[block_key]])
    }
  }

  # Remove stale block sections no longer present in in-memory YAML blocks.
  block_ranges <- parse_yaml_block_ranges(lines, blocks_start)
  stale_ranges <- Filter(function(x) !(x$key %in% block_keys), block_ranges)
  if (length(stale_ranges) > 0) {
    for (range_item in rev(stale_ranges)) {
      remove_start <- range_item$start
      if (remove_start > 1 && identical(lines[remove_start - 1], "")) {
        remove_start <- remove_start - 1
      }
      lines <- lines[-seq.int(remove_start, range_item$end)]
    }
  }

  writeLines(lines, yaml_path, useBytes = TRUE)
  invisible(TRUE)
}

cleanup_duplicate_files_keys_in_yaml <- function(yaml_path) {
  lines <- readLines(yaml_path, warn = FALSE)
  if (length(lines) == 0) {
    return(invisible(FALSE))
  }

  blocks_header_idx <- which(grepl("^blocks:\\s*$", lines))
  if (length(blocks_header_idx) == 0) {
    return(invisible(FALSE))
  }
  blocks_start <- blocks_header_idx[[1]]
  block_ranges <- parse_yaml_block_ranges(lines, blocks_start)
  if (length(block_ranges) == 0) {
    return(invisible(FALSE))
  }

  changed <- FALSE

  for (range_item in rev(block_ranges)) {
    block_start <- range_item$start
    block_end <- range_item$end
    files_headers <- which(grepl("^\\s*files:\\s*(\\[\\s*\\])?\\s*$", lines[block_start:block_end]))
    if (length(files_headers) <= 1) {
      next
    }

    # Keep first files field, remove any extra files fields and their list items.
    abs_headers <- block_start + files_headers - 1L
    keep_header <- abs_headers[[1]]
    remove_idx <- integer(0)
    for (hdr in abs_headers[-1]) {
      end_idx <- hdr
      while (end_idx + 1L <= length(lines) && grepl("^\\s{6}-\\s", lines[end_idx + 1L])) {
        end_idx <- end_idx + 1L
      }
      remove_idx <- c(remove_idx, seq.int(hdr, end_idx))
    }

    if (length(remove_idx) > 0) {
      lines <- lines[-sort(unique(remove_idx))]
      changed <- TRUE
    }
  }

  if (changed) {
    writeLines(lines, yaml_path, useBytes = TRUE)
  }

  invisible(changed)
}

sync_yaml_with_review_doc <- function(yaml_path, review_doc_path, add_new_blocks = TRUE, allow_key_rename = TRUE) {
  cleanup_duplicate_files_keys_in_yaml(yaml_path)
  yml_data <- yaml::read_yaml(yaml_path)
  ensured <- ensure_block_ids(yml_data)
  yml_data <- ensured$data
  deduped <- resolve_duplicate_block_ids(yml_data)
  yml_data <- deduped$data

  if (is.null(yml_data$blocks) || !is.list(yml_data$blocks)) {
    yml_data$blocks <- list()
  }

  existing_keys <- names(yml_data$blocks)

  # Hard lock for reverse/report sync modes where keys must never be renamed.
  # This also clears stale rename metadata from previous runs.
  if (!isTRUE(allow_key_rename) && length(existing_keys) > 0) {
    for (k in existing_keys) {
      blk <- yml_data$blocks[[k]]
      if (is.null(blk) || !is.list(blk)) next
      blk$previous_key <- NULL
      blk$key_renamed <- NULL
      yml_data$blocks[[k]] <- blk
    }
  }

  existing_ids <- vapply(
    existing_keys,
    function(key) normalize_doc_scalar(yml_data$blocks[[key]]$id),
    character(1)
  )
  names(existing_ids) <- existing_keys

  review_blocks <- extract_review_blocks_from_doc(review_doc_path)
  # Keep rows that either have actual placeholders, or match known YAML block IDs.
  # This drops reportifyr control markers like RPFY_FOOTNOTE_START that are not block identities.
  review_blocks <- review_blocks |>
    dplyr::filter(has_placeholder | block_id %in% existing_ids | !nzchar(block_id))

  missing_id_rows <- review_blocks |>
    dplyr::filter(has_placeholder & !nzchar(block_id))

  if (nrow(missing_id_rows) > 0) {
    stop(
      "Reviewed document contains placeholder(s) without hidden BLOCK_ID. Sync aborted.",
      call. = FALSE
    )
  }

  duplicate_scope_ids <- if (isTRUE(add_new_blocks)) {
    unique(review_blocks$block_id[nzchar(review_blocks$block_id)])
  } else {
    unique(review_blocks$block_id[nzchar(review_blocks$block_id) & review_blocks$block_id %in% existing_ids])
  }

  duplicate_ids <- review_blocks |>
    dplyr::filter(block_id %in% duplicate_scope_ids) |>
    dplyr::filter(nzchar(block_id)) |>
    dplyr::count(block_id, name = "n") |>
    dplyr::filter(n > 1)

  if (nrow(duplicate_ids) > 0) {
    stop(
      paste0(
        "Duplicate BLOCK_ID values in reviewed document: ",
        paste(duplicate_ids$block_id, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  yaml_duplicate_ids <- existing_ids[nzchar(existing_ids)]
  if (length(unique(yaml_duplicate_ids)) != length(yaml_duplicate_ids)) {
    dup_vals <- unique(yaml_duplicate_ids[duplicated(yaml_duplicate_ids)])
    stop(
      paste0(
        "Unresolved duplicate block IDs in YAML after cleanup: ",
        paste(dup_vals, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  status_rows <- list()
  review_id_set <- unique(review_blocks$block_id[nzchar(review_blocks$block_id)])
  original_reviewable_ids <- existing_ids[nzchar(existing_ids) & existing_ids %in% review_id_set]
  rename_actions <- list()
  reserved_keys <- names(yml_data$blocks)
  consumed_review_ids <- character(0)

  for (key in existing_keys) {
    block <- yml_data$blocks[[key]]
    block_id <- normalize_doc_scalar(block$id)
    if (!nzchar(block_id)) {
      block_id <- make_block_id(key)
      block$id <- block_id
    }

    match_row <- review_blocks |>
      dplyr::filter(!(block_id %in% consumed_review_ids)) |>
      dplyr::filter(block_id == !!block_id) |>
      dplyr::slice_head(n = 1)

    if (nrow(match_row) == 0) {
      legacy_previous_key <- normalize_doc_scalar(block$previous_key)
      if (nzchar(legacy_previous_key)) {
        legacy_id <- make_block_id(legacy_previous_key)
        match_row <- review_blocks |>
          dplyr::filter(!(block_id %in% consumed_review_ids)) |>
          dplyr::filter(block_id == !!legacy_id) |>
          dplyr::slice_head(n = 1)
        if (nrow(match_row) > 0) {
          block$id <- legacy_id
          block_id <- legacy_id
        }
      }
    }

    if (nrow(match_row) == 0) {
      block$status <- "deleted"
      yml_data$blocks[[key]] <- block
      status_rows[[length(status_rows) + 1]] <- data.frame(
        block_id = block_id,
        block_key = key,
        status = "deleted",
        stringsAsFactors = FALSE
      )
      next
    }
    consumed_review_ids <- c(consumed_review_ids, normalize_doc_scalar(match_row$block_id[[1]]))

    prev <- list(
      title = normalize_doc_scalar(block$title),
      footnote = normalize_doc_scalar(block$footnote),
      type = normalize_doc_scalar(block$type),
      files = normalize_doc_files(block$files)
    )

    reviewed_name <- normalize_doc_key_exact(match_row$name[[1]])
    has_placeholder <- isTRUE(match_row$has_placeholder[[1]])

    # If block ID still exists in reviewed DOC, treat block as present.
    # This allows partial edits (e.g. title removed) without marking block deleted.
    block$title <- normalize_review_value(match_row$title[[1]])
    block$footnote <- normalize_review_value(match_row$footnote[[1]])
    block$type <- normalize_review_value(match_row$type[[1]])
    next_files <- parse_files_field(match_row$files[[1]])

    # Preserve existing files only when real asset content is still present.
    # If asset content was deleted by user, files must become empty.
    prev_files <- if (!is.null(block$files)) block$files else list()
    next_type <- tolower(normalize_doc_scalar(block$type))
    has_image_content <- if ("has_image_content" %in% names(match_row)) isTRUE(match_row$has_image_content[[1]]) else FALSE
    has_table_content <- if ("has_table_content" %in% names(match_row)) isTRUE(match_row$has_table_content[[1]]) else FALSE

    if (length(next_files) == 0 && length(prev_files) > 0) {
      if (identical(next_type, "image") && has_image_content) {
        next_files <- prev_files
      }
      if (identical(next_type, "table") && has_table_content) {
        next_files <- prev_files
      }
    }

    block$files <- next_files
    block$order <- as.integer(match_row$order[[1]])
    block$key_renamed <- NULL
    block$previous_key <- NULL

    target_key <- key
    if (isTRUE(allow_key_rename) && has_placeholder && nzchar(reviewed_name)) {
      desired_key <- reviewed_name
      if (!identical(desired_key, key)) {
        target_key <- ensure_unique_block_key_exact(
          desired_key,
          existing_keys = reserved_keys,
          exclude_key = key
        )
        block$previous_key <- key
        block$key_renamed <- "true"
        reserved_keys <- c(setdiff(reserved_keys, key), target_key)
      }
    }

    next_state <- "unchanged"
    if (
      !identical(prev$title, normalize_doc_scalar(block$title)) ||
      !identical(prev$footnote, normalize_doc_scalar(block$footnote)) ||
      !identical(prev$type, normalize_doc_scalar(block$type)) ||
      !identical(prev$files, normalize_doc_files(block$files))
    ) {
      next_state <- "updated"
    }

    block$status <- next_state
    yml_data$blocks[[key]] <- block
    if (!identical(target_key, key)) {
      rename_actions[[length(rename_actions) + 1]] <- list(
        old_key = key,
        new_key = target_key
      )
    }
    status_rows[[length(status_rows) + 1]] <- data.frame(
      block_id = block_id,
      block_key = target_key,
      status = next_state,
      stringsAsFactors = FALSE
    )
  }

  if (length(rename_actions) > 0) {
    for (action in rename_actions) {
      old_key <- action$old_key
      new_key <- action$new_key
      if (is.null(yml_data$blocks[[old_key]])) {
        next
      }
      yml_data$blocks[[new_key]] <- yml_data$blocks[[old_key]]
      yml_data$blocks[[old_key]] <- NULL
    }
  }

  new_rows <- review_blocks |>
    dplyr::filter(nzchar(block_id) & !(block_id %in% existing_ids) & has_placeholder)

  if (isTRUE(add_new_blocks) && nrow(new_rows) > 0) {
    for (i in seq_len(nrow(new_rows))) {
      row <- new_rows[i, ]
      new_key <- ensure_unique_block_key_exact(
        normalize_doc_key_exact(row$name[[1]]),
        existing_keys = names(yml_data$blocks)
      )

      yml_data$blocks[[new_key]] <- list(
        id = normalize_doc_scalar(row$block_id[[1]]),
        type = normalize_review_value(row$type[[1]]),
        title = normalize_review_value(row$title[[1]]),
        footnote = normalize_review_value(row$footnote[[1]]),
        status = "new",
        order = as.integer(row$order[[1]]),
        files = parse_files_field(row$files[[1]])
      )

      status_rows[[length(status_rows) + 1]] <- data.frame(
        block_id = normalize_doc_scalar(row$block_id[[1]]),
        block_key = new_key,
        status = "new",
        stringsAsFactors = FALSE
      )
    }
  }

  reviewed_order_ids <- review_blocks |>
    dplyr::arrange(order) |>
    dplyr::distinct(block_id, .keep_all = TRUE) |>
    dplyr::pull(block_id)

  moved_ids <- character(0)
  if (length(original_reviewable_ids) == length(reviewed_order_ids) && length(reviewed_order_ids) > 0) {
    moved_ids <- reviewed_order_ids[reviewed_order_ids != original_reviewable_ids]
  }

  if (length(moved_ids) > 0 && length(status_rows) > 0) {
    for (i in seq_along(status_rows)) {
      row <- status_rows[[i]]
      if (row$block_id[[1]] %in% moved_ids && row$status[[1]] == "unchanged") {
        row$status <- "reordered"
        status_rows[[i]] <- row

        key <- row$block_key[[1]]
        if (!is.null(yml_data$blocks[[key]])) {
          yml_data$blocks[[key]]$status <- "reordered"
        }
      }
    }
  }

  ordered_blocks <- yml_data$blocks
  if (length(ordered_blocks) > 0) {
    block_orders <- vapply(
      names(ordered_blocks),
      function(key) {
        value <- ordered_blocks[[key]]$order
        if (is.null(value) || is.na(value)) Inf else as.numeric(value)
      },
      numeric(1)
    )
    ordered_keys <- names(sort(block_orders, na.last = TRUE))
    yml_data$blocks <- ordered_blocks[ordered_keys]
  }

  write_mapping_yaml_preserve_format(yaml_path, yml_data)

  status_df <- if (length(status_rows) > 0) {
    dplyr::bind_rows(status_rows)
  } else {
    data.frame(
      block_id = character(0),
      block_key = character(0),
      status = character(0),
      stringsAsFactors = FALSE
    )
  }

  list(
    yaml_path = yaml_path,
    statuses = status_df,
    blocks_detected = nrow(review_blocks),
    ids_detected = length(review_id_set)
  )
}

sync_inline_tags_with_review_doc <- function(yaml_path, review_doc_path) {
  cleanup_duplicate_files_keys_in_yaml(yaml_path)
  yml_data <- yaml::read_yaml(yaml_path)
  ensured <- ensure_inline_ids(yml_data)
  yml_data <- ensured$data
  inline_map <- if (!is.null(yml_data$inline) && is.list(yml_data$inline)) yml_data$inline else list()
  if (length(inline_map) > 0) {
    for (key in names(inline_map)) {
      inline_map[[key]] <- normalize_inline_entry(inline_map[[key]])
    }
  }

  doc_tags <- extract_tag_pairs_from_doc(review_doc_path, c("<<>>", "{{}}"))
  reviewed_inline_keys <- doc_tags$clean[doc_tags$tag %in% c("inline", "text")]
  reviewed_inline_keys <- reviewed_inline_keys[!is.na(reviewed_inline_keys) & nzchar(reviewed_inline_keys)]
  reviewed_inline_keys <- unique(reviewed_inline_keys)

  existing_inline_keys <- names(inline_map)
  if (is.null(existing_inline_keys)) {
    existing_inline_keys <- character(0)
  }

  deleted_keys <- setdiff(existing_inline_keys, reviewed_inline_keys)
  new_keys <- setdiff(reviewed_inline_keys, existing_inline_keys)

  # Prefer in-place key renames (not delete+new) when list cardinality is unchanged.
  renamed_pairs <- list()
  if (length(existing_inline_keys) == length(reviewed_inline_keys)) {
    for (i in seq_along(existing_inline_keys)) {
      old_key <- existing_inline_keys[[i]]
      new_key <- reviewed_inline_keys[[i]]

      if (identical(old_key, new_key)) {
        next
      }
      if (!(old_key %in% deleted_keys) || !(new_key %in% new_keys)) {
        next
      }
      if (new_key %in% names(inline_map)) {
        next
      }

      inline_map[[new_key]] <- inline_map[[old_key]]
      inline_map[[old_key]] <- NULL
      renamed_pairs[[length(renamed_pairs) + 1]] <- data.frame(
        old_key = old_key,
        new_key = new_key,
        stringsAsFactors = FALSE
      )
    }
  }

  # Fallback: pair remaining delete/new keys by order.
  deleted_keys <- setdiff(names(inline_map), reviewed_inline_keys)
  new_keys <- setdiff(reviewed_inline_keys, names(inline_map))
  if (length(deleted_keys) > 0 && length(new_keys) > 0) {
    pair_count <- min(length(deleted_keys), length(new_keys))
    for (i in seq_len(pair_count)) {
      old_key <- deleted_keys[[i]]
      new_key <- new_keys[[i]]
      if (new_key %in% names(inline_map)) {
        next
      }
      inline_map[[new_key]] <- inline_map[[old_key]]
      inline_map[[old_key]] <- NULL
      renamed_pairs[[length(renamed_pairs) + 1]] <- data.frame(
        old_key = old_key,
        new_key = new_key,
        stringsAsFactors = FALSE
      )
    }
  }

  existing_inline_keys <- names(inline_map)
  deleted_keys <- setdiff(existing_inline_keys, reviewed_inline_keys)
  new_keys <- setdiff(reviewed_inline_keys, existing_inline_keys)
  unchanged_keys <- intersect(existing_inline_keys, reviewed_inline_keys)

  if (length(deleted_keys) > 0) {
    for (key in deleted_keys) {
      entry <- normalize_inline_entry(inline_map[[key]])
      entry$status <- "deleted"
      entry$value <- ""
      inline_map[[key]] <- entry
    }
  }

  if (length(new_keys) > 0) {
    for (key in new_keys) {
      inline_map[[key]] <- list(
        type = "text",
        status = "new",
        value = ""
      )
    }
  }

  if (length(unchanged_keys) > 0) {
    for (key in unchanged_keys) {
      entry <- normalize_inline_entry(inline_map[[key]])
      if (!identical(entry$status, "new")) {
        entry$status <- "unchanged"
      }
      inline_map[[key]] <- entry
    }
  }

  ordered_keys <- c(existing_inline_keys, setdiff(new_keys, existing_inline_keys))
  ordered_keys <- unique(ordered_keys)
  inline_map <- inline_map[ordered_keys]

  write_inline_sections_preserve_format(
    yaml_path = yaml_path,
    inline_map = inline_map
  )

  renamed_df <- if (length(renamed_pairs) > 0) dplyr::bind_rows(renamed_pairs) else
    data.frame(old_key = character(0), new_key = character(0), stringsAsFactors = FALSE)

  list(
    yaml_path = yaml_path,
    renamed = renamed_df,
    new = length(new_keys),
    deleted = length(deleted_keys),
    unchanged = length(unchanged_keys)
  )
}

extract_inline_marker_tokens <- function(run_text) {
  run_text <- normalize_doc_scalar(run_text)
  if (!nzchar(run_text) || !grepl("RPFY_INLINE_", run_text, fixed = TRUE)) {
    return(data.frame(
      token = character(0),
      inline_id = character(0),
      stringsAsFactors = FALSE
    ))
  }

  marker <- gsub("\\[\\[\\s*INLINE_ID\\s*:[^\\]]+\\]\\]", "", run_text, perl = TRUE)
  marker <- trimws(marker)
  if (!nzchar(marker)) {
    return(data.frame(
      token = character(0),
      inline_id = character(0),
      stringsAsFactors = FALSE
    ))
  }

  marker <- gsub("(RPFY_INLINE_(START|END):)", "|||\\1", marker, perl = TRUE)
  pieces <- strsplit(marker, "\\|\\|\\|", perl = TRUE)[[1]]
  pieces <- trimws(pieces)
  pieces <- pieces[nzchar(pieces) & grepl("^RPFY_INLINE_(START|END):", pieces)]

  if (length(pieces) == 0) {
    return(data.frame(
      token = character(0),
      inline_id = character(0),
      stringsAsFactors = FALSE
    ))
  }

  out <- lapply(pieces, function(piece) {
    m <- stringr::str_match(piece, "^RPFY_INLINE_(START|END):([^\\s]+?)(?=RPFY_|$)")
    if (nrow(m) == 0) return(NULL)
    tok <- normalize_doc_scalar(m[, 2])
    inl <- normalize_doc_scalar(m[, 3])
    if (!nzchar(tok) || !nzchar(inl)) return(NULL)
    data.frame(
      token = tolower(tok),
      inline_id = inl,
      stringsAsFactors = FALSE
    )
  })

  out <- Filter(Negate(is.null), out)
  if (length(out) == 0) {
    return(data.frame(
      token = character(0),
      inline_id = character(0),
      stringsAsFactors = FALSE
    ))
  }

  dplyr::bind_rows(out)
}

extract_inline_values_from_review_report_doc <- function(review_doc_path) {
  doc <- officer::read_docx(review_doc_path)
  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  paragraphs <- xml2::xml_find_all(body_xml, ".//w:p", ns)

  values <- list()
  seen_ids <- character(0)

  if (length(paragraphs) == 0) {
    return(list(values = values, seen_ids = seen_ids))
  }

  for (paragraph in paragraphs) {
    runs <- xml2::xml_find_all(paragraph, ".//w:r", ns)
    if (length(runs) == 0) next

    active_ids <- character(0)

    for (run in runs) {
      text_nodes <- xml2::xml_find_all(run, ".//w:t", ns)
      if (length(text_nodes) == 0) next
      run_text <- paste(xml2::xml_text(text_nodes), collapse = "")
      if (!nzchar(run_text)) next

      vanish_node <- xml2::xml_find_first(run, "./w:rPr/w:vanish", ns)
      is_hidden <- !inherits(vanish_node, "xml_missing")

      if (is_hidden) {
        tokens <- extract_inline_marker_tokens(run_text)
        if (nrow(tokens) == 0) next

        for (i in seq_len(nrow(tokens))) {
          token <- tokens$token[[i]]
          marker_id <- tokens$inline_id[[i]]
          if (!nzchar(marker_id)) next
          seen_ids <- c(seen_ids, marker_id)

          if (identical(token, "start")) {
            active_ids <- c(active_ids, marker_id)
            if (is.null(values[[marker_id]])) {
              values[[marker_id]] <- ""
            }
          } else if (identical(token, "end")) {
            # Remove most recent matching open marker.
            matches <- which(active_ids == marker_id)
            if (length(matches) > 0) {
              pos <- max(matches)
              active_ids <- active_ids[-pos]
            }
          }
        }
        next
      }

      if (length(active_ids) == 0) next
      current_id <- active_ids[[length(active_ids)]]
      prev <- values[[current_id]]
      if (is.null(prev)) prev <- ""
      values[[current_id]] <- paste0(prev, run_text)
    }
  }

  seen_ids <- unique(seen_ids[nzchar(seen_ids)])
  list(values = values, seen_ids = seen_ids)
}

resolve_inline_key_from_marker_id <- function(marker_id, inline_map) {
  marker_id <- normalize_doc_scalar(marker_id)
  if (!nzchar(marker_id) || length(inline_map) == 0) {
    return("")
  }

  keys <- names(inline_map)
  if (is.null(keys) || length(keys) == 0) {
    return("")
  }

  ids <- vapply(
    keys,
    function(k) normalize_doc_scalar(normalize_inline_entry(inline_map[[k]])$id),
    character(1)
  )
  names(ids) <- keys

  exact <- names(ids)[ids == marker_id]
  if (length(exact) > 0) {
    return(exact[[1]])
  }

  # Support repeated inline markers created as <base>_2, <base>_3, ...
  for (k in keys) {
    base_id <- ids[[k]]
    if (!nzchar(base_id)) next
    escaped_base <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", base_id, perl = TRUE)
    if (grepl(paste0("^", escaped_base, "_[0-9]+$"), marker_id, perl = TRUE)) {
      return(k)
    }
  }

  ""
}

sync_inline_tags_with_review_report_doc <- function(yaml_path, review_doc_path) {
  cleanup_duplicate_files_keys_in_yaml(yaml_path)
  yml_data <- yaml::read_yaml(yaml_path)
  ensured <- ensure_inline_ids(yml_data)
  yml_data <- ensured$data
  inline_map <- if (!is.null(yml_data$inline) && is.list(yml_data$inline)) yml_data$inline else list()
  if (length(inline_map) > 0) {
    for (key in names(inline_map)) {
      inline_map[[key]] <- normalize_inline_entry(inline_map[[key]])
    }
  }

  extracted <- extract_inline_values_from_review_report_doc(review_doc_path)
  seen_marker_ids <- extracted$seen_ids
  seen_keys <- character(0)
  if (length(seen_marker_ids) > 0) {
    seen_keys <- unique(vapply(
      seen_marker_ids,
      function(mid) resolve_inline_key_from_marker_id(mid, inline_map),
      character(1)
    ))
    seen_keys <- seen_keys[nzchar(seen_keys)]
  }

  existing_inline_keys <- names(inline_map)
  if (is.null(existing_inline_keys)) {
    existing_inline_keys <- character(0)
  }

  deleted_count <- 0L
  unchanged_count <- 0L
  updated_count <- 0L
  for (key in existing_inline_keys) {
    entry <- normalize_inline_entry(inline_map[[key]])
    old_value <- normalize_doc_scalar(entry$value)
    marker_ids <- names(extracted$values)
    marker_ids <- marker_ids[nzchar(marker_ids)]
    key_marker_ids <- marker_ids[vapply(
      marker_ids,
      function(mid) identical(resolve_inline_key_from_marker_id(mid, inline_map), key),
      logical(1)
    )]

    if (key %in% seen_keys || length(key_marker_ids) > 0) {
      parts <- character(0)
      if (length(key_marker_ids) > 0) {
        parts <- vapply(
          key_marker_ids,
          function(mid) normalize_review_value(extracted$values[[mid]]),
          character(1)
        )
      }
      parts <- parts[nzchar(parts)]
      new_value <- if (length(parts) > 0) paste(parts, collapse = " ") else ""

      if (!nzchar(new_value)) {
        # In report DOCs, INLINE markers can be appended as hidden runs
        # after visible text, so there may be no captured text "between"
        # START/END even when inline content still exists. In that case,
        # keep existing value and mark unchanged (not deleted).
        if (!identical(entry$status, "new")) {
          entry$status <- "unchanged"
        }
        entry$value <- old_value
        unchanged_count <- unchanged_count + 1L
      } else if (!identical(old_value, new_value)) {
        entry$status <- "updated"
        entry$value <- new_value
        updated_count <- updated_count + 1L
      } else {
        if (!identical(entry$status, "new")) {
          entry$status <- "unchanged"
        }
        entry$value <- new_value
        unchanged_count <- unchanged_count + 1L
      }
    } else {
      entry$status <- "deleted"
      entry$value <- ""
      deleted_count <- deleted_count + 1L
    }
    inline_map[[key]] <- entry
  }

  write_inline_sections_preserve_format(
    yaml_path = yaml_path,
    inline_map = inline_map
  )

  list(
    yaml_path = yaml_path,
    renamed = data.frame(old_key = character(0), new_key = character(0), stringsAsFactors = FALSE),
    new = 0L,
    deleted = deleted_count,
    unchanged = unchanged_count,
    updated = updated_count
  )
}

get_inline_value_from_yaml <- function(inline_entry) {
  if (is.list(inline_entry) && !is.null(inline_entry$value)) {
    return(normalize_doc_scalar(inline_entry$value))
  }
  normalize_doc_scalar(inline_entry)
}

build_review_block_placeholder <- function(block_key, block_data, mark_missing = FALSE) {
  display_name <- normalize_doc_key_exact(block_key)
  block_type <- normalize_doc_scalar(block_data$type)
  title <- normalize_doc_scalar(block_data$title)
  footnote <- normalize_doc_scalar(block_data$footnote)
  files <- normalize_doc_files(block_data$files)

  if (isTRUE(mark_missing)) {
    name_field <- field_or_missing(display_name, "Name")
    title_field <- field_or_missing(title, "Title")
    footnote_field <- field_or_missing(footnote, "Footnote")
    files_field <- field_or_missing(files, "Files")
    type_field <- field_or_missing(block_type, "Type")

    return(paste0(
      "<<Name:", name_field$value,
      "|Title:", title_field$value,
      "|Footnote:", footnote_field$value,
      "|Files:", files_field$value,
      "|Type:", type_field$value,
      ">>"
    ))
  }

  paste0(
    "<<Name:", display_name,
    "|Title:", title,
    "|Footnote:", footnote,
    "|Files:", files,
    "|Type:", block_type,
    ">>"
  )
}

replace_text_in_paragraph_nodes <- function(paragraph, replacement_fn) {
  ns <- xml2::xml_ns(paragraph)
  text_nodes <- xml2::xml_find_all(paragraph, ".//w:t", ns)
  if (length(text_nodes) == 0) {
    return(FALSE)
  }

  original_texts <- xml2::xml_text(text_nodes)
  full_text <- paste(original_texts, collapse = "")
  updated <- replacement_fn(full_text)
  if (identical(updated, full_text)) {
    return(FALSE)
  }

  # Do not place regenerated visible content into hidden runs (w:vanish),
  # otherwise placeholders can appear partially visible.
  is_hidden <- vapply(text_nodes, function(node) {
    run_node <- xml2::xml_find_first(node, "ancestor::w:r[1]", ns)
    if (inherits(run_node, "xml_missing")) return(FALSE)
    vanish_node <- xml2::xml_find_first(run_node, "./w:rPr/w:vanish", ns)
    !inherits(vanish_node, "xml_missing")
  }, logical(1))

  target_idx <- which(!is_hidden)
  if (length(target_idx) == 0) {
    target_idx <- seq_along(text_nodes)
  }

  target_nodes <- text_nodes[target_idx]
  target_texts <- original_texts[target_idx]

  # Preserve run styling by keeping visible text-node boundaries as much as possible.
  node_sizes <- nchar(target_texts, type = "chars", allowNA = FALSE, keepNA = FALSE)
  total_nodes <- length(target_nodes)
  char_pos <- 1L
  updated_len <- nchar(updated, type = "chars")

  # Clear hidden node texts to avoid stale marker fragments.
  hidden_idx <- which(is_hidden)
  if (length(hidden_idx) > 0) {
    for (idx in hidden_idx) {
      xml2::xml_text(text_nodes[[idx]]) <- ""
    }
  }

  for (idx in seq_len(total_nodes)) {
    if (char_pos > updated_len) {
      xml2::xml_text(target_nodes[[idx]]) <- ""
      next
    }

    if (idx < total_nodes) {
      take_n <- node_sizes[[idx]]
      if (take_n <= 0) {
        xml2::xml_text(target_nodes[[idx]]) <- ""
        next
      }
      end_pos <- min(updated_len, char_pos + take_n - 1L)
      xml2::xml_text(target_nodes[[idx]]) <- substr(updated, char_pos, end_pos)
      char_pos <- end_pos + 1L
    } else {
      # Put remaining characters in final node.
      xml2::xml_text(target_nodes[[idx]]) <- substr(updated, char_pos, updated_len)
      char_pos <- updated_len + 1L
    }
  }

  TRUE
}

replace_text_in_visible_nodes <- function(paragraph, replacement_fn) {
  ns <- xml2::xml_ns(paragraph)
  text_nodes <- xml2::xml_find_all(paragraph, ".//w:t", ns)
  if (length(text_nodes) == 0) {
    return(FALSE)
  }

  is_hidden <- vapply(text_nodes, function(node) {
    run_node <- xml2::xml_find_first(node, "ancestor::w:r[1]", ns)
    if (inherits(run_node, "xml_missing")) return(FALSE)
    vanish_node <- xml2::xml_find_first(run_node, "./w:rPr/w:vanish", ns)
    !inherits(vanish_node, "xml_missing")
  }, logical(1))

  visible_idx <- which(!is_hidden)
  if (length(visible_idx) == 0) {
    return(FALSE)
  }

  visible_nodes <- text_nodes[visible_idx]
  original_texts <- xml2::xml_text(visible_nodes)
  full_text <- paste(original_texts, collapse = "")
  updated <- replacement_fn(full_text)
  if (identical(updated, full_text)) {
    return(FALSE)
  }

  node_sizes <- nchar(original_texts, type = "chars", allowNA = FALSE, keepNA = FALSE)
  total_nodes <- length(visible_nodes)
  char_pos <- 1L
  updated_len <- nchar(updated, type = "chars")

  for (idx in seq_len(total_nodes)) {
    if (char_pos > updated_len) {
      xml2::xml_text(visible_nodes[[idx]]) <- ""
      next
    }
    if (idx < total_nodes) {
      take_n <- node_sizes[[idx]]
      if (take_n <= 0) {
        xml2::xml_text(visible_nodes[[idx]]) <- ""
        next
      }
      end_pos <- min(updated_len, char_pos + take_n - 1L)
      xml2::xml_text(visible_nodes[[idx]]) <- substr(updated, char_pos, end_pos)
      char_pos <- end_pos + 1L
    } else {
      xml2::xml_text(visible_nodes[[idx]]) <- substr(updated, char_pos, updated_len)
      char_pos <- updated_len + 1L
    }
  }

  TRUE
}

strip_visible_block_id_markers <- function(doc) {
  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  paragraphs <- xml2::xml_find_all(body_xml, ".//w:p", ns)

  if (length(paragraphs) == 0) {
    return(doc)
  }

  marker_rx <- "\\[\\[\\s*(BLOCK_ID|INLINE_ID)\\s*:[^\\]]+\\]\\]|RPFY_[A-Za-z0-9_\\-]+"

  for (paragraph in paragraphs) {
    replace_text_in_paragraph_nodes(paragraph, function(full_text) {
      gsub(marker_rx, "", full_text, perl = TRUE)
    })
  }

  doc
}

replace_inline_tag_keys_from_yaml <- function(doc, yml_data) {
  inline_map <- if (!is.null(yml_data$inline) && is.list(yml_data$inline)) yml_data$inline else list()
  if (is.null(inline_map) || length(inline_map) == 0) {
    return(list(doc = doc, updates = 0L))
  }

  # Build inline ID -> YAML key mapping.
  inline_id_to_key <- list()
  for (key in names(inline_map)) {
    entry <- normalize_inline_entry(inline_map[[key]])
    inline_id <- normalize_doc_scalar(entry$id)
    if (nzchar(inline_id)) {
      inline_id_to_key[[inline_id]] <- key
    }
  }
  if (length(inline_id_to_key) == 0) {
    return(list(doc = doc, updates = 0L))
  }

  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  paragraphs <- xml2::xml_find_all(body_xml, ".//w:p", ns)
  if (length(paragraphs) == 0) {
    return(list(doc = doc, updates = 0L))
  }

  updates <- 0L

  for (paragraph in paragraphs) {
    changed <- replace_text_in_paragraph_nodes(paragraph, function(full_text) {
      updated <- full_text

      id_matches <- stringr::str_match_all(updated, "\\[\\[INLINE_ID:([^\\]]+)\\]\\]")[[1]]
      if (nrow(id_matches) == 0) {
        return(updated)
      }
      inline_ids <- id_matches[, 2]
      inline_ids <- inline_ids[nzchar(inline_ids)]
      if (length(inline_ids) == 0) {
        return(updated)
      }

      # Update the inline key in tags based on ID mapping.
      # If multiple tags exist in paragraph, apply in marker order.
      for (inline_id in inline_ids) {
        target_key <- inline_id_to_key[[inline_id]]
        if (is.null(target_key) || !nzchar(target_key)) next

        updated <- sub("\\{\\{\\s*(.*?)\\s*\\}\\}", paste0("{{", target_key, "}}"), updated, perl = TRUE)
        updated <- sub("<<\\s*(?i:text)\\s*:\\s*(.*?)\\s*>>", paste0("<<text:", target_key, ">>"), updated, perl = TRUE)
      }

      updated
    })

    if (isTRUE(changed)) {
      updates <- updates + 1L
    }
  }

  list(doc = doc, updates = updates)
}

add_hidden_inline_ids_to_doc <- function(doc, yml_data) {
  inline_map <- if (!is.null(yml_data$inline) && is.list(yml_data$inline)) yml_data$inline else list()
  if (length(inline_map) == 0) {
    return(doc)
  }

  key_to_id <- list()
  for (key in names(inline_map)) {
    entry <- normalize_inline_entry(inline_map[[key]])
    inline_id <- normalize_doc_scalar(entry$id)
    if (nzchar(inline_id)) {
      key_to_id[[key]] <- inline_id
    }
  }
  if (length(key_to_id) == 0) {
    return(doc)
  }

  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  paragraphs <- xml2::xml_find_all(body_xml, ".//w:p", ns)
  if (length(paragraphs) == 0) {
    return(doc)
  }

  for (paragraph in paragraphs) {
    text_nodes <- xml2::xml_find_all(paragraph, ".//w:t", ns)
    if (length(text_nodes) == 0) next
    full_text <- paste(xml2::xml_text(text_nodes), collapse = "")

    brace_keys <- stringr::str_match_all(full_text, "\\{\\{(.*?)\\}\\}")[[1]]
    text_keys <- stringr::str_match_all(full_text, stringr::regex("<<\\s*text\\s*:\\s*(.*?)\\s*>>", ignore_case = TRUE))[[1]]
    keys <- c(
      if (nrow(brace_keys) > 0) trimws(brace_keys[, 2]) else character(0),
      if (nrow(text_keys) > 0) trimws(text_keys[, 2]) else character(0)
    )
    keys <- keys[nzchar(keys)]
    if (length(keys) == 0) next

    for (key in keys) {
      inline_id <- key_to_id[[key]]
      if (is.null(inline_id) || !nzchar(inline_id)) next

      marker <- paste0("[[INLINE_ID:", inline_id, "]]")
      if (grepl(marker, full_text, fixed = TRUE)) next

      hidden_run <- xml2::read_xml(
        sprintf(
          '<w:r xmlns:w="%s"><w:rPr><w:vanish/></w:rPr><w:t>%s</w:t></w:r>',
          WORD_NS,
          marker
        )
      )
      xml2::xml_add_child(paragraph, hidden_run)
      full_text <- paste0(full_text, marker)
    }
  }

  doc
}

sync_yaml_to_review_doc <- function(yaml_path, review_doc_path, output_doc_path = NULL) {
  cleanup_duplicate_files_keys_in_yaml(yaml_path)
  yml_data <- yaml::read_yaml(yaml_path)
  ensured <- ensure_block_ids(yml_data)
  yml_data <- ensured$data
  ensured_inline <- ensure_inline_ids(yml_data)
  yml_data <- ensured_inline$data
  deduped <- resolve_duplicate_block_ids(yml_data)
  yml_data <- deduped$data

  if (is.null(output_doc_path) || !nzchar(output_doc_path)) {
    output_doc_path <- review_doc_path
  }

  doc <- officer::read_docx(review_doc_path)
  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  paragraphs <- xml2::xml_find_all(body_xml, ".//w:p", ns)

  id_to_key <- list()
  block_replacement_rows <- list()
  if (!is.null(yml_data$blocks) && is.list(yml_data$blocks)) {
    for (key in names(yml_data$blocks)) {
      block_data <- yml_data$blocks[[key]]
      block_id <- normalize_doc_scalar(block_data$id)
      if (nzchar(block_id)) {
        id_to_key[[block_id]] <- key
        block_replacement_rows[[length(block_replacement_rows) + 1]] <- data.frame(
          block_id = block_id,
          replacement = build_review_block_placeholder(key, block_data),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  block_updates <- 0L
  inline_updates <- 0L

  if (length(paragraphs) > 0 && length(id_to_key) > 0) {
    for (paragraph in paragraphs) {
      changed <- replace_text_in_paragraph_nodes(paragraph, function(full_text) {
        block_id <- extract_hidden_block_id(full_text)
        if (!nzchar(block_id)) {
          return(full_text)
        }
        if (is.null(id_to_key[[block_id]])) {
          return(full_text)
        }

        block_key <- id_to_key[[block_id]]
        block_data <- yml_data$blocks[[block_key]]
        replacement <- build_review_block_placeholder(block_key, block_data)

        # Deterministic regeneration: for tracked block paragraphs, force a
        # canonical placeholder string instead of partial in-line substitutions.
        remove_hidden_id_markers(replacement)
      })

      if (isTRUE(changed)) {
        block_updates <- block_updates + 1L
      }
    }
  }

  # Preserve inline placeholders (no value replacement), but allow key renames
  # to propagate from YAML keys into inline tags.
  inline_rename <- replace_inline_tag_keys_from_yaml(doc, yml_data)
  doc <- inline_rename$doc
  inline_updates <- as.integer(inline_rename$updates)

  doc <- strip_visible_block_id_markers(doc)

  block_replacements_df <- if (length(block_replacement_rows) > 0) {
    dplyr::bind_rows(block_replacement_rows)
  } else {
    data.frame(block_id = character(0), replacement = character(0), stringsAsFactors = FALSE)
  }

  if (nrow(block_replacements_df) > 0) {
    doc <- add_hidden_ids_to_doc(doc, block_replacements_df)
  }
  doc <- add_hidden_inline_ids_to_doc(doc, yml_data)

  print(doc, target = output_doc_path)

  list(
    yaml_path = yaml_path,
    review_doc_path = review_doc_path,
    output_doc_path = output_doc_path,
    block_paragraphs_updated = block_updates,
    inline_paragraphs_updated = inline_updates
  )
}

build_block_id_to_key_map <- function(yml_data) {
  out <- list()
  if (is.null(yml_data$blocks) || !is.list(yml_data$blocks)) {
    return(out)
  }

  for (key in names(yml_data$blocks)) {
    block <- yml_data$blocks[[key]]
    block_id <- normalize_doc_scalar(block$id)
    if (nzchar(block_id)) {
      out[[block_id]] <- key
    }
  }
  out
}

convert_report_blocks_to_magic_placeholders <- function(doc, yml_data, skip_deleted = TRUE, mark_missing = FALSE) {
  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  paragraphs <- xml2::xml_find_all(body_xml, ".//w:p", ns)
  if (length(paragraphs) == 0) {
    return(list(doc = doc, updates = 0L, replacements = data.frame(
      block_id = character(0),
      replacement = character(0),
      stringsAsFactors = FALSE
    )))
  }

  marker_rows <- list()
  for (idx in seq_along(paragraphs)) {
    p <- paragraphs[[idx]]
    runs <- xml2::xml_find_all(p, ".//w:r", ns)
    if (length(runs) == 0) next

    for (run in runs) {
      vanish_node <- xml2::xml_find_first(run, "./w:rPr/w:vanish", ns)
      is_hidden <- !inherits(vanish_node, "xml_missing")
      if (!is_hidden) next
      text_nodes <- xml2::xml_find_all(run, ".//w:t", ns)
      if (length(text_nodes) == 0) next
      run_text <- paste(xml2::xml_text(text_nodes), collapse = "")
      markers <- extract_rpfy_markers_from_hidden_runs(run_text)
      if (nrow(markers) == 0) next
      markers$para_idx <- idx
      marker_rows[[length(marker_rows) + 1]] <- markers
    }
  }

  if (length(marker_rows) == 0) {
    return(list(doc = doc, updates = 0L, replacements = data.frame(
      block_id = character(0),
      replacement = character(0),
      stringsAsFactors = FALSE
    )))
  }

  markers <- dplyr::bind_rows(marker_rows)
  starts <- markers |>
    dplyr::filter(marker_type == "block_start" & nzchar(block_id)) |>
    dplyr::arrange(para_idx)
  if (nrow(starts) == 0) {
    return(list(doc = doc, updates = 0L, replacements = data.frame(
      block_id = character(0),
      replacement = character(0),
      stringsAsFactors = FALSE
    )))
  }

  id_to_key <- build_block_id_to_key_map(yml_data)
  updates <- 0L
  replacement_rows <- list()
  used_end_rows_by_id <- list()

  for (i in seq_len(nrow(starts))) {
    block_id <- normalize_doc_scalar(starts$block_id[[i]])
    if (!nzchar(block_id)) next

    start_para <- as.integer(starts$para_idx[[i]])
    next_start_para <- if (i < nrow(starts)) {
      as.integer(starts$para_idx[[i + 1]]) - 1L
    } else {
      length(paragraphs)
    }
    if (!is.finite(next_start_para) || next_start_para < start_para) {
      next_start_para <- start_para
    }

    end_rows <- markers |>
      dplyr::filter(
        block_id == !!block_id,
        marker_type == "block_end",
        para_idx >= start_para,
        para_idx <= next_start_para
      ) |>
      dplyr::arrange(para_idx)

    # Pair each block_start with nearest unused block_end within this start-window.
    used_for_id <- used_end_rows_by_id[[block_id]]
    if (is.null(used_for_id)) {
      used_for_id <- integer(0)
    }

    end_row_idx <- which(!(seq_len(nrow(end_rows)) %in% used_for_id))
    if (length(end_row_idx) > 0) {
      end_pick <- end_row_idx[[1]]
      used_end_rows_by_id[[block_id]] <- c(used_for_id, end_pick)
      end_para <- as.integer(end_rows$para_idx[[end_pick]])
    } else {
      # Fallback when BLOCK_END is missing/malformed:
      # keep block bounded to before next block start, and extend to the
      # furthest same-block marker seen in this window (title/image/footnote).
      same_block_markers <- markers |>
        dplyr::filter(
          block_id == !!block_id,
          para_idx >= start_para,
          para_idx <= next_start_para
        )
      if (nrow(same_block_markers) > 0) {
        end_para <- max(as.integer(same_block_markers$para_idx), na.rm = TRUE)
      } else {
        end_para <- next_start_para
      }
    }

    if (start_para < 1L || start_para > length(paragraphs)) next
    if (end_para < start_para) end_para <- start_para
    if (end_para > length(paragraphs)) end_para <- length(paragraphs)

    block_key <- id_to_key[[block_id]]
    if (is.null(block_key) || !nzchar(block_key)) {
      block_key <- ""
    }
    block_data <- if (nzchar(block_key)) yml_data$blocks[[block_key]] else NULL
    block_status <- if (!is.null(block_data)) normalize_doc_scalar(block_data$status) else ""
    should_skip <- isTRUE(skip_deleted) && identical(block_status, "deleted")

    placeholder <- ""
    if (!should_skip && !is.null(block_data) && is.list(block_data)) {
      placeholder <- build_review_block_placeholder(
        block_key = block_key,
        block_data = block_data,
        mark_missing = mark_missing
      )
      replacement_rows[[length(replacement_rows) + 1]] <- data.frame(
        block_id = block_id,
        replacement = placeholder,
        stringsAsFactors = FALSE
      )
    }

    for (pidx in seq.int(start_para, end_para)) {
      p <- paragraphs[[pidx]]
      if (pidx == start_para) {
        changed <- replace_text_in_paragraph_nodes(p, function(full_text) placeholder)
      } else {
        changed <- replace_text_in_paragraph_nodes(p, function(full_text) "")
      }
      if (isTRUE(changed)) {
        updates <- updates + 1L
      }

      # Remove inline drawing objects in the block range.
      drawing_nodes <- xml2::xml_find_all(p, ".//w:drawing", ns)
      if (length(drawing_nodes) > 0) {
        xml2::xml_remove(drawing_nodes)
      }
    }

    # Remove any table whose paragraphs are fully inside this block range.
    tbl_nodes <- xml2::xml_find_all(body_xml, ".//w:tbl", ns)
    if (length(tbl_nodes) > 0) {
      for (tbl in tbl_nodes) {
        tbl_paras <- xml2::xml_find_all(tbl, ".//w:p", ns)
        if (length(tbl_paras) == 0) next
        para_hits <- which(vapply(paragraphs, function(px) {
          any(vapply(tbl_paras, function(tp) identical(tp, px), logical(1)))
        }, logical(1)))
        if (length(para_hits) == 0) next
        if (all(para_hits >= start_para & para_hits <= end_para)) {
          xml2::xml_remove(tbl)
        }
      }
    }
  }

  replacements_df <- if (length(replacement_rows) > 0) dplyr::bind_rows(replacement_rows) else
    data.frame(block_id = character(0), replacement = character(0), stringsAsFactors = FALSE)

  list(doc = doc, updates = updates, replacements = replacements_df)
}

convert_report_inline_values_to_tags <- function(doc, yml_data, inline_tag_style = "brace") {
  inline_map <- if (!is.null(yml_data$inline) && is.list(yml_data$inline)) yml_data$inline else list()
  if (length(inline_map) == 0) {
    return(list(doc = doc, updates = 0L))
  }
  for (key in names(inline_map)) {
    inline_map[[key]] <- normalize_inline_entry(inline_map[[key]])
  }

  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  paragraphs <- xml2::xml_find_all(body_xml, ".//w:p", ns)
  if (length(paragraphs) == 0) {
    return(list(doc = doc, updates = 0L))
  }

  updates <- 0L
  for (paragraph in paragraphs) {
    runs <- xml2::xml_find_all(paragraph, ".//w:r", ns)
    if (length(runs) == 0) next

    active_ids <- character(0)
    id_nodes <- list()
    paragraph_marker_ids <- character(0)

    for (run in runs) {
      vanish_node <- xml2::xml_find_first(run, "./w:rPr/w:vanish", ns)
      is_hidden <- !inherits(vanish_node, "xml_missing")
      text_nodes <- xml2::xml_find_all(run, ".//w:t", ns)
      if (length(text_nodes) == 0) next
      run_text <- paste(xml2::xml_text(text_nodes), collapse = "")

      if (is_hidden) {
        key_matches <- stringr::str_match_all(
          run_text,
          "RPFY_INLINE_KEY:([^:]+):(.+?)(?=RPFY_|$)"
        )[[1]]
        if (nrow(key_matches) > 0) {
          ids <- key_matches[, 2]
          ids <- trimws(as.character(ids))
          ids <- ids[nzchar(ids)]
          if (length(ids) > 0) {
            paragraph_marker_ids <- c(paragraph_marker_ids, ids)
          }
        }

        tokens <- extract_inline_marker_tokens(run_text)
        if (nrow(tokens) == 0) next
        for (i in seq_len(nrow(tokens))) {
          token <- tokens$token[[i]]
          marker_id <- tokens$inline_id[[i]]
          if (!nzchar(marker_id)) next

          if (identical(token, "start")) {
            active_ids <- c(active_ids, marker_id)
          } else if (identical(token, "end")) {
            matches <- which(active_ids == marker_id)
            if (length(matches) > 0) {
              active_ids <- active_ids[-max(matches)]
            }
          }
        }
        next
      }

      if (length(active_ids) == 0) next
      target_id <- active_ids[[length(active_ids)]]
      if (is.null(id_nodes[[target_id]])) {
        id_nodes[[target_id]] <- text_nodes
      } else {
        id_nodes[[target_id]] <- c(id_nodes[[target_id]], text_nodes)
      }
    }

    if (length(id_nodes) > 0) {
      for (marker_id in names(id_nodes)) {
        key <- resolve_inline_key_from_marker_id(marker_id, inline_map)
        if (!nzchar(key)) next
        tag <- if (identical(inline_tag_style, "text")) {
          paste0("<<text:", key, ">>")
        } else {
          paste0("{{", key, "}}")
        }
  
        nodes <- id_nodes[[marker_id]]
        if (length(nodes) == 0) next
        xml2::xml_text(nodes[[1]]) <- tag
        if (length(nodes) > 1) {
          for (i in 2:length(nodes)) {
            xml2::xml_text(nodes[[i]]) <- ""
          }
        }
        updates <- updates + 1L
      }
    }

    # Fallback for report docs where INLINE markers are appended as hidden text
    # and do not wrap visible runs. In that case, map by marker ID and replace
    # visible inline value (from YAML) with tag in the same paragraph.
    if (length(id_nodes) == 0 && length(paragraph_marker_ids) > 0) {
      marker_ids <- unique(paragraph_marker_ids[nzchar(paragraph_marker_ids)])
      if (length(marker_ids) == 0) next

      changed <- replace_text_in_visible_nodes(paragraph, function(full_text) {
        updated <- full_text
        for (marker_id in marker_ids) {
          key <- resolve_inline_key_from_marker_id(marker_id, inline_map)
          if (!nzchar(key)) next
          entry <- normalize_inline_entry(inline_map[[key]])
          value <- normalize_doc_scalar(entry$value)
          if (!nzchar(value)) next
          tag <- if (identical(inline_tag_style, "text")) {
            paste0("<<text:", key, ">>")
          } else {
            paste0("{{", key, "}}")
          }
          updated <- sub(value, tag, updated, fixed = TRUE)
        }
        updated
      })

      if (isTRUE(changed)) {
        updates <- updates + 1L
      }
    }
  }

  list(doc = doc, updates = updates)
}

remove_hidden_rpfy_runs <- function(doc) {
  body_xml <- officer::docx_body_xml(doc)
  ns <- xml2::xml_ns(body_xml)
  run_nodes <- xml2::xml_find_all(body_xml, ".//w:r", ns)
  if (length(run_nodes) == 0) {
    return(doc)
  }

  for (run in run_nodes) {
    vanish_node <- xml2::xml_find_first(run, "./w:rPr/w:vanish", ns)
    is_hidden <- !inherits(vanish_node, "xml_missing")
    if (!is_hidden) next
    text_nodes <- xml2::xml_find_all(run, ".//w:t", ns)
    if (length(text_nodes) == 0) next
    run_text <- paste(xml2::xml_text(text_nodes), collapse = "")
    if (grepl("RPFY_", run_text, fixed = TRUE)) {
      xml2::xml_remove(run)
    }
  }

  doc
}

generate_magic_doc_from_reviewed_output <- function(yaml_path, reviewed_doc_path, output_doc_path, inline_tag_style = "brace", mark_missing = FALSE) {
  if (!file.exists(yaml_path)) {
    stop(paste0("YAML file not found: ", yaml_path), call. = FALSE)
  }
  if (!file.exists(reviewed_doc_path)) {
    stop(paste0("Reviewed DOCX not found: ", reviewed_doc_path), call. = FALSE)
  }

  cleanup_duplicate_files_keys_in_yaml(yaml_path)
  yml_data <- yaml::read_yaml(yaml_path)
  ensured_blocks <- ensure_block_ids(yml_data)
  yml_data <- ensured_blocks$data
  ensured_inline <- ensure_inline_ids(yml_data)
  yml_data <- ensured_inline$data
  deduped <- resolve_duplicate_block_ids(yml_data)
  yml_data <- deduped$data

  doc <- officer::read_docx(reviewed_doc_path)

  block_result <- convert_report_blocks_to_magic_placeholders(
    doc = doc,
    yml_data = yml_data,
    skip_deleted = TRUE,
    mark_missing = mark_missing
  )
  doc <- block_result$doc

  inline_result <- convert_report_inline_values_to_tags(
    doc = doc,
    yml_data = yml_data,
    inline_tag_style = inline_tag_style
  )
  doc <- inline_result$doc

  doc <- remove_hidden_rpfy_runs(doc)
  doc <- strip_visible_block_id_markers(doc)
  if (isTRUE(mark_missing)) {
    doc <- color_missing_runs_red(doc)
  }

  if (nrow(block_result$replacements) > 0) {
    doc <- add_hidden_ids_to_doc(doc, block_result$replacements)
  }
  doc <- add_hidden_inline_ids_to_doc(doc, yml_data)

  print(doc, target = output_doc_path)

  list(
    yaml_path = yaml_path,
    input_doc = reviewed_doc_path,
    output_doc = output_doc_path,
    block_updates = block_result$updates,
    inline_updates = inline_result$updates
  )
}

apply_magic_doc_replacements <- function(input_doc_path, output_doc_path, tags, yml_data, yml_path = NULL) {
  if (!is.null(yml_path) && nzchar(yml_path) && file.exists(yml_path)) {
    cleanup_duplicate_files_keys_in_yaml(yml_path)
  }
  ensured <- ensure_block_ids(yml_data)
  yml_data <- ensured$data
  ensured_inline <- ensure_inline_ids(yml_data)
  yml_data <- ensured_inline$data
  deduped <- resolve_duplicate_block_ids(yml_data)
  yml_data <- deduped$data
  if ((isTRUE(ensured$changed) || isTRUE(ensured_inline$changed) || isTRUE(deduped$changed)) && !is.null(yml_path) && nzchar(yml_path)) {
    write_mapping_yaml_preserve_format(yml_path, yml_data)
    if (!is.null(yml_data$inline) && is.list(yml_data$inline)) {
      write_inline_sections_preserve_format(yml_path, yml_data$inline)
    }
  }

  replacements <- build_magic_replacements(tags, yml_data)
  
  doc <- read_docx(input_doc_path)

  if (nrow(replacements) > 0) {
    for (i in seq_len(nrow(replacements))) {
      doc <- body_replace_all_text(
        x = doc,
        old_value = replacements$raw[[i]],
        new_value = replacements$replacement[[i]],
        fixed = TRUE
      )
    }
  }

  doc <- replace_split_placeholders(doc, replacements)
  doc <- add_hidden_ids_to_doc(doc, replacements)
  doc <- add_hidden_inline_ids_to_doc(doc, yml_data)
  doc <- color_missing_runs_red(doc)
  print(doc, target = output_doc_path)

  remaining_tags <- extract_tag_pairs_from_doc(output_doc_path, c("<<>>", "{{}}")) |>
    dplyr::filter(!(tag %in% c("inline", "text")))

  list(
    yml_updated = isTRUE(ensured$changed),
    replacements = replacements,
    remaining_non_inline_tags = remaining_tags,
    output = output_doc_path
  )
}
