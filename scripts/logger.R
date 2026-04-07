# =============================================================================
# logger.R  —  Report pipeline logging engine
#
# USAGE — wrap around existing pipeline calls:
#
#   source("logger.R")
#
#   # 1. When user triggers "Generate Mapping YML"
#   session <- start_session(paths)
#
#   # 2. After first DOC → YML extraction
#   create_baseline(session, variables)
#
#   # 3. Before each conversion run
#   session <- start_run(session, phase = "doc_to_yml")  # or "yml_to_doc"
#
#   # 4. After detection logic produces events — call once per event
#   session <- log_event(session,
#     type     = "VARIABLE_UPDATED",
#     variable = "cwres_vs_time",
#     metadata = list(field = "title", old = "Figure 1", new = "Figure 1 (v2)")
#   )
#
#   # 5. After all events logged
#   session <- end_run(session, current_variables)
#
# EVENT TYPES:
#   VARIABLE_ADDED    — variable appears in current state, not in snapshot
#   VARIABLE_DELETED  — variable in snapshot, missing from current state
#   VARIABLE_UPDATED  — variable exists in both, value changed
#   VARIABLE_MISSING  — variable expected but not found in doc or yml
# =============================================================================

library(jsonlite)
library(here)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b


# =============================================================================
# 1. start_session()
#    Call when user triggers "Generate Mapping YML".
#    Creates the session folder structure and returns a session object.
# =============================================================================

start_session <- function(paths) {
  timestamp  <- format(Sys.time(), "%Y-%m-%d_%H-%M")
  session_id <- paste0("session_", timestamp)
  
  # Logs live inside the project folder — next to mapping.yaml
  project_dir  <- dirname(paths$mapping_yaml)
  session_dir  <- file.path(project_dir, "logs", session_id)
  archive_dir  <- file.path(session_dir, "archive")
  
  dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  
  session <- list(
    id           = session_id,
    timestamp    = timestamp,
    session_dir  = session_dir,
    archive_dir  = archive_dir,
    changes_file = file.path(session_dir, "CHANGES.txt"),
    current_run  = NULL,   # populated by start_run()
    run_count    = 0L
  )
  
  # Write session header to CHANGES.txt
  header <- paste0(
    strrep("=", 80), "\n",
    "SESSION  ", timestamp, "\n",
    "Report   ", basename(project_dir), "\n",
    strrep("=", 80), "\n"
  )
  writeLines(header, session$changes_file)
  
  message("Logger: session started — ", session_id)
  message("Logger: logs at ", session_dir)
  
  invisible(session)
}


# =============================================================================
# 2. create_baseline()
#    Call after the FIRST DOC → YML extraction of a session.
#    Stores a snapshot of all variables. Logs NOTHING — this is expected state.
#
#    variables: named list — { varname: list(type, status, files, title, footnote) }
# =============================================================================

create_baseline <- function(session, variables) {
  baseline <- list(
    session_id   = session$id,
    captured_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    variable_count = length(variables),
    variables    = variables
  )
  
  # Write baseline.json and snapshot.json (snapshot will be overwritten each run)
  write_json(baseline, file.path(session$archive_dir, "baseline.json"),
             pretty = TRUE, auto_unbox = TRUE)
  write_json(baseline, file.path(session$archive_dir, "snapshot.json"),
             pretty = TRUE, auto_unbox = TRUE)
  
  # Append to CHANGES.txt
  line <- paste0(
    "\nBASELINE  ", format(Sys.time(), "%H:%M"),
    "  (first scan — all variables recorded, no changes logged)\n",
    "  Variables captured: ", length(variables), "\n"
  )
  cat(line, file = session$changes_file, append = TRUE)
  
  message("Logger: baseline created — ", length(variables), " variables")
  
  invisible(session)
}


# =============================================================================
# 3. start_run()
#    Call before each DOC→YML or YML→DOC conversion.
#    Loads the previous snapshot for comparison and opens a new run record.
#
#    phase: "doc_to_yml" | "yml_to_doc"
# =============================================================================

start_run <- function(session, phase) {
  stopifnot(phase %in% c("doc_to_yml", "yml_to_doc"))
  
  session$run_count   <- session$run_count + 1L
  run_time            <- format(Sys.time(), "%H-%M")
  run_id              <- paste0("run_", run_time)
  
  # Load previous snapshot for comparison
  snapshot_path <- file.path(session$archive_dir, "snapshot.json")
  prev_snapshot <- if (file.exists(snapshot_path)) {
    tryCatch(
      fromJSON(snapshot_path, simplifyVector = FALSE),
      error = function(e) { message("Logger: could not load snapshot"); NULL }
    )
  } else NULL
  
  session$current_run <- list(
    run_id       = run_id,
    phase        = phase,
    started_at   = Sys.time(),
    events       = list(),
    compared_to  = if (!is.null(prev_snapshot)) prev_snapshot$run_id %||% "baseline" else "baseline",
    prev_snapshot = prev_snapshot
  )
  
  message("Logger: run started — ", run_id, " [", phase, "]")
  
  invisible(session)
}


# =============================================================================
# 4. log_event()
#    Call once per detected change event.
#    Do NOT change your existing detection logic — just pass its output here.
#
#    type:     "VARIABLE_ADDED" | "VARIABLE_DELETED" | "VARIABLE_UPDATED" | "VARIABLE_MISSING"
#    variable: character — the variable name
#    metadata: named list — any of: field, old_value, new_value
# =============================================================================

log_event <- function(session, type, variable, metadata = list()) {
  stopifnot(!is.null(session$current_run))
  stopifnot(type %in% c("VARIABLE_ADDED", "VARIABLE_DELETED",
                        "VARIABLE_UPDATED", "VARIABLE_MISSING"))
  
  severity <- switch(type,
                     VARIABLE_ADDED   = "INFO",
                     VARIABLE_DELETED = "IMPORTANT",
                     VARIABLE_UPDATED = "WARNING",
                     VARIABLE_MISSING = "WARNING"
  )
  
  message_text <- .build_event_message(type, variable, metadata)
  
  event <- c(
    list(
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
      type      = type,
      severity  = severity,
      variable  = variable,
      message   = message_text
    ),
    metadata
  )
  
  session$current_run$events <- append(session$current_run$events, list(event))
  
  invisible(session)
}


# =============================================================================
# 5. end_run()
#    Call after all events for this run have been logged.
#    Writes run_HH-MM.json to archive, updates snapshot.json,
#    and appends the human-readable summary to CHANGES.txt.
#
#    current_variables: named list — current state of all variables
# =============================================================================

end_run <- function(session, current_variables) {
  stopifnot(!is.null(session$current_run))
  
  run    <- session$current_run
  events <- run$events
  
  # Summarise event counts
  types   <- sapply(events, `[[`, "type")
  summary <- list(
    added   = sum(types == "VARIABLE_ADDED"),
    deleted = sum(types == "VARIABLE_DELETED"),
    updated = sum(types == "VARIABLE_UPDATED"),
    missing = sum(types == "VARIABLE_MISSING"),
    total   = length(events)
  )
  
  # Build run record
  run_record <- list(
    session_id   = session$id,
    run_id       = run$run_id,
    phase        = run$phase,
    started_at   = format(run$started_at, "%Y-%m-%dT%H:%M:%SZ"),
    ended_at     = format(Sys.time(),     "%Y-%m-%dT%H:%M:%SZ"),
    compared_to  = run$compared_to,
    no_op        = (summary$total == 0L),
    summary      = summary,
    events       = events
  )
  
  # Archive: write run JSON
  run_path <- file.path(session$archive_dir, paste0(run$run_id, ".json"))
  write_json(run_record, run_path, pretty = TRUE, auto_unbox = TRUE)
  
  # Archive: overwrite snapshot with current state
  snapshot <- list(
    session_id = session$id,
    run_id     = run$run_id,
    captured_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    variables  = current_variables
  )
  write_json(snapshot,
             file.path(session$archive_dir, "snapshot.json"),
             pretty = TRUE, auto_unbox = TRUE)
  
  # Human log: append to CHANGES.txt
  .append_run_to_changes(session, run, summary, events)
  
  message("Logger: run ended — ", run$run_id,
          " (", summary$total, " event(s))")
  
  # Clear current_run
  session$current_run <- NULL
  
  invisible(session)
}


# =============================================================================
# INTERNAL HELPERS
# =============================================================================

.build_event_message <- function(type, variable, metadata) {
  field     <- metadata$field     %||% ""
  old_value <- metadata$old_value %||% ""
  new_value <- metadata$new_value %||% ""
  
  switch(type,
         VARIABLE_ADDED   = paste0("Added: ", variable,
                                   if (nzchar(field)) paste0(" (", field, ")") else ""),
         VARIABLE_DELETED = paste0("Deleted: ", variable),
         VARIABLE_UPDATED = paste0("Updated: ", variable,
                                   if (nzchar(field)) paste0(" — ", field) else ""),
         VARIABLE_MISSING = paste0("Missing: ", variable,
                                   if (nzchar(field)) paste0(" (", field, ")") else "")
  )
}

.append_run_to_changes <- function(session, run, summary, events) {
  phase_label <- switch(run$phase,
                        doc_to_yml = "DOC \u2192 YML",
                        yml_to_doc = "YML \u2192 DOC"
  )
  
  lines <- c(
    "",
    strrep("\u2500", 80),
    paste0("RUN  ", format(run$started_at, "%H:%M"),
           "  ", phase_label)
  )
  
  if (summary$total == 0L) {
    lines <- c(lines, "  No changes detected.")
  } else {
    for (event in events) {
      type     <- event$type
      variable <- event$variable
      old_v    <- event$old_value %||% ""
      new_v    <- event$new_value %||% ""
      field    <- event$field     %||% ""
      
      prefix <- switch(type,
                       VARIABLE_ADDED   = "  [ADDED]  ",
                       VARIABLE_DELETED = "  [DELETED]",
                       VARIABLE_UPDATED = "  [UPDATED]",
                       VARIABLE_MISSING = "  [MISSING]"
      )
      
      flag <- if (type == "VARIABLE_DELETED") "          *** IMPORTANT ***" else ""
      
      lines <- c(lines, paste0(prefix, "   ", variable, flag))
      
      if (nzchar(field) && nzchar(old_v)) {
        lines <- c(lines,
                   paste0("              ", field, ": \"", old_v, "\""),
                   paste0("           \u2192  ", field, ": \"", new_v, "\"")
        )
      } else if (type == "VARIABLE_DELETED" && nzchar(old_v)) {
        lines <- c(lines, paste0("              last known: ", old_v))
      } else if (type == "VARIABLE_ADDED" && nzchar(new_v)) {
        lines <- c(lines, paste0("              ", new_v))
      }
    }
  }
  
  cat(paste(lines, collapse = "\n"), "\n",
      file = session$changes_file, append = TRUE)
}


# =============================================================================
# UTILITY: write session summary at the very end
#          Call once when the full report build is complete.
# =============================================================================

finalize_session <- function(session) {
  # Count totals across all run JSONs in archive
  run_files <- list.files(session$archive_dir,
                          pattern = "^run_.*\\.json$",
                          full.names = TRUE)
  
  totals <- list(added = 0L, deleted = 0L, updated = 0L, missing = 0L)
  
  for (rf in run_files) {
    tryCatch({
      r <- fromJSON(rf, simplifyVector = FALSE)
      s <- r$summary
      totals$added   <- totals$added   + (s$added   %||% 0L)
      totals$deleted <- totals$deleted + (s$deleted %||% 0L)
      totals$updated <- totals$updated + (s$updated %||% 0L)
      totals$missing <- totals$missing + (s$missing %||% 0L)
    }, error = function(e) NULL)
  }
  
  footer <- paste0(
    "\nSUMMARY  Session total:  ",
    totals$added,   " added  \u00b7  ",
    totals$deleted, " deleted  \u00b7  ",
    totals$updated, " updated  \u00b7  ",
    totals$missing, " missing\n",
    strrep("=", 80), "\n"
  )
  
  cat(footer, file = session$changes_file, append = TRUE)
  
  message("Logger: session finalised — ",
          "CHANGES.txt at ", session$changes_file)
  
  invisible(session)
}


# =============================================================================
# AUTOMATIC YAML AUDIT LOGGER (session-based)
# - New session is created when generate_mapping_yaml is triggered.
# - All subsequent YAML-affecting operations append to that session LOGS.txt.
# =============================================================================

.yaml_audit_state <- new.env(parent = emptyenv())
.yaml_audit_state$session_id <- NULL
.yaml_audit_state$session_dir <- NULL
.yaml_audit_state$log_path <- NULL

start_yaml_audit_session <- function(config, force_new = TRUE) {
  stopifnot(!is.null(config), is.list(config), !is.null(config$paths))
  mapping_yaml <- config$paths$mapping_yaml
  project_dir <- dirname(mapping_yaml)
  logs_root <- file.path(project_dir, "logs")
  dir.create(logs_root, recursive = TRUE, showWarnings = FALSE)

  if (!isTRUE(force_new) && !is.null(.yaml_audit_state$log_path) && file.exists(.yaml_audit_state$log_path)) {
    return(invisible(.yaml_audit_state$log_path))
  }

  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  session_id <- paste0("session_", timestamp)
  session_dir <- file.path(logs_root, session_id)
  dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)

  log_path <- file.path(session_dir, "LOGS.txt")
  header <- c(
    strrep("=", 90),
    paste0("YAML AUDIT SESSION: ", session_id),
    paste0("Started At: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")),
    paste0("YAML Path: ", mapping_yaml),
    strrep("=", 90),
    ""
  )
  writeLines(header, log_path, useBytes = TRUE)

  .yaml_audit_state$session_id <- session_id
  .yaml_audit_state$session_dir <- session_dir
  .yaml_audit_state$log_path <- log_path

  invisible(log_path)
}

get_audit_log_path <- function(config) {
  if (!is.null(config$paths$log_txt) && nzchar(config$paths$log_txt)) {
    dir.create(dirname(config$paths$log_txt), recursive = TRUE, showWarnings = FALSE)
    return(config$paths$log_txt)
  }

  if (is.null(.yaml_audit_state$log_path) || !file.exists(.yaml_audit_state$log_path)) {
    return(start_yaml_audit_session(config, force_new = TRUE))
  }

  .yaml_audit_state$log_path
}

append_audit_log <- function(config, severity = "IMPORTANT", event, details = character(0)) {
  log_path <- get_audit_log_path(config)
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")

  severity <- toupper(as.character(severity %||% "IMPORTANT"))
  if (!nzchar(severity)) severity <- "IMPORTANT"

  sep <- strrep("-", 90)
  lines <- c(
    sep,
    paste0("[", ts, "] [", severity, "] ", event)
  )
  if (length(details) > 0) {
    details <- as.character(details)
    details <- details[!is.na(details) & nzchar(trimws(details))]
    if (length(details) > 0) {
      lines <- c(lines, paste0("  - ", details))
    }
  }
  lines <- c(lines, sep, "")
  cat(paste(lines, collapse = "\n"), file = log_path, append = TRUE)
  invisible(log_path)
}

read_yaml_snapshot <- function(yaml_path) {
  if (is.null(yaml_path) || !nzchar(yaml_path) || !file.exists(yaml_path)) {
    return(list(inline = list(), blocks = list()))
  }

  data <- tryCatch(yaml::read_yaml(yaml_path), error = function(e) NULL)
  if (is.null(data) || !is.list(data)) {
    return(list(inline = list(), blocks = list()))
  }

  inline <- list()
  if (!is.null(data$inline) && is.list(data$inline)) {
    for (k in names(data$inline)) {
      entry <- data$inline[[k]]
      inline[[k]] <- list(
        id = as.character(entry$id %||% ""),
        type = as.character(entry$type %||% ""),
        status = as.character(entry$status %||% ""),
        value = as.character(entry$value %||% "")
      )
    }
  }

  blocks <- list()
  if (!is.null(data$blocks) && is.list(data$blocks)) {
    for (k in names(data$blocks)) {
      b <- data$blocks[[k]]
      files_val <- b$files
      files_txt <- ""
      if (!is.null(files_val) && length(files_val) > 0) {
        files_txt <- paste(trimws(as.character(unlist(files_val, use.names = FALSE))), collapse = ",")
      }
      blocks[[k]] <- list(
        id = as.character(b$id %||% ""),
        type = as.character(b$type %||% ""),
        title = as.character(b$title %||% ""),
        footnote = as.character(b$footnote %||% ""),
        status = as.character(b$status %||% ""),
        files = files_txt
      )
    }
  }

  list(inline = inline, blocks = blocks)
}

build_yaml_change_details <- function(before, after, context = "") {
  before_inline_keys <- names(before$inline %||% list())
  after_inline_keys <- names(after$inline %||% list())
  before_block_keys <- names(before$blocks %||% list())
  after_block_keys <- names(after$blocks %||% list())

  inline_added <- setdiff(after_inline_keys, before_inline_keys)
  inline_deleted <- setdiff(before_inline_keys, after_inline_keys)
  block_added <- setdiff(after_block_keys, before_block_keys)
  block_deleted <- setdiff(before_block_keys, after_block_keys)

  common_inline <- intersect(before_inline_keys, after_inline_keys)
  common_blocks <- intersect(before_block_keys, after_block_keys)

  inline_updated <- 0L
  for (k in common_inline) {
    if (!identical(before$inline[[k]], after$inline[[k]])) inline_updated <- inline_updated + 1L
  }

  block_updated <- 0L
  for (k in common_blocks) {
    if (!identical(before$blocks[[k]], after$blocks[[k]])) block_updated <- block_updated + 1L
  }

  details <- c(
    if (nzchar(context)) paste0("context=", context),
    paste0("blocks_added=", length(block_added)),
    paste0("blocks_deleted=", length(block_deleted)),
    paste0("blocks_updated=", block_updated),
    paste0("inline_added=", length(inline_added)),
    paste0("inline_deleted=", length(inline_deleted)),
    paste0("inline_updated=", inline_updated)
  )

  if (length(block_added) > 0) {
    details <- c(details, paste0("added_blocks=", paste(block_added, collapse = ", ")))
    details <- c(details, "note=Variable added in template and then added to YAML")
  }
  if (length(block_deleted) > 0) {
    details <- c(details, paste0("deleted_blocks=", paste(block_deleted, collapse = ", ")))
    details <- c(details, "note=Variable deleted from YAML")
  }
  if (length(inline_deleted) > 0) {
    details <- c(details, paste0("deleted_inline=", paste(inline_deleted, collapse = ", ")))
  }

  details
}

collect_yaml_field_changes <- function(before, after, max_items = 40L) {
  changes <- character(0)

  before_inline <- before$inline %||% list()
  after_inline <- after$inline %||% list()
  common_inline <- intersect(names(before_inline), names(after_inline))
  for (k in common_inline) {
    b <- before_inline[[k]] %||% list()
    a <- after_inline[[k]] %||% list()
    fields <- union(names(b), names(a))
    fields <- fields[fields %in% c("id", "type", "status", "value")]
    for (f in fields) {
      old_v <- as.character(b[[f]] %||% "")
      new_v <- as.character(a[[f]] %||% "")
      if (!identical(old_v, new_v)) {
        changes <- c(
          changes,
          paste0("inline.", k, ".", f, ": '", old_v, "' -> '", new_v, "'")
        )
      }
    }
  }

  before_blocks <- before$blocks %||% list()
  after_blocks <- after$blocks %||% list()
  common_blocks <- intersect(names(before_blocks), names(after_blocks))
  for (k in common_blocks) {
    b <- before_blocks[[k]] %||% list()
    a <- after_blocks[[k]] %||% list()
    fields <- union(names(b), names(a))
    fields <- fields[fields %in% c("id", "type", "title", "footnote", "status", "files")]
    for (f in fields) {
      old_v <- as.character(b[[f]] %||% "")
      new_v <- as.character(a[[f]] %||% "")
      if (!identical(old_v, new_v)) {
        changes <- c(
          changes,
          paste0("blocks.", k, ".", f, ": '", old_v, "' -> '", new_v, "'")
        )
      }
    }
  }

  if (length(changes) > max_items) {
    hidden_n <- length(changes) - max_items
    changes <- c(changes[seq_len(max_items)], paste0("... and ", hidden_n, " more field change(s)"))
  }

  changes
}

emit_yaml_change_audit <- function(config, event, before, after, context = "") {
  before_inline_keys <- names(before$inline %||% list())
  after_inline_keys <- names(after$inline %||% list())
  before_block_keys <- names(before$blocks %||% list())
  after_block_keys <- names(after$blocks %||% list())

  block_added <- setdiff(after_block_keys, before_block_keys)
  block_deleted <- setdiff(before_block_keys, after_block_keys)
  inline_deleted <- setdiff(before_inline_keys, after_inline_keys)

  append_audit_log(
    config = config,
    severity = "IMPORTANT",
    event = event,
    details = build_yaml_change_details(before, after, context = context)
  )

  if (length(block_added) > 0) {
    append_audit_log(
      config = config,
      severity = "IMPORTANT",
      event = "Variable added in template and then added to YAML",
      details = c(
        if (nzchar(context)) paste0("context=", context),
        paste0("keys=", paste(block_added, collapse = ", "))
      )
    )
  }

  if (length(block_deleted) > 0) {
    append_audit_log(
      config = config,
      severity = "WARNING",
      event = "Variable deleted from YAML",
      details = c(
        if (nzchar(context)) paste0("context=", context),
        paste0("keys=", paste(block_deleted, collapse = ", "))
      )
    )
  }

  if (length(inline_deleted) > 0) {
    append_audit_log(
      config = config,
      severity = "WARNING",
      event = "Inline variable deleted from YAML",
      details = c(
        if (nzchar(context)) paste0("context=", context),
        paste0("keys=", paste(inline_deleted, collapse = ", "))
      )
    )
  }

  field_changes <- collect_yaml_field_changes(before, after)
  if (length(field_changes) > 0) {
    append_audit_log(
      config = config,
      severity = "IMPORTANT",
      event = "YAML field-level updates",
      details = c(
        if (nzchar(context)) paste0("context=", context),
        field_changes
      )
    )
  }
}
# ======================== my code below =======================
# =============================================================================
# ASSET MANIFEST TRACKING
# Track file modification times when assets are inserted into reports.
# Enables detection of stale reports (source files updated after build).
#
# IMPORTANT: We track ORIGINAL source files (from config$paths$source_figure_dirs
# and config$paths$source_table_dirs), NOT the staged copies in temp_assets/
# =============================================================================

#' Create Asset Manifest
#' 
#' Reads YAML mapping file, extracts all file references from blocks,
#' finds them in ORIGINAL source directories, and records their modification times.
#'
#' @param yaml_path Path to mapping.yaml
#' @param source_figure_dirs Vector of original figure source directories
#' @param source_table_dirs Vector of original table source directories
#' @param report_version Version string (e.g., "v002")
#' @param docx_out Path to the output docx file
#'
#' @return List containing manifest data
create_asset_manifest <- function(yaml_path, 
                                   source_figure_dirs, 
                                   source_table_dirs, 
                                   report_version, 
                                   docx_out) {
  
 build_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  
  # Read YAML
  yaml_data <- tryCatch(
    yaml::read_yaml(yaml_path),
    error = function(e) {
      warning("Could not read YAML for manifest: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(yaml_data) || is.null(yaml_data$blocks)) {
    return(list(
      report_version = report_version,
      build_time = build_time,
      docx_out = docx_out,
      source_figure_dirs = source_figure_dirs,
      source_table_dirs = source_table_dirs,
      assets = list(),
      asset_count = 0L
    ))
  }
  
  assets <- list()
  
  # Iterate through all blocks
  for (var_name in names(yaml_data$blocks)) {
    block <- yaml_data$blocks[[var_name]]
    files_list <- block$files
    
    # Skip if no files
    if (is.null(files_list) || length(files_list) == 0) next
    
    # Handle each file in the files array
    for (file_name in files_list) {
      if (is.null(file_name) || !nzchar(trimws(file_name))) next
      
      file_name <- trimws(file_name)
      
      # Find file in ORIGINAL source directories (not staged)
      file_info <- .find_original_asset_file(
        file_name, 
        source_figure_dirs, 
        source_table_dirs
      )
      
      asset_entry <- list(
        variable_name = var_name,
        variable_id = block$id %||% "",
        variable_type = block$type %||% "",
        file_name = file_name,
        original_path = file_info$path,
        source_dir = file_info$source_dir,
        file_exists = file_info$exists,
        file_mtime = file_info$mtime,
        inserted_at = build_time
      )
      
      assets <- append(assets, list(asset_entry))
    }
  }
  
  manifest <- list(
    report_version = report_version,
    build_time = build_time,
    docx_out = normalizePath(docx_out, mustWork = FALSE),
    source_figure_dirs = source_figure_dirs,
    source_table_dirs = source_table_dirs,
    assets = assets,
    asset_count = length(assets)
  )
  
  manifest
}


#' Find Asset File in Original Source Directories
#' 
#' Searches through all source directories (figures and tables) to find the file.
#' Returns the ORIGINAL path, not staged copy.
#'
#' @param file_name Name of the file to find
#' @param source_figure_dirs Vector of figure source directories
#' @param source_table_dirs Vector of table source directories
#'
#' @return List with path, source_dir, exists, mtime
.find_original_asset_file <- function(file_name, source_figure_dirs, source_table_dirs) {
  
  # Combine all source directories
  all_source_dirs <- c(source_figure_dirs, source_table_dirs)
  all_source_dirs <- all_source_dirs[dir.exists(all_source_dirs)]
  
  # Search in each source directory (including subdirectories)
  for (src_dir in all_source_dirs) {
    # List all files recursively
    all_files <- list.files(
      path = src_dir,
      pattern = paste0("^", gsub("([.\\\\|()[{^$*+?])", "\\\\\\1", file_name), "$"),
      full.names = TRUE,
      recursive = TRUE
    )
    
    if (length(all_files) > 0) {
      # Found the file
      found_path <- all_files[1]
      return(list(
        path = normalizePath(found_path),
        source_dir = normalizePath(src_dir),
        exists = TRUE,
        mtime = format(file.mtime(found_path), "%Y-%m-%dT%H:%M:%S%z")
      ))
    }
  }
  
  # File not found in any source directory
  list(
    path = NA_character_,
    source_dir = NA_character_,
    exists = FALSE,
    mtime = NA_character_
  )
}


#' Save Asset Manifest to JSON
#' 
#' Saves manifest next to the report file.
#'
#' @param manifest Manifest list from create_asset_manifest()
#' @param docx_out Path to report docx (manifest saved alongside)
#'
#' @return Path to saved manifest file
save_asset_manifest <- function(manifest, docx_out) {
  
  # Generate manifest path: report_v002.docx -> report_v002_manifest.json
  docx_dir <- dirname(docx_out)
  docx_base <- tools::file_path_sans_ext(basename(docx_out))
  manifest_path <- file.path(docx_dir, paste0(docx_base, "_manifest.json"))
  
  tryCatch({
    jsonlite::write_json(
      manifest, 
      manifest_path, 
      pretty = TRUE, 
      auto_unbox = TRUE
    )
    message("Logger: asset manifest saved — ", manifest_path)
    message("Logger: ", manifest$asset_count, " asset(s) tracked from original sources")
  }, error = function(e) {
    warning("Could not save asset manifest: ", e$message)
  })
  
  invisible(manifest_path)
}


#' Check Report Freshness
#' 
#' Compares current file modification times of ORIGINAL source files
#' against recorded times in manifest.
#' Identifies stale files (updated after report was built).
#'
#' @param manifest_path Path to manifest JSON file
#'
#' @return List with freshness status and details
check_report_freshness <- function(manifest_path) {
  
  checked_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  
  # Read manifest
  if (!file.exists(manifest_path)) {
    return(list(
      success = FALSE,
      error = "Manifest file not found",
      manifest_path = manifest_path,
      checked_at = checked_at
    ))
  }
  
  manifest <- tryCatch(
    jsonlite::fromJSON(manifest_path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(manifest)) {
    return(list(
      success = FALSE,
      error = "Could not parse manifest JSON",
      manifest_path = manifest_path,
      checked_at = checked_at
    ))
  }
  
  assets <- manifest$assets %||% list()
  
  stale_files <- list()
  fresh_files <- list()
  missing_files <- list()
  
  for (asset in assets) {
    original_path <- asset$original_path
    file_name <- asset$file_name
    variable_name <- asset$variable_name
    recorded_mtime <- asset$file_mtime
    
    # Check if file exists now
    if (is.na(original_path) || !file.exists(original_path)) {
      missing_files <- append(missing_files, list(list(
        variable_name = variable_name,
        file_name = file_name,
        original_path = original_path %||% NA_character_,
        status = "MISSING"
      )))
      next
    }
    
    # Get current mtime of ORIGINAL file
    current_mtime <- format(file.mtime(original_path), "%Y-%m-%dT%H:%M:%S%z")
    
    # Compare times
    recorded_time <- as.POSIXct(recorded_mtime, format = "%Y-%m-%dT%H:%M:%S%z")
    current_time <- as.POSIXct(current_mtime, format = "%Y-%m-%dT%H:%M:%S%z")
    
    if (current_time > recorded_time) {
      # Original file is newer than what we inserted — STALE!
      stale_files <- append(stale_files, list(list(
        variable_name = variable_name,
        file_name = file_name,
        original_path = original_path,
        recorded_mtime = recorded_mtime,
        current_mtime = current_mtime,
        status = "STALE"
      )))
    } else {
      fresh_files <- append(fresh_files, list(list(
        variable_name = variable_name,
        file_name = file_name,
        original_path = original_path,
        recorded_mtime = recorded_mtime,
        current_mtime = current_mtime,
        status = "FRESH"
      )))
    }
  }
  
  is_fresh <- (length(stale_files) == 0) && (length(missing_files) == 0)
  
  result <- list(
    success = TRUE,
    is_fresh = is_fresh,
    report_version = manifest$report_version,
    report_build_time = manifest$build_time,
    docx_out = manifest$docx_out,
    checked_at = checked_at,
    summary = list(
      total_assets = length(assets),
      fresh_count = length(fresh_files),
      stale_count = length(stale_files),
      missing_count = length(missing_files)
    ),
    stale_files = stale_files,
    missing_files = missing_files,
    fresh_files = fresh_files
  )
  
  # Print summary to console
  .print_freshness_summary(result)
  
  result
}


#' Print Freshness Summary
#' 
#' @param result Result from check_report_freshness()
.print_freshness_summary <- function(result) {
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("REPORT FRESHNESS CHECK\n")
  cat(strrep("=", 70), "\n")
  cat("Report:       ", basename(result$docx_out %||% ""), "\n")
  cat("Built at:     ", result$report_build_time, "\n")
  cat("Checked at:   ", result$checked_at, "\n")
  cat(strrep("-", 70), "\n")
  
  s <- result$summary
  cat(sprintf("Total assets: %d\n", s$total_assets))
  cat(sprintf("  Fresh:      %d\n", s$fresh_count))
  cat(sprintf("  Stale:      %d\n", s$stale_count))
  cat(sprintf("  Missing:    %d\n", s$missing_count))
  cat(strrep("-", 70), "\n")
  
  if (result$is_fresh) {
    cat("STATUS: ✓ REPORT IS FRESH\n")
  } else {
    cat("STATUS: ✗ REPORT IS OUTDATED\n")
    
    if (length(result$stale_files) > 0) {
      cat("\nSTALE FILES (original source updated after build):\n")
      for (sf in result$stale_files) {
        cat(sprintf("  • [%s] %s\n", sf$variable_name, sf$file_name))
        cat(sprintf("      source: %s\n", sf$original_path))
        cat(sprintf("      at build:  %s\n", sf$recorded_mtime))
        cat(sprintf("      now:       %s  ← NEWER\n", sf$current_mtime))
      }
    }
    
    if (length(result$missing_files) > 0) {
      cat("\nMISSING FILES (not found in source directories):\n")
      for (mf in result$missing_files) {
        cat(sprintf("  • [%s] %s\n", mf$variable_name, mf$file_name))
      }
    }
    
    cat("\n→ Recommendation: Re-run build_report_extended() to update the report.\n")
  }
  
  cat(strrep("=", 70), "\n\n")
}


#' Build Report Extended With Audit (UPDATED)
#' 
#' Wrapper around azreportifyr::build_report_extended that:
#' 1. Creates asset manifest tracking ORIGINAL source files
#' 2. Calls the actual build
#' 3. Saves manifest next to report
#' 4. Logs to audit log
#'
build_report_extended_with_audit <- function(
  config,
  docx_in,
  docx_out,
  figures_path,
  tables_path,
  yaml_in,
  config_yaml,
  version,
  versions_root,
  style_file = NULL,
  docx_table_style = NULL
) {
  
  # --- 1. Create asset manifest tracking ORIGINAL sources ---
  version_str <- sprintf("v%03d", as.integer(version))
  
  manifest <- create_asset_manifest(
    yaml_path = yaml_in,
    source_figure_dirs = config$paths$source_figure_dirs,
    source_table_dirs = config$paths$source_table_dirs,
    report_version = version_str,
    docx_out = docx_out
  )
  
  # Log asset info to audit log
  if (manifest$asset_count > 0) {
    asset_details <- vapply(manifest$assets, function(a) {
      if (isTRUE(a$file_exists)) {
        sprintf("%s: %s (mtime: %s)", 
                a$variable_name, 
                a$file_name, 
                a$file_mtime)
      } else {
        sprintf("%s: %s (NOT FOUND in sources)", 
                a$variable_name, 
                a$file_name)
      }
    }, character(1))
    
    append_audit_log(
      config = config,
      severity = "INFO",
      event = "Asset manifest created (tracking original sources)",
      details = c(
        paste0("asset_count=", manifest$asset_count),
        paste0("source_figure_dirs=", paste(config$paths$source_figure_dirs, collapse = ", ")),
        paste0("source_table_dirs=", paste(config$paths$source_table_dirs, collapse = ", ")),
        asset_details
      )
    )
  }
  
  # --- 2. Call actual build ---
  out <- azreportifyr::build_report_extended(
    docx_in = docx_in,
    docx_out = docx_out,
    figures_path = figures_path,
    tables_path = tables_path,
    yaml_in = yaml_in,
    config_yaml = config_yaml,
    version = version,
    versions_root = versions_root,
    docx_table_style = docx_table_style
  )
  
  # --- 3. Save manifest next to report ---
  manifest_path <- save_asset_manifest(manifest, docx_out)
  
  # --- 4. Log completion ---
  append_audit_log(
    config = config,
    severity = "IMPORTANT",
    event = "build_report_extended completed",
    details = c(
      paste0("version=", version),
      paste0("docx_out=", docx_out),
      paste0("versions_root=", versions_root),
      paste0("assets_tracked=", manifest$asset_count),
      paste0("manifest_path=", manifest_path)
    )
  )
  
  # Return both the build output and manifest path
  list(
    build_result = out,
    manifest_path = manifest_path,
    manifest = manifest
  )
}