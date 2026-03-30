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

  lines <- c(paste0("[", ts, "] [", severity, "] ", event))
  if (length(details) > 0) {
    details <- as.character(details)
    details <- details[!is.na(details) & nzchar(trimws(details))]
    if (length(details) > 0) {
      lines <- c(lines, paste0("  - ", details))
    }
  }
  lines <- c(lines, "")
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
