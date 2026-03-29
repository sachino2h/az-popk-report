

# Function to select and format runs for run log:
run_log_report <- function(runs, levels = as.character(runs)) {
  select_runs <- data.frame(runno = runs,
                            run = factor(c(as.character(levels)))) # select runs for report 
  
  log_summary<- run_log(modDir, .recurse = FALSE,
                        .include = runs) %>% 
    filter(model_type != "nmboot") %>%
    add_summary(.bbi_args = list(no_grd_file = T)) %>%
    left_join(select_runs) %>% 
    arrange(run) %>%
    collapse_to_string(tags, notes) %>%
    select(runno, description, ofv, condition_number) %>% 
    mutate(run = factor(runs, levels = levels))%>% 
    arrange(run)
  
  runlog_tab <- log_summary %>% 
    mutate(across(.cols = c(ofv:condition_number), ~round(.x))) %>% 
    select(Run=runno, Structure = description,
           OFV=ofv, CondNo = condition_number)
  return(runlog_tab)
}

