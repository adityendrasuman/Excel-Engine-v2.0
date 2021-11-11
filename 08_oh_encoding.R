# cleanup the environment ----
rm(list = ls())
if (!is.null(dev.list())) dev.off()
cat("\014")
start_time <- Sys.time()

# capture variable coming from vba ----
args <- commandArgs(trailingOnly=T)

# set working director ---- 
setwd(do.call(file.path, as.list(strsplit(args[1], "\\|")[[1]])))

# load environment ----
load("env.RData")
load("dt_01_A.Rda")

# load libraries ----
error = f_libraries(
  necessary.std = c("glue", "dplyr", "stringr", "purrr", "utils"),
  necessary.github = c()
)
glue::glue("RUNNING R SERVER ...") %>% print()
glue::glue("Package status: {error}") %>% print()
glue::glue("\n") %>% print()

# Log of run ----
glue::glue("===================== Running '08_oh_encoding.R' =====================") %>% f_log_string(g_file_log)
glue::glue("This code broke a list of 'Live-Capture' columns provided in the excel interface into constituent columns with Yes/No values") %>% f_log_string(g_file_log)
glue::glue("\n") %>% f_log_string(g_file_log)

#====================================================

make_col_names <- function(vector) {
  purrr::map_chr(vector, function(x) {
    x %>%
      tolower() %>%
      str_replace_all("'", "") %>%
      str_replace_all(",", "") %>%
      str_replace_all("\\.", "") %>%
      str_replace_all("\\(", "") %>%
      str_replace_all("\\)", "") %>%
      str_replace_all("\\\\", "_") %>%
      str_replace_all("\\/", "_") %>%
      str_replace_all("-", "_") %>%
      str_replace_all("\\:", "") %>%
      str_replace_all("\\s+", " ") %>%
      str_replace_all(" ", "_") %>%
      str_replace_all("_+", "_") %>%
      trimws() %>% 
      stringr::str_trim()
  })
}

dt_01_B <- dt_01_A %>% 
  mutate(temp_id = row_number())

glue::glue("Importing 'Live Capture' column names from the excel interface...") %>% print()

col_list <- f_read_xl(g_file_path, namedRegion = "body_OHE_columns", colNames = F) 

if (is.null(nrow(col_list))) {
  
  glue::glue("No column names found for OH Encoding. Either use a Regex pattern to create them or enter them manually in the excel interface") %>% f_log_string(g_file_log)
  glue::glue("If no 'Live Capture' columns need conversion, then click 'skip' and move to the next step") %>% f_log_string(g_file_log)
  
} else {
  
  col_list <- col_list %>% 
    filter_all(any_vars(!is.na(.)))

  # Get column names and questions for OHE
  columns <- list()
  questions <- list()
  
  for (i in 1:nrow(col_list)){
    
    columns <- dt_01_B %>% 
      select(matches(col_list[i, 2])) %>% 
      colnames() %>% 
      c(columns) %>% 
      unique()
    
    questions <- dt_01_B %>% 
      select(matches(col_list[i, 2])) %>% 
      colnames() %>% 
      stringr::str_extract(col_list[i, 1]) %>% 
      c(questions) %>% 
      unique()
    
  }
  
  # dataframe to hold new column names
  summary2 <- matrix(ncol=2,nrow=0) %>% 
    data.frame() %>% 
    select(column_category = 1, new_column = 2)
  
  summary3 <- matrix(ncol=2,nrow=0) %>% 
    data.frame() %>% 
    select(column_category = 1, column = 2)
  
  # Check if regex is identifying one column under one question uniquely 
  for (q in questions) {
    
    summary3 <- summary3 %>% 
      rbind(dt_01_B %>%
              select(matches(paste0("^.*", q, ".*$"))) %>%
              colnames() %>% 
              intersect(columns) %>% 
              unlist() %>% 
              as.data.frame() %>% 
              rename(column = 1) %>% 
              mutate(column_category = q) %>% 
              select(2, 1))
  }
  
  summary3_temp <- summary3 %>% 
    select(2) %>% 
    group_by_all() %>% 
    count() %>% 
    as.data.frame() %>% 
    filter(n > 1)
  
  if (nrow(summary3_temp) > 0){
    
    summary3 %>% 
      right_join(summary3_temp, by = "column") %>% 
      f_log_table("Live capture columns captured under multiple groups", g_file_log)
    
    glue::glue("CRITICAL ERROR: Regex needs to be refined. It currently identifies same column under multiple question grouping") %>% f_log_string(g_file_log)
    glue::glue("Please see log file for more information") %>% f_log_string(g_file_log)

  } else {
    
    print(glue::glue("creating one-hot encoding..."))
    counter <- 0
    pb <- utils::txtProgressBar(min = 0, max = length(questions), style = 3, width = 50)
    
    # For each question category
    for (q in questions) {
      
      # create a variable temp_all_values in the main file that combines values from all relevant variables
    
      list_of_columns <- dt_01_B %>%
        select(matches(paste0("^.*", q, ".*$"))) %>%
        colnames() %>% 
        intersect(columns) %>% 
        unlist()
      
      table_with_relevant_cols <- dt_01_B %>%
        select(temp_id, all_of(list_of_columns)) %>%
        mutate(temp_all_values = "")
      
      # For each column within this question  
      for (i in 1:length(list_of_columns)){
        table_with_relevant_cols <- table_with_relevant_cols %>%
          mutate(temp_all_values = paste0(temp_all_values, 
                                          ifelse(is.na(.[, i+1]) | .[, i+1] == "", 
                                                 "", 
                                                 paste0("|", make_col_names(.[, i+1])))))
        # f_progress(i/length(list_of_columns), counter/length(questions))
      }
      
      table_with_relevant_cols <- table_with_relevant_cols %>%
        select(temp_id, temp_all_values)
    
      dt_01_B <- dt_01_B %>%
        left_join(table_with_relevant_cols, by = "temp_id")
    
      column_values <- dt_01_B %>%
        select(all_of(list_of_columns)) %>%
        unlist() %>%
        table() %>%
        data.frame() %>%
        select("value" = 1, "freq" = 2) %>%
        filter(!is.na(value)) %>%
        filter(value != "") %>%
        filter(value != "{0}") %>%
        mutate(value_colnames = make_col_names(value)) %>%
        mutate(value = as.character(value))
    
      num_column_values <- column_values %>%
        nrow()
      
      len <- nchar(q)
      last <- substr(q, len, len)
      last2 <- substr(q, len - 1, len)
      start <- ifelse(last == "_", "o", ifelse(last2 == "_o" | last2 == "_O", "", "_o"))
      
      # For each response to the question
      for (j in 1:num_column_values){
        
        column_new <- paste0("z_", q, start, j, "_", column_values[j, "value_colnames"]) %>%
          tolower() %>% 
          rlang::sym()
    
        search_term = column_values[j, "value_colnames"]
    
        dt_01_B <- dt_01_B %>%
          mutate(!!column_new := case_when(
            stringr::str_detect(temp_all_values, search_term) ~ "Yes",
            stringr::str_detect(temp_all_values, search_term, negate = T) ~ "No",
            stringr::str_detect(temp_all_values, "^\\|*$") ~ NA_character_,
            TRUE ~ "Check OHE!"
          )
        )
    
        dt_01_B %>%
          select(all_of(column_new), all_of(list_of_columns)) %>%
          group_by_all() %>% 
          count() %>% 
          as.data.frame() %>% 
          f_log_table(paste0("OH encoding for: ", as.character(column_new)), g_file_log)
        
        new_df <- data.frame(q, as.character(column_new))
        names(new_df) <- names(summary2)
        
        summary2 <- summary2 %>% 
          rbind(new_df)
      }
      
      counter = counter + 1
      utils::setTxtProgressBar(pb, counter)
      
      dt_01_B <- dt_01_B %>%
        select(-temp_all_values) 
        # %>% select(-all_of(list_of_columns))
    }
    
    # End of question category loop
    
    dt_01_B <- dt_01_B %>%
      select(-temp_id)
    
    summary2 %>% 
      write.table(file = file.path("temp.csv"), sep=",", col.names = F, row.names = F)
    
    close(pb)
  }
}

#====================================================

# Log of run ----
glue::glue("\n") %>% f_log_string(g_file_log)
glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs") %>% f_log_string(g_file_log)
glue::glue("\n\n") %>% f_log_string(g_file_log)

# Save relevant datasets ----
save(dt_01_B, file = "dt_01_B.Rda")

# remove unnecessary variables from environment ----
rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))

# save environment in a session temp variable ----
save.image(file=file.path(g_wd, "env.RData"))

# Close the R code
print(glue::glue("\n\nAll done!"))
for(i in 1:3){
  print(glue::glue("Finishing in: {4 - i} sec"))
  Sys.sleep(1)
}