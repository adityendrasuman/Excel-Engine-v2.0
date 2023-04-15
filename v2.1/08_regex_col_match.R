tryCatch(
  {
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
    load("dt_01_B.Rda")
    
    # load libraries ----
    error = f_libraries(
      necessary.std = c("glue", "dplyr", "stringr", "purrr"),
      necessary.github = c()
    )
    glue::glue("RUNNING R SERVER ...") %>% print()
    glue::glue("Package status: {error}") %>% print()
    glue::glue("\n") %>% print()
    
    # Code specific inputs ----
    purpose <- "Identifying list of 'MULTISELECT' questions based on the regex input"
    
    code_full <- scriptName::current_filename()
    code_path <- ifelse(is.null(code_full), "", dirname(code_full)) 
    code_name <- ifelse(is.null(code_full), "", basename(code_full))
    
    # Log of run ----
    glue::glue("===================== Running '{code_name}' =====================") %>% f_log_string(g_file_log) 
    glue::glue("{purpose}")%>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)
    
    # Log of run ----
    glue::glue("===================== Running '07_regex_col_match.R' =====================") %>% f_log_string(g_file_log)
    glue::glue("This code identifies list of columns corresponding to the regex input for 'Live Capture'") %>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)
    
    #====================================================
    
    print(glue::glue("Getting user input on Regex strings"))
    
    if (args[3] == ""){args[3] = ".+"}
    
    if (args[2] == "") {
      
      glue::glue("Incomplete user input found for the 'Live Capture' columns. Please provide reg-ex inputs and retry.") %>% f_log_string(g_file_log)
      
    } else {
      
      # Get column names and questions for OHE
      columns <- dt_01_B %>% 
        select(matches(args[3])) %>% 
        colnames() %>% 
        unique() %>% 
        na.omit() %>% 
        as.list()
      
      questions <- dt_01_B %>% 
        select(matches(args[3])) %>% 
        colnames() %>% 
        stringr::str_extract(args[2]) %>% 
        unique() %>% 
        na.omit() %>% 
        as.list()
      
      summary <- matrix(ncol=2,nrow=0) %>% 
        data.frame() %>% 
        select(column_category = 1, column = 2)
      
      # Check if regex is identifying one column under one question uniquely 
      for (q in questions) {
        
        summary <- summary %>% 
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
      
      summary_temp <- summary %>% 
        select(2) %>% 
        group_by_all() %>% 
        count() %>% 
        as.data.frame() %>% 
        filter(n > 1)
      
      if (nrow(summary_temp) > 0){
        
        glue::glue("CRITICAL ERROR: Regex needs to be refined. It currently identifies same column under various groupings") %>% f_log_string(g_file_log)
        glue::glue("Such cases are highlighted in red cells in the interface") %>% f_log_string(g_file_log)
        
      } else {
        
        glue::glue("{length(question)} questions identified for conversion into OH Encoding") %>% f_log_string(g_file_log)
        glue::glue("SUCCESS: Regex could map columns uniquely - i.e. each 'Live Capture' column will be combined in only one question") %>% f_log_string(g_file_log)
        
      }
      
      summary %>% 
        write.table(file = file.path("temp.csv"), sep=",", col.names = F, row.names = F)
    }
    
    #====================================================
    
    # Log of run ----
    glue::glue("\n") %>% f_log_string(g_file_log)
    glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs. Saving the analysis environment") %>% f_log_string(g_file_log)
    glue::glue("\n\n") %>% f_log_string(g_file_log)
    
    # clean and save environment in a session temp variable ----
    rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))
    save.image(file=file.path(g_wd, "env.RData"))
    
    # Close the R code
    print(glue::glue("\n\nAll done!"))
    for(i in 1:3){
      print(glue::glue("Finishing in: {4 - i} sec"))
      Sys.sleep(1)
    }
  }, 
  
  warning = function(x){
    msg = glue::glue("{toString(x)}\n\ncheck code '{code_full}'")
    tcltk::tk_messageBox(type = c("ok"), msg, caption = "WARNING!", default = "", icon = "warning")
  },
  
  error = function(x){
    msg = glue::glue("{toString(x)}\n\ncheck code '{code_full}'")
    tcltk::tk_messageBox(type = c("ok"), msg, caption = "ERROR!", default = "", icon = "error")
  }
)