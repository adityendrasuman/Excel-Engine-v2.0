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
    
    # load libraries ----
    error = f_libraries(
      necessary.std = c("openxlsx", "dplyr", "glue"),
      necessary.github = c()
    )
    glue::glue("RUNNING R SERVER ...") %>% print()
    glue::glue("Package status: {error}") %>% print()
    glue::glue("\n") %>% print()
    
    # Code specific inputs ----
    purpose <- "Uploading raw data into R environment"
    
    code_full <- scriptName::current_filename()
    code_path <- ifelse(is.null(code_full), "", dirname(code_full)) 
    code_name <- ifelse(is.null(code_full), "", basename(code_full))
    
    # Log of run ----
    glue::glue("===================== Running '{code_name}' =====================") %>% f_log_string(g_file_log) 
    glue::glue("{purpose}")%>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)
    
    #====================================================
    
    csv_loc <- do.call(file.path, as.list(strsplit(args[2], "\\|")[[1]]))
    glue::glue("Importing raw data from the selected CSV file...") %>% f_log_string(g_file_log)
    dt_01 <- read.csv(csv_loc)
    glue::glue("Imported data has {ncol(dt_01)} columns and {nrow(dt_01)} rows") %>% f_log_string(g_file_log)
    
    # Output unique set of values for each column, upto 50
    df_unique_val <- data.frame(matrix(ncol=50, nrow=0))
    for (i in 1:50){
      colnames(df_unique_val)[i] <- paste0("V", i)
    }

    glue::glue("Creating table of unique values in the data") %>% f_log_string(g_file_log)
    pb <- txtProgressBar(min = 0, max = ncol(dt_01), style = 3, width = 40)
    i = 0
    for (col in colnames(dt_01)){
      
      unique_val <- dt_01[col] %>% 
        unique() %>%
        sample_n(min(50, nrow(.))) 
      
      na_rows <- matrix(NA, nrow = 50 - nrow(unique_val), ncol = 1) %>% data.frame()
      colnames(na_rows)[1] <- col
      
      df_unique_val <- unique_val %>% 
        rbind(na_rows) %>% 
        t() %>% 
        rbind(df_unique_val)
      
      i = i + 1
      setTxtProgressBar(pb, i)
    }
    
    dt_01 %>%
      sample_n(min(25, nrow(dt_01))) %>% 
      rbind(t(df_unique_val)) %>% 
      write.table(file = file.path("temp.csv"), sep=",", col.names = T, row.names = F)
    
    # save relevant dataset in local drive ----
    save(dt_01, file = "dt_01.Rda")
    
    #====================================================
    
    # Log of run ----
    glue::glue("\n") %>% f_log_string(g_file_log)
    glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs. Saving the analysis environment") %>% f_log_string(g_file_log)
    glue::glue("\n\n") %>% f_log_string(g_file_log)
    
    # clean and save environment in local drive ----
    rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))
    save.image(file=file.path(g_wd, "env.RData"))
    
    # Close the R code ----
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
