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
    load("dt_01_E.Rda")
    
    # load librarise ----
    error = f_libraries(
      necessary.std = c("dplyr", "glue"),
      necessary.github = c()
    )
    glue::glue("RUNNING R SERVER ...") %>% print()
    glue::glue("Package status: {error}") %>% print()
    glue::glue("\n") %>% print()
    
    # Code specific inputs ----
    purpose <- "Creating new columns and exporting a sample into the interface"
    
    code_full <- scriptName::current_filename()
    code_path <- ifelse(is.null(code_full), "", dirname(code_full)) 
    code_name <- ifelse(is.null(code_full), "", basename(code_full))
    
    # Log of run ----
    glue::glue("===================== Running '{code_name}' =====================") %>% f_log_string(g_file_log) 
    glue::glue("{purpose}")%>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)

    #====================================================
    file_name <- file.path(g_excel_backend_dir, "create_new_columns_in_r.R")
    source(file_name, print.eval = TRUE, echo = F)
    
    d <- create_new_col(dt_01_E)
    dt_02 <- d[[1]]
    d_skip_newcol <- d[[2]]
    
    # Output unique set of values for each column, upto 50
    df_unique_val <- data.frame(matrix(ncol=50, nrow=0))
    for (i in 1:50){
      colnames(df_unique_val)[i] <- paste0("V", i)
    }
    
    glue::glue("Creating table of unique values in the data") %>% f_log_string(g_file_log)
    pb <- txtProgressBar(min = 0, max = ncol(dt_02), style = 3, width = 40)
    i = 0
    for (col in colnames(dt_02)){
      
      unique_val <- dt_02[col] %>% 
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
    
    dt_02 %>%
      sample_n(min(25, nrow(dt_02))) %>%
      rbind(t(df_unique_val)) %>%
      write.table(file = file.path("temp.csv"), sep=",", col.names = T, row.names = F)
    
    dt_02 %>%
      colnames() %>%
      write.table(file = file.path("temp_2.csv"), sep=",", col.names = F, row.names = F)
    
    # Save relevant dataset in local drive ----
    save(dt_02, file = "dt_02.Rda")
    
    #====================================================
    
    # Log of run ----
    glue::glue("\n") %>% f_log_string(g_file_log)
    glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs. Saving the analysis environment") %>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)
    
    # clean and save environment in local drive ----
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
    
