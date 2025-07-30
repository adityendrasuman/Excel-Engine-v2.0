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
    load("dt_01_A.Rda")
    
    # load librarise ----
    error = f_libraries(
      necessary.std = c("dplyr", "stringr", "openxlsx", "glue"),
      necessary.github = c()
    )
    glue::glue("RUNNING R SERVER ...") %>% print()
    glue::glue("Package status: {error}") %>% print()
    glue::glue("\n") %>% print()
    
    # Code specific inputs ----
    purpose <- "Replacing unrecognised characters in data, based on user suggestions"
    
    code_full <- scriptName::current_filename()
    code_path <- ifelse(is.null(code_full), "", dirname(code_full)) 
    code_name <- ifelse(is.null(code_full), "", basename(code_full))
    
    # Log of run ----
    glue::glue("===================== Running '{code_name}' =====================") %>% f_log_string(g_file_log) 
    glue::glue("{purpose}")%>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)

    #====================================================
    
    print(glue::glue("Picking mapping for weird characters from the excel interface..."))
    
    df_temp <- f_read_xl(g_file_path, namedRegion = "wc3_R", colNames = F)
    
    if (is.null(df_temp) || nrow(df_temp) == 0){
      map <- data.frame(X1 = character(0), X2 = character(0), stringsAsFactors = FALSE)
    } else {
      map <- df_temp %>%
        unique() %>%
        filter_all(any_vars(!is.na(.))) %>%
        filter(!is.na(X1))
    }
    
    dt_01_B <- dt_01_A
    
    i = 0
    pb <- txtProgressBar(min = 1, max = ncol(dt_01_B), style = 3, width = 40)
    print(glue::glue("Replacing weird characters..."))
    
    for (var in colnames(dt_01_B)){
      for (name in map[,"X1"]){
        
        value <- map[map$X1 == name, "X2"]
        
        dt_01_B[, var] <- gsub(name, value, dt_01_B[, var])  
      }
      i = i + 1
      setTxtProgressBar(pb, i)
    }
    close(pb)
    
    print(glue::glue("Double checking..."))
    supplied_weird_chr <- f_read_xl(g_file_path, namedRegion = "wc1_R", colNames = F)
    weird_chr <- paste(c("[^\x01-\x7F]", supplied_weird_chr[[1]]), collapse = "|")
    summary <- f_id_char(dt_01_B, weird_chr)
    
    if(is.null(nrow(summary))) {
      glue::glue("Any occurance of weird characters has been replaced") %>% f_log_string(g_file_log)
    } else if(nrow(summary) > 0) {
      glue::glue("All occurances of weird characters could not be removed.") %>% f_log_string(g_file_log)
      glue::glue("Please check log file for the values that could not be removed") %>% f_log_string(g_file_log)
      glue::glue("Please remove manually in the raw data and upload it again") %>% f_log_string(g_file_log)
    }
    
    # IN CASE LIVE CAPTURE IS SKIPPED
    dt_01_C <- dt_01_B 
    
    # output erronous datapoints 
    if (!is.null(nrow(summary))){
      summary %>% f_log_table("List of Unrecognised Characters that could not be removed", g_file_log)
    }
    
    # save relevant dataset ----
    save(dt_01_B, file = "dt_01_B.Rda")
    save(dt_01_C, file = "dt_01_C.Rda")
    
    #====================================================
    
    # Log of run ----
    glue::glue("\n") %>% f_log_string(g_file_log)
    glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs. Saving the analysis environment") %>% f_log_string(g_file_log)
    glue::glue("\n\n") %>% f_log_string(g_file_log)
    
    # clean and save environment in a session temp variable ----
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
