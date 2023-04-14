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
    
    # load custom functions ----
    source(file.path(g_excel_backend_temp_nospace_dir, "00_functions.R"))
    
    # load libraries ----
    error = f_libraries(
      necessary.std = c("glue", "dplyr"),
      necessary.github = c()
    )
    glue::glue("RUNNING R SERVER ...") %>% print()
    glue::glue("Package status: {error}") %>% print()
    glue::glue("\n") %>% print()
    
    # Code specific inputs ----
    purpose <- "Deleting raw data and all subsequent datasets from analysis environment"
    
    code_full <- scriptName::current_filename()
    code_path <- ifelse(is.null(code_full), "", dirname(code_full)) 
    code_name <- ifelse(is.null(code_full), "", basename(code_full))
    
    # Log of run ----
    glue::glue("===================== Running '{code_name}' =====================") %>% f_log_string(g_file_log) 
    glue::glue("{purpose}")%>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)
    
    #====================================================
    
    junk <- dir(path=g_wd,  pattern="^dt_.+.Rda")
    file.remove(junk) %>% 
      invisible()
    glue::glue("Success: All datasets deleted") %>% f_log_string(g_file_log)
    
    #====================================================
    
    # Log of run ----
    glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs. Saving the analysis environment") %>% f_log_string(g_file_log)
    glue::glue("\n\n") %>% f_log_string(g_file_log)
    
    # remove unnecessary variables from environment ----
    rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))
    
    # save environment in a session temp variable ----
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