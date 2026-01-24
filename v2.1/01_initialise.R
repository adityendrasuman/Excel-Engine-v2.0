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
    if (args[5] == "refresh" & file.exists("env.RData")) {load("env.RData")}
    
    # load custom functions ----
    source(do.call(file.path, as.list(strsplit(paste0(args[2], "00_functions.R"), "\\|")[[1]])), print.eval = TRUE, echo = F)
    
    # load libraries ----
    error = f_libraries(
      necessary.std = c("dplyr", "forcats", "gdata", "glue", "ggplot2", "gridExtra", 
                        "jsonlite", "openxlsx", "purrr", "profvis", "rlang", "srvyr", 
                        "stringr", "stats", "scales", "tidyselect", "tibble", "utils", 
                        "tidyr", "caret", "janitor", "e1071", "rpart", "rpart.plot", 
                        "tcltk", "scriptName"),
      necessary.github = c()
    )
    
    glue::glue("RUNNING R SERVER ...") %>% print()
    glue::glue("Package status: {error}") %>% print()
    glue::glue("\n") %>% print()
    
    # Code specific inputs ----
    purpose <- if (args[5] == "refresh") {
      "REFRESHING: Loading the last saved environment"
    } else if (args[5] == "reset") {
      "RESETTING: Initialising a blank environment"
    }
    
    code_full <- scriptName::current_filename()
    code_path <- ifelse(is.null(code_full), "", dirname(code_full)) 
    code_name <- ifelse(is.null(code_full), "", basename(code_full))
  
    #====================================================
    
    # global variables
    g_excel_backend_dir                 <- do.call(file.path, as.list(strsplit(args[1], "\\|")[[1]]))
    g_excel_backend_temp_nospace_dir    <- do.call(file.path, as.list(strsplit(args[2], "\\|")[[1]]))
    g_excel_frontend_dir                <- do.call(file.path, as.list(strsplit(args[3], "\\|")[[1]]))
    g_file_name                         <- args[4]
    
    g_file_path                         <- file.path(g_excel_frontend_dir, g_file_name)
    g_wd                                <- g_excel_backend_dir
    g_file_log                          <- file.path(g_excel_frontend_dir, "Latest R logs.txt")
    
    # g_tick                              <- "\u2713"
    # g_cross                             <- "\u2715"
    # g_pref_autoclose                    <- F
    
    # remove log file
    unlink(g_file_log)
    # workerstr("Testing environment")
    
    # if reset then clean interface history folder
    if (args[5] == "reset") {
      all <- dir(".",  pattern=".*")
      keep <- dir(".",  pattern=".+\\.R$")
      junk <- all[! all %in% keep]
      file.remove(junk) %>% 
        invisible() %>% 
        suppressWarnings()
    }
    
    #====================================================
    
    # Log of run ----
    glue::glue("===================== Running '{code_name}' =====================") %>% f_log_string(g_file_log) 
    glue::glue("{purpose}")%>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)
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


