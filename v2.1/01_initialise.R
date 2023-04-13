tryCatch(
  {
    # cleanup the environment ----
    rm(list = ls())
    if (!is.null(dev.list())) dev.off()
    cat("\014")
    options(show.error.locations = TRUE)
    start_time <- Sys.time()
    
    # capture variable coming from vba ----
    args <- commandArgs(trailingOnly=T)
    args <- c("C:|Users|User|Downloads|20230406|20230406|interface history|", "C:|Users|User|AppData|Local|Temp|TEMP_R|", "C:|Users|User|Downloads|20230406|20230406|", "interface master v1.2.xlsm", "reset")
    
    # set working director ---- 
    setwd(do.call(file.path, as.list(strsplit(args[1], "\\|")[[1]])))
    
    # load environment ----
    if (args[5] == "refresh") {
      if (file.exists("env.RData")) {load("env.RData")}
    }
    
    # load custom functions ----
    source(do.call(file.path, as.list(strsplit(paste0(args[2], "00_functions.R"), "\\|")[[1]])), 
           print.eval = TRUE, echo = F)
    
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
    
    ################################################################
    
    # global variables ----
    g_excel_backend_dir                 <- do.call(file.path, as.list(strsplit(args[1], "\\|")[[1]]))
    g_excel_backend_temp_nospace_dir    <- do.call(file.path, as.list(strsplit(args[2], "\\|")[[1]]))
    g_excel_frontend_dir                <- do.call(file.path, as.list(strsplit(args[3], "\\|")[[1]]))
    g_file_name                         <- args[4]
    
    g_file_path                         <- file.path(g_excel_frontend_dir, g_file_name)
    g_wd                                <- g_excel_backend_dir
    g_file_log                          <- file.path(g_excel_frontend_dir, "Latest R logs.txt")
    
    g_tick                              <- "\u2713"
    g_cross                             <- "\u2715"
    g_pref_autoclose                    <- T
    
    unlink(g_file_log)
    
    if (args[5] == "reset") {
      all <- dir(".",  pattern=".*")
      keep <- dir(".",  pattern=".+\\.R$")
      junk <- all[! all %in% keep]
      file.remove(junk) %>% 
        invisible() %>% 
        suppressWarnings()
    } 
    
    ################################################################
    f_ending(code_name, purpose, start_time)
  }, 
  
  warning = function(warr){
    print(1)
    msg = glue::glue("{toString(warr)}\ncheck code '{code_full}'")
    tcltk::tk_messageBox(type = c("ok"), msg, caption = "WARNING!", default = "", icon = "warning")
  },
    
  error = function(erro){
    print(2)
    msg = glue::glue("{toString(erro)}\ncheck code '{code_full}'")
    tcltk::tk_messageBox(type = c("ok"), msg, caption = "ERROR!", default = "", icon = "error")
  }
  
)
