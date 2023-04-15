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
    load("dt_01.Rda")
    
    # load libraries ----
    error = f_libraries(
      necessary.std = c("dplyr", "stringr", "profvis", "glue"),
      necessary.github = c()
    )
    glue::glue("RUNNING R SERVER ...") %>% print()
    glue::glue("Package status: {error}") %>% print()
    glue::glue("\n") %>% print()
    
    # Code specific inputs ----
    purpose <- "Trimming whitespaces around each data value"
    
    code_full <- scriptName::current_filename()
    code_path <- ifelse(is.null(code_full), "", dirname(code_full)) 
    code_name <- ifelse(is.null(code_full), "", basename(code_full))
    
    # Log of run ----
    glue::glue("===================== Running '{code_name}' =====================") %>% f_log_string(g_file_log) 
    glue::glue("{purpose}")%>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)
    
    #====================================================
    
    dt_01_A <- dt_01
    
    cols_to_be_rectified <- names(dt_01_A)[vapply(dt_01_A, is.character, logical(1))]
    dt_01_A[,cols_to_be_rectified] <- lapply(dt_01_A[,cols_to_be_rectified], trimws)
    dt_01_A[,cols_to_be_rectified] <- lapply(dt_01_A[,cols_to_be_rectified], stringr::str_trim)
    
    whitespaces <- paste(c("^\\s+.+\\s+$", ".+\\s+$", "^\\s+.+$"), collapse = "|")
    summary <- f_id_char(dt_01_A, whitespaces)
    
    if(is.null(nrow(summary))) {
      glue::glue("All leading or lagging white spaces have been removed") %>% f_log_string(g_file_log)
    } else if(nrow(summary) > 0) {
      glue::glue("All white spaces could not be removed") %>% f_log_string(g_file_log)
      glue::glue("Please check log file for the values that could not be removed") %>% f_log_string(g_file_log)
      summary %>% f_log_table("List of White spaces that could not be removed", g_file_log)
    }
    
    # Save relevant datasets ----
    save(dt_01_A, file = "dt_01_A.Rda")
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
