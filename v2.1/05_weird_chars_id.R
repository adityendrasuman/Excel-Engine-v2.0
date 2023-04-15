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
    
    # load librarise ----
    error = f_libraries(
      necessary.std = c("dplyr", "stringr", "openxlsx", "glue"),
      necessary.github = c()
    )
    glue::glue("RUNNING R SERVER ...") %>% print()
    glue::glue("Package status: {error}") %>% print()
    glue::glue("\n") %>% print()
    
    # Log of run ----
    glue::glue("===================== Running '05_weird_chars_id.R' =====================") %>% f_log_string(g_file_log)
    glue::glue("This code identifies unrecognised characters in the data based on user suggestions in the excel interface") %>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)
    
    #====================================================
    
    print(glue::glue("Picking suggestions for weird characters from the excel interface..."))
    supplied_weird_chr <- f_read_xl(g_file_path, namedRegion = "wc1_R", colNames = F) %>% 
      select(1) %>% 
      filter_all(any_vars(!is.na(.)))
    
    weird_chr <- paste(c("[^\x01-\x7F]", supplied_weird_chr[[1]]), collapse = "|")
    
    print(glue::glue("Searching for weird characters..."))
    summary <- f_id_char(dt_01, weird_chr)
    
    summary %>% 
      write.table(file = file.path("temp.csv"), sep=",", col.names = F, row.names = F)
    
    #====================================================
    
    # Log of run ----
    glue::glue("\n") %>% f_log_string(g_file_log)
    glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs") %>% f_log_string(g_file_log)
    glue::glue("\n\n") %>% f_log_string(g_file_log)
    
    # remove unnecessary variables from environment ----
    rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))
    
    # save environment in a session temp variable ----
    save.image(file=file.path(g_wd, "env.RData"))
    
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