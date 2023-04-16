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
    
    # load librarise ----
    error = f_libraries(
      necessary.std = c("dplyr", "openxlsx", "rlang", "glue", "jsonlite"),
      necessary.github = c()
    )
    glue::glue("RUNNING R SERVER ...") %>% print()
    glue::glue("Package status: {error}") %>% print()
    glue::glue("\n") %>% print()
    
    # Code specific inputs ----
    purpose <- "Uploading Summariser and Weights information into the R environment"
    
    code_full <- scriptName::current_filename()
    code_path <- ifelse(is.null(code_full), "", dirname(code_full)) 
    code_name <- ifelse(is.null(code_full), "", basename(code_full))
    
    # Log of run ----
    glue::glue("===================== Running '{code_name}' =====================") %>% f_log_string(g_file_log) 
    glue::glue("{purpose}")%>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)

        # Log of run ----
    glue::glue("===================== Running '45_ipload_summ.R' =====================")
    glue::glue("")
    glue::glue("\n") %>% f_log_string(g_file_log)
    
    #====================================================
    
    glue::glue("Importing summariser info ...") %>% print()
    
    json_str <- gsub("~", '"', args[2]) 
    d_summ <- jsonlite::fromJSON(json_str) %>% 
      apply(2, function(x) gsub("^$|^ $", NA, x)) %>% 
      as.data.frame() %>% 
      dplyr::select_if(function(x){any(!is.na(x))})
    
    row.names(d_summ) <- d_summ$NAME
    d_summ["NAME"] <- NULL
    
    for (name1 in colnames(d_summ)){
      name_sym <- name1 %>% 
        rlang::sym()
      
      filled <- d_summ %>% 
        filter(!is.na(!!name_sym), !(!!name_sym %in% c("", " ", "-"))) %>% 
        nrow()
      
      if (filled == 0) {
        d_summ <- d_summ %>% select(-all_of(name1))
        glue::glue("summariser '{name1}' is blank and hence dropped") %>% f_log_string(g_file_log)
      }
      
      if (filled == 1 | filled == 2) {
        glue::glue("summariser '{name1}' is incomplete. It will not be loaded") %>% f_log_string(g_file_log)
        d_summ <- d_summ %>% select(-all_of(name1))
      }
      
      if (filled == 3) {
        glue::glue("summariser '{name1}' loaded") %>% f_log_string(g_file_log)
      }
    }
    
    #====================================================
    
    # Log of run ----
    glue::glue("\n") %>% f_log_string(g_file_log)
    glue::glue("finished run in {round(Sys.time() - start_time, 0)} secs. Saving the analysis environment") %>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)
    
    # clean and save environment in local drive ----
    rm(list = setdiff(ls(), ls(pattern = "^(d_|g_|f_)")))
    save.image(file=file.path(g_wd, "env.RData"))
    
    print(glue::glue("\n\n All done!"))
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
    