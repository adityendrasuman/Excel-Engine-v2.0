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
    load("dt_01_C.Rda")
    
    # load librarise ----
    error = f_libraries(
      necessary.std = c("dplyr", "purrr", "stringr"),
      necessary.github = c()
    )
    glue::glue("RUNNING R SERVER ...") %>% print()
    glue::glue("Package status: {error}") %>% print()
    glue::glue("\n") %>% print()
    
    # Code specific inputs ----
    purpose <- "ID-ing potentially incomplete responses by looking at responses longer than user-provided length"
    
    code_full <- scriptName::current_filename()
    code_path <- ifelse(is.null(code_full), "", dirname(code_full)) 
    code_name <- ifelse(is.null(code_full), "", basename(code_full))
    
    # Log of run ----
    glue::glue("===================== Running '{code_name}' =====================") %>% f_log_string(g_file_log) 
    glue::glue("{purpose}")%>% f_log_string(g_file_log)
    glue::glue("\n") %>% f_log_string(g_file_log)

    #====================================================
    
    threshold = as.numeric(args[2])
    
    glue::glue("Searching for strings with {threshold} or more characters...") %>% print()
    
    list_of_variables <- dt_01_C %>%
      colnames()
    
    summary <- purrr::map_dfr(list_of_variables, function(var) {
      
      temp <- dt_01_C %>%
        pull(!!var) %>%
        nchar() %>%
        max(na.rm = TRUE) %>% 
        suppressWarnings()
      
      if (temp >= threshold) {
        dt_01_C %>%
          select(response = !!var) %>%
          count(response) %>%
          mutate(no_of_char = nchar(response),
                 variable = var) %>%
          filter(no_of_char >= threshold) %>%
          select(-n) %>%
          return()
      }
    }) 
    
    if (nrow(summary) == 0){
      summary <- data.frame()
      
    } else {
      summary <- summary %>%
        select(variable, everything()) %>% 
        arrange(-no_of_char) %>%
        select(variable, response) %>%
        mutate(replacement = "~") %>% 
        filter(!stringr::str_detect(variable, "(_OTH|_OE)$"))
    }
    
    summary %>% 
      write.table(file = file.path("temp.csv"), sep=",", col.names = F, row.names = F)
    
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